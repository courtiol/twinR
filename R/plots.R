#' Theme for plots
#'
#' This is the custom ggplot theme that we use for all the figures produced in twinR.
#' The theme is built around [`theme_bw`][`ggplot2::theme_bw`] and is inspired by
#' [`theme_few`][`ggthemes::theme_few`].
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#'
#' @return the theme
#' @export
#'
theme_twin <- function(base_size = 11, base_family = "",
                       base_line_size = base_size/22, base_rect_size = base_size/22) {
  gray <- "#4D4D4D"
  black <- "#000000"
  ggplot2::theme_bw(base_size = base_size, base_family = base_family,
                    base_line_size = base_line_size, base_rect_size = base_rect_size) +
    ggplot2::theme(
      line = ggplot2::element_line(colour = gray),
      rect = ggplot2::element_rect(fill = "white", colour = NA),
      text = ggplot2::element_text(colour = black),
      axis.ticks = ggplot2::element_line(colour = gray),
      legend.key = ggplot2::element_rect(colour = NA),
      panel.border = ggplot2::element_rect(colour = gray),
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white", colour = NA),
      title = ggplot2::element_text(colour = black, size = 7, family = base_family),
      axis.text.x = ggplot2::element_text(colour = black, size = 5, family = base_family),
      axis.text.y = ggplot2::element_text(colour = black, size = 5, family = base_family),
      axis.title.x = ggplot2::element_text(colour = black, size = 7, family = base_family, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)),
      axis.title.y = ggplot2::element_text(colour = black, size = 7,  family = base_family, margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 10)),
      plot.margin = ggplot2::margin(3, 1, 3, 1)
    )
}


#' Import plot_grid from the package cowplot
#'
#' See [`plot_grid`][`cowplot::plot_grid`] for details.
#'
#' @name plot_grid
#' @export
#' @importFrom cowplot plot_grid
NULL


#' Import ggsave from the package ggplot2
#'
#' See [`ggsave`][`ggplot2::ggsave`] for details.
#'
#' @name ggsave
#' @export
#' @importFrom ggplot2 ggsave
NULL



#' Functions to prepare data for the figures
#'
#' These functions are used to prepare the data required to draw the figures.
#' They allow for the computation of predictions averaged over the non focal fixed predictors.
#'
#' We recommend you to look at the raw R code of these functions on GitHub (file
#' '/R/plots.R') to understand how they work. We commented the code to make this clear.
#' While you could directly look at the code of these functions while using the package, mind that
#' the comments will have been stripped away during the installation process.
#'
#' @name prepare_data_for_fig
#' @inheritParams fit_models
#' @inheritParams predictions
#' @param xaxis a `character` indicating whether `"age"` or `"parity"` must be considered as the x-axis
#' @param fit_PP a fitted model for predicting the parity progression
#' @param fit_IBI a fitted model for predicting the interbirth interval
#' @param fit_twin a fitted model for predicting the per-birth probability of twinning
#' @param fit_AFB a fitted model for predicting the age at first birth
#' @param simulation_obj an object produced by [`run_simulation`]
#' @examples
#' prepare_newdata_fig_3(expand_data(data_births_all), xaxis = "age")
#' prepare_newdata_fig_3(expand_data(data_births_all), xaxis = "parity")
#'
#' # for more realistic use, see ?twinR
#'
NULL


#' @describeIn prepare_data_for_fig an internal function to prepare the data used to generate the predictions to be plotted in Fig. 3
#' @export
#'
prepare_newdata_fig_3 <- function(birth_level_data, xaxis = c("age", "parity")) {

  new_data <- birth_level_data[, c("age", "parity")]

  if (length(xaxis) > 1) {
    xaxis <- xaxis[1]
    warning("the argument 'xaxis' must be of length 1")
  }

  ## when the x-axis is the maternal age, then we want a regular sampling of 100 maternal ages
  ## within the 95% range of the actual distribution for age among mothers of each given parity:
  if (xaxis == "age") {

    new_data %>%
      dplyr::filter(.data$parity %in% c(1, 2, 3, 5, 7, 9)) %>%
      dplyr::group_by(parity_fct = as.factor(.data$parity)) %>%
      dplyr::summarise(parity = unique(.data$parity)[1],
                       age_min = stats::quantile(.data$age, probs = 0.025, na.rm = TRUE),
                       age_max = stats::quantile(.data$age, probs = 0.975, na.rm = TRUE)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(age = list(seq(.data$age_min, .data$age_max, length.out = 100))) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$parity, .data$age) %>%
      tidyr::unnest(cols = .data$age) %>% # age is a nested column, so we must unnest it
      dplyr::mutate(pop = "new",
                    maternal_ID = "new",
                    twin = TRUE) -> new_data

  ## when the x-axis is the parity, then we want all ages associated with each parity, so that
  ## we can compute the integration over the observed distribution of ages:
  } else if (xaxis == "parity") {
    new_data %>%
      dplyr::count(.data$parity, .data$age) %>%
      dplyr::mutate(pop = "new",
                    maternal_ID = "new",
                    twin = TRUE) -> new_data
  } else {
    stop("the argument 'xaxis' must be 'age' or 'parity'")
  }

  new_data %>%
    dplyr::bind_rows(new_data %>%
                       dplyr::mutate(pop = "new",
                                     maternal_ID = "new",
                                     twin = FALSE))
}


#' @describeIn prepare_data_for_fig prepare the data to be plotted in Fig. 4A
#' @export
#'
prepare_data_fig_4A <- function(fit_PP) {

  birth_level_data <- fit_PP$data

  ## compute the newdata for predictions:
  new_data <- prepare_newdata_fig_3(birth_level_data, xaxis = "parity")

  ## compute the predictions for all existing parity - ages combinations:
  pred_PP <- compute_predictions(fit_PP, new_data, nb_boot = 0)[[1]]

  ## average the predictions across the different ages corresponding to each parity and twinning status:
  pred_PP %>%
    dplyr::group_by(.data$parity, .data$twin) %>%
    dplyr::summarise(PP = sum(.data$n*.data$estimates)/sum(.data$n)) %>%
    dplyr::ungroup() -> pred_PP_avg

  ## compute the mean maternal age at each parity:
  birth_level_data %>%
    dplyr::group_by(parity_fct = as.factor(.data$parity)) %>%
    dplyr::summarise(parity = unique(.data$parity)[1],
                     age = mean(.data$age)) %>%
    dplyr::select(-.data$parity_fct) -> mean_age_per_parity

  ## join the datasets:
  dplyr::full_join(pred_PP_avg, mean_age_per_parity, by = "parity")
}


#' @describeIn prepare_data_for_fig prepare the data to be plotted in Fig. 4B
#' @export
#'
prepare_data_fig_4B <- function(fit_IBI) {

  birth_level_data <- fit_IBI$data

  ## compute the newdata for predictions:
  new_data <- prepare_newdata_fig_3(birth_level_data, xaxis = "age")

  ## compute the predictions:
  pred_IBI <- compute_predictions(fit_IBI, new_data, nb_boot = 0)

  ## keep only the outputs needed and add the 6 months that were removed
  ## during the fit of the interbirth interval:
  pred_IBI[[1]] %>%
    dplyr::mutate(IBI = .data$estimates + 6) %>%
    dplyr::select(.data$parity, .data$age, .data$twin, .data$IBI) %>%
    dplyr::relocate(.data$age, .after = .data$IBI) %>%
    tibble::as_tibble()
}


#' @describeIn prepare_data_for_fig prepare the data to be plotted in Fig. 4C
#' @export
#'
prepare_data_fig_4C <- function(fit_twin) {

  birth_level_data <- fit_twin$data

  ## compute the newdata for predictions:
  new_data <- prepare_newdata_fig_3(birth_level_data, xaxis = "age")

  ## remove the twin covariate, which is not used in twin_fit:
  new_data %>%
    dplyr::filter(.data$twin) %>%
    dplyr::select(-.data$twin) -> new_data

  ## compute the predictions:
  pred_twin <- compute_predictions(fit_twin, new_data, nb_boot = 0)

  ## keep only the outputs needed:
  pred_twin[[1]] %>%
    dplyr::mutate(twin = .data$estimates) %>%
    dplyr::select(.data$parity, .data$age, .data$twin) %>%
    dplyr::relocate(.data$age, .after = .data$twin) %>%
    tibble::as_tibble()
}


#' @describeIn prepare_data_for_fig an internal function to prepare the data used to generate the predictions to be plotted in Fig. S1
#' @export
#'
prepare_newdata_fig_S1 <- function() {
  expand.grid(twinner = c(TRUE, FALSE),
              births_total_fac = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10+"))
}


#' @describeIn prepare_data_for_fig prepare the data to be plotted in Fig. S1
#' @export
#'
prepare_data_fig_S1 <- function(fit_AFB, nb_boot = 1000) {

  ### part 1: predictions for AFB according to total births and twinning status

  ## compute the newdata for predictions:
  new_data <- prepare_newdata_fig_S1()

  ## compute the predictions:
  pred_AFB <- compute_predictions(fit_AFB, newdata = new_data, nb_boot = nb_boot)

  ## keep only the outputs needed:
  pred_AFB$results %>%
    dplyr::rename(AFB = .data$estimate) %>%
    tibble::as_tibble() -> data_part1


  ### part 2: sample size for each total birth and twinning status

  mother_level_data <- fit_AFB$data

  mother_level_data %>%
    dplyr::count(.data$twinner, .data$births_total_fac) %>%
    tibble::as_tibble() -> data_part2

  list(data_part1 = data_part1, data_part2 = data_part2)
}


#' @describeIn prepare_data_for_fig prepare the data to be plotted in Fig. S2
#' @export
#'
prepare_data_fig_S2 <- function(fit_PP, fit_IBI, fit_twin, mother_level_data) {

  ## extract random effects from each model fit:
  ranef_PP   <- tibble::as_tibble_row(spaMM::ranef(fit_PP)[[1]]) %>% tidyr::pivot_longer(tidyselect::everything(), values_to = "ranef_PP")
  ranef_IBI  <- tibble::as_tibble_row(spaMM::ranef(fit_IBI)[[1]]) %>% tidyr::pivot_longer(tidyselect::everything(), values_to = "ranef_IBI")
  ranef_twin <- tibble::as_tibble_row(spaMM::ranef(fit_twin)[[1]]) %>% tidyr::pivot_longer(tidyselect::everything(), values_to = "ranef_twin")

  ## combine random effects in one tibble:
  ranefs_all <- dplyr::full_join(dplyr::full_join(ranef_PP, ranef_IBI, by = "name"), ranef_twin, by = "name")

  ## add number of twin births to the data:
  dplyr::left_join(mother_level_data %>% dplyr::select(.data$maternal_id, .data$twin_total), ranefs_all, by = c(maternal_id = "name"))
}


#' @describeIn prepare_data_for_fig prepare the data to be plotted in Fig. S6
#' @export
#'
prepare_data_fig_S6 <- function(simulation_obj, birth_level_data) {

  expand_data(simulation_obj$birth_level_data.simulated) %>%
    dplyr::select(.data$pop, .data$maternal_id, .data$parity, .data$twin, .data$IBI, .data$age)  %>%
    dplyr::mutate(scenario = paste0("data simulated (", simulation_obj$scenario, ")")) -> simulated

  birth_level_data %>%
    dplyr::select(.data$pop, .data$maternal_id, .data$parity, .data$twin, .data$IBI, .data$age) %>%
    dplyr::mutate(scenario = "data observed") -> observed

  dplyr::bind_rows(simulated, observed) %>%
    dplyr::mutate(age_round = round(.data$age, 0L))

}


#' Functions producing the figures
#'
#' @param data a `data.frame` or a `list` containing the data to be plotted
#' @param width_petal a numeric used to influence the width of each petal in the flower plot (default = 0.5)
#' @return a ggplot
#' @name figures
#' @examples
#' # See ?twinR
#'
NULL


#' @describeIn figures draw fig. 1A
#' @export
#'
draw_fig_1A <- function(data) {

  ## extract results component from the output of compute_predictions() if not done by user
  if (is.list(data) && !is.data.frame(data)) {
    data <- data$results
  }

  #data_mothers_monthly %>% count(births_total, twinner) -> data_points

  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = factor(.data$twinner),
                 y = .data$estimate) +
    #ggplot2::geom_point(ggplot2::aes(x = twinner, y = births_total, size = log(n)), data = data_points, shape = 1) +
    ggplot2::geom_point(color = "black", size = 0.5) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lwr, ymax = .data$upr), size = 0.5, width = 0.1, linetype = 1) +
    ggplot2::labs(y = "Maternal total births", x = "Lifetime twin. status") +
    ggplot2::scale_x_discrete(labels = c("Non-twinner", "Twinner")) +
    ggplot2::scale_y_continuous(limits = c(0, 8), breaks = c(0, 2, 4, 6, 8)) +
    theme_twin()
}


#' @describeIn figures draw fig. 1B
#' @export
#'
draw_fig_1B <- function(data) {

  ## extract results component from the output of compute_predictions() if not done by user
  if (is.list(data) && !is.data.frame(data)) {
    data <- data$results
  }

  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$births_total,
                 y = .data$estimate,
                 ymin = .data$lwr,
                 ymax = .data$upr,
                 fill = .data$estimate) +
    ggplot2::labs(y = "Lifetime twin. prob.",
                  x = "Maternal total births" ) +
    ggplot2::geom_line(colour = "black", size = 1) +
    ggplot2::geom_ribbon(alpha = 0.3, fill = "grey") +
    ggplot2::scale_x_continuous(breaks = c(1, 5, 10, 15, 18),
                                labels = c("1", "5", "10", "15", "18")) +
    ggplot2::scale_y_continuous(trans = "logit",
                            breaks = c(0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
                            labels = c("0.03", "0.05", "0.1", "0.2", "0.3", "0.4", "0.5")) +
    ggplot2::expand_limits(y = c(0, 0.5)) +
    theme_twin()
}


#' @describeIn figures draw fig. 1C
#' @export
#'
draw_fig_1C <- function(data) {

  ## extract results component from the output of compute_predictions() if not done by user
  if (is.list(data) && !is.data.frame(data)) {
    data <- data$results
  }

  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = factor(.data$first_twinner),
                 y = .data$estimate,
                 ymin = .data$lwr,
                 ymax = .data$upr) +
    ggplot2::geom_col(color = "black", fill = c("lightgray", "white"), width = .6) +
    ggplot2::geom_errorbar(size = 0.5, width = 0.1, linetype = 1) +
    ggplot2::labs(y = "Maternal total births", x = "First-birth twin. status") +
    ggplot2::scale_x_discrete(labels = c("Singleton", "Twins")) +
    ggplot2::scale_y_continuous(limits = c(0, 6), breaks = c(0, 2, 4, 6)) +
    theme_twin()
}


#' @describeIn figures draw fig. 1D
#' @export
#'
draw_fig_1D <- function(data) {

  ## extract results component from the output of compute_predictions() if not done by user
  if (is.list(data) && !is.data.frame(data)) {
    data <- data$results
  }

  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$births_total,
                 y = .data$estimate,
                 ymin = .data$lwr,
                 ymax = .data$upr,
                 fill = .data$estimate) +
    ggplot2::labs(y = "First-birth twin. prob.",
                  x = "Maternal total births" ) +
    ggplot2::geom_line(colour = "black", size = 1) +
    ggplot2::geom_ribbon(alpha = 0.3, fill = "grey") +
    ggplot2::scale_x_continuous(breaks = c(1, 5, 10, 15, 18),
                                labels = c("1", "5", "10", "15", "18")) +
    ggplot2::expand_limits(y = c(0, 0.025)) +
    theme_twin()
}


#' @describeIn figures draw fig. 2
#' @export
#'
draw_fig_2 <- function(data) {

  ## extract results component from the output of compute_predictions() if not done by user
  if (is.list(data) && !is.data.frame(data)) {
    data <- data$results
  }

  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$births_total,
                 y = .data$estimate,
                 ymin = .data$lwr,
                 ymax = .data$upr,
                 fill = .data$estimate) +
    ggplot2::labs(y = "Per-birth twin. prob.",
                  x = "Maternal total births" ) +
    ggplot2::geom_line(colour = "black", size = 1) +
    ggplot2::geom_ribbon(alpha = 0.3, fill = "grey") +
    ggplot2::scale_x_continuous(breaks = 1:18) +
    ggplot2::scale_y_continuous(trans = "logit",
                                breaks = seq(0.005, 0.025, by = 0.001)) +
    theme_twin() +
    ggplot2::coord_cartesian()
}



#' @describeIn figures draw fig. 4A
#' @export
#'
draw_fig_4A <- function(data) {

  data %>%
    dplyr::mutate(
      twin = ifelse(.data$twin, "twin", "singleton"),
      twin = factor(.data$twin, levels = c("singleton", "twin"))) -> data_plot

  data_plot %>%
    dplyr::filter(.data$twin == "twin") -> data_plot_twin

  data_plot %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$parity) +
    #ggplot2::geom_line(data = data_plot_twin,
    #                   mapping = ggplot2::aes(y = (.data$age - 25) / 20,
    #                                          x = .data$parity), colour = "grey") +
    ggplot2::geom_point(data = data_plot_twin,
                        mapping = ggplot2::aes(y = (.data$age - 25) / 20,
                                               x = .data$parity),
                        colour = "grey") +
    ggplot2::scale_y_continuous("Parity progression prob.",
                                sec.axis = ggplot2::sec_axis(~ (.* 20) + 25,
                                                             name = "Mean maternal age",
                                                             breaks = seq(25, 45, by = 5)),
                                breaks = seq(0, 1, by = 0.1)) +
    #ggplot2::geom_line(ggplot2::aes(y = .data$PP,
    #                                linetype = .data$twin)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$PP,
                                     shape = .data$twin)) +
    ggplot2::expand_limits(y = c(0, 1)) +
    ggplot2::scale_x_continuous("Parity", breaks = c(1, 5, 10, 15, 18)) +
    ggplot2::scale_shape_discrete("Twinning status", guide = ggplot2::guide_legend(title.vjust = 0.5, nrow = 1), solid = FALSE) +
    ggplot2::scale_linetype_discrete("Twinning status", guide = ggplot2::guide_legend(title.vjust = 0.5, nrow = 1)) +
    ggplot2::labs(linetype = "Twinning status",
                  shape = "Twinning status") +
    theme_twin() +
    ggplot2::theme(legend.position = "bottom",
                   legend.key.width = ggplot2::unit(0.5, units = "cm"),
                   legend.text = ggplot2::element_text(size = 6),
                   legend.title = ggplot2::element_text(size = 7),
                   legend.margin = ggplot2::margin(c(0, 0, 0, 0)))
}


#' @describeIn figures draw fig. 4B
#' @export
#'
draw_fig_4B <- function(data) {

  data %>%
    dplyr::mutate(
      twin = ifelse(.data$twin, "twin", "singleton"),
      twin = factor(.data$twin, levels = c("singleton", "twin"))) -> data_plot

  data_plot %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$age,
                 y = .data$IBI,
                 col = as.factor(.data$parity),
                 shape = .data$twin,
                 linetype = .data$twin) +
    ggplot2::geom_line() +
    ggplot2::scale_color_viridis_d("Parity", guide = NULL) +
    ggplot2::scale_linetype_manual("Twinning status", values = c("dashed", "solid"), guide = ggplot2::guide_legend(title.vjust = 0.5, nrow = 1)) +
    ggplot2::scale_x_continuous("Maternal age", breaks = seq(20, 45, by = 5)) +
    theme_twin() +
    ggplot2::labs(y = "Interbirth interval (months)") +
    ggplot2::expand_limits(y = c(25, 50)) +
    ggplot2::expand_limits(x = c(20, 45)) +
    ggplot2::theme(legend.position = "bottom",
                   legend.key.width = ggplot2::unit(0.5, units = "cm"),
                   legend.text = ggplot2::element_text(size = 6),
                   legend.title = ggplot2::element_text(size = 7),
                   legend.margin = ggplot2::margin(c(0, 0, 0, 0)))
}


#' @describeIn figures draw fig. 4C
#' @export
#'
draw_fig_4C <- function(data) {

  data %>%
    dplyr::mutate(
      twin = ifelse(.data$twin, "twin", "singleton"),
      twin = factor(.data$twin, levels = c("singleton", "twin"))) -> data_plot

  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$age,
                 y = .data$twin,
                 col = as.factor(.data$parity)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_viridis_d("Parity", guide = ggplot2::guide_legend(title.vjust = 0.5, nrow = 1)) +
    ggplot2::scale_y_continuous("Per-birth twin. prob.", breaks = seq(0.01, 0.04, by = 0.005)) +
    ggplot2::scale_x_continuous("Maternal age", breaks = seq(20, 45, by = 5)) +
    ggplot2::expand_limits(y = c(0.01, 0.04)) +
    ggplot2::expand_limits(x = c(20, 45)) +
    theme_twin() +
    ggplot2::theme(legend.position = "bottom",
                   legend.key.width = ggplot2::unit(0.5, units = "cm"),
                   legend.text = ggplot2::element_text(size = 6),
                   legend.title = ggplot2::element_text(size = 7),
                   legend.margin = ggplot2::margin(c(0, 0, 0, 0)))
}


#' @describeIn figures draw fig. 5
#' @export
#'
draw_fig_5 <- function(data, width_petal = 0.5) {

  ## extract gof info:
  gof_data <- goodness_of_fit(data)

  ## merge datasets and drop slopes level 2:
  dplyr::full_join(data, gof_data, by = "scenario") %>%
    dplyr::group_by(.data$scenario, .data$seed) %>%
    dplyr::slice(1L) %>%
    dplyr::select(-.data$slopes_level2) -> data_plot

  ## reformat data to name and order scenarios properly:
  data_plot %>%
    dplyr::mutate(scenario = ifelse(.data$scenario  == "base_model", "0", .data$scenario),
                  scenario = factor(.data$scenario,
                                    levels = c("PISH", "PIS", "PIH", "PSH", "PI", "PS", "PH",
                                               "P", "ISH", "IS", "IH", "I", "SH", "S", "H",
                                               "0"))) -> data_plot

  ## flower plot (an idea from us!):
  data_plot %>%
    ggplot2::ggplot() +
      ggplot2::aes(x = .data$scenario, y = .data$slopes_level1, fill = .data$pv_gof) +
      ggplot2::coord_polar(start = pi/length(unique(data_plot$scenario))) +
      ggplot2::geom_violin(size = 0.1, width = width_petal) + ## change width to change the width of the petals
      ggplot2::geom_hline(yintercept = 0, colour = "black", size = 0.2) +
      ggplot2::geom_hline(yintercept = unique(data_plot$slope_observed),
                          colour = "darkgreen", linetype = "dashed", size = 0.2) +
      ggplot2::scale_fill_gradient2(low = "#0018ffff", high = "yellow",
                                    midpoint = log(0.05, 10), limits  = c(min(c(0.005, data_plot$pv_gof)), 1), trans = "log10",
                                    breaks = c(0.005, 0.05, 1),
                                    guide = ggplot2::guide_colorbar(barwidth = ggplot2::unit(3.5, "cm"),
                                                                    ticks.colour = "black",
                                                                    title.vjust = 0.8)) +
      ggplot2::labs(fill = "p-value") +
      theme_twin() +
      ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(size = 6),
                     legend.position = "bottom",
                     legend.text = ggplot2::element_text(size = 6),
                     legend.title = ggplot2::element_text(size = 7),
                     legend.title.align = 0.5,
                     legend.margin = ggplot2::margin(c(0, 0, 0, 0)),
                     legend.box.spacing	= ggplot2::unit(0, units = "cm"))

}


#' @describeIn figures draw fig. S1
#' @export
#'
draw_fig_S1 <- function(data) {

  data$data_part1 %>%
    dplyr::mutate(twinner = factor(ifelse(.data$twinner, "twinner", "non-twinner"),
                                   levels = c("non-twinner", "twinner"))) -> data_plot1

  data$data_part2 %>%
    dplyr::mutate(twinner = factor(ifelse(.data$twinner, "twinner", "non-twinner"),
                                   levels = c("non-twinner", "twinner"))) -> data_plot2

  data_plot1 %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = factor(.data$births_total_fac)) +
    ggplot2::geom_line(data = data_plot2,
                       mapping = ggplot2::aes(y = log(.data$n)*2.5 + 12,
                                              group = .data$twinner,
                                              linetype = .data$twinner), colour = "grey") +
    ggplot2::geom_point(data = data_plot2,
                        mapping = ggplot2::aes(y = log(.data$n)*2.5 + 12,
                                               shape = .data$twinner), colour = "grey") +
    ggplot2::scale_shape_discrete("Lifetime twin. status", solid = TRUE) +
    ggplot2::scale_linetype_manual(values = c("dotted", "solid")) +
    ggnewscale::new_scale("shape") +
    ggplot2::geom_errorbar(ggplot2::aes(
      group = .data$twinner,
      y = .data$AFB/12,
      ymin = .data$lwr/12,
      ymax = .data$upr/12),
      size = 0.2, width = 0.3, linetype = 1,
      position = ggplot2::position_dodge(width = 0.2)) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$twinner,
                                     y = .data$AFB/12),
                        size = 2, position = ggplot2::position_dodge(width = 0.2)) +
    ggplot2::scale_y_continuous("Maternal age at first birth",
                                breaks = c(21:35),
                                sec.axis = ggplot2::sec_axis(~ (exp((. - 12)/2.5)),
                                                             name = "Number of observations",
                                                             breaks = c(50, 100, 200, 400, 800, 1600, 3200))) +
    ggplot2::scale_shape_discrete("Lifetime twin. status", solid = FALSE) +
    ggplot2::labs(x = "Maternal total births", linetype = "Lifetime twin. status") +
    theme_twin() +
    ggplot2::theme(legend.position = "right",
                   legend.key.width = ggplot2::unit(0.5, units = "cm"),
                   legend.text = ggplot2::element_text(size = 6),
                   legend.title = ggplot2::element_text(size = 8),
                   legend.margin = ggplot2::margin(c(0, 0, 0, 0)))
}


#' @describeIn figures draw fig. S2
#' @export
#'
draw_fig_S2 <- function(data) {

  data %>%
    dplyr::filter(.data$twin_total == 0) %>%
    tidyr::drop_na(.data$ranef_twin, .data$ranef_PP) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .data$ranef_twin, y = .data$ranef_PP), size = 0.05) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::scale_y_continuous(limits = c(-0.5, 1.55)) +
    ggplot2::scale_x_continuous(breaks = seq(-0.15, 0, 0.05), limits = c(-0.15, 0)) +
    theme_twin() +
    ggplot2::labs(x = "Random effect on twinning probability", y = "Random effect on parity progression") -> p1

  data %>%
    dplyr::filter(.data$twin_total == 1) %>%
    tidyr::drop_na(.data$ranef_twin, .data$ranef_PP) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .data$ranef_twin, y = .data$ranef_PP), size = 0.05) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::scale_y_continuous(limits = c(-0.5, 1.55)) +
    ggplot2::scale_x_continuous(breaks = seq(0.3, 0.5, 0.05), limits = c(0.3, 0.5)) +
    theme_twin() +
    ggplot2::labs(x = "Random effect on twinning probability", y = "Random effect on parity progression") -> p2

  data %>%
    dplyr::filter(.data$twin_total == 2) %>%
    tidyr::drop_na(.data$ranef_twin, .data$ranef_PP) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .data$ranef_twin, y = .data$ranef_PP), size = 0.05) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::scale_y_continuous(limits = c(-0.5, 1.55)) +
    ggplot2::scale_x_continuous(breaks = seq(0.7, 0.95, 0.05), limits = c(0.7, 0.95)) +
    theme_twin() +
    ggplot2::labs(x = "Random effect on twinning probability", y = "Random effect on parity progression") -> p3

  data %>%
    dplyr::filter(.data$twin_total == 0) %>%
    tidyr::drop_na(.data$ranef_twin, .data$ranef_IBI) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .data$ranef_twin, y = .data$ranef_IBI), size = 0.05) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::scale_y_continuous(breaks = seq(-1.5, 1.5, 0.5), limits = c(-1.5, 1.55)) +
    ggplot2::scale_x_continuous(breaks = seq(-0.15, 0, 0.05), limits = c(-0.15, 0)) +
    theme_twin() +
    ggplot2::labs(x = "Random effect on twinning probability", y = "Random effect on interbirth interval") -> p4

  data %>%
    dplyr::filter(.data$twin_total == 1) %>%
    tidyr::drop_na(.data$ranef_twin, .data$ranef_IBI) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .data$ranef_twin, y = .data$ranef_IBI), size = 0.05) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::scale_y_continuous(breaks = seq(-1.5, 1.5, 0.5), limits = c(-1.5, 1.55)) +
    ggplot2::scale_x_continuous(breaks = seq(0.3, 0.5, 0.05), limits = c(0.3, 0.5)) +
    theme_twin() +
    ggplot2::labs(x = "Random effect on twinning probability", y = "Random effect on interbirth interval") -> p5

  data %>%
    dplyr::filter(.data$twin_total == 2) %>%
    tidyr::drop_na(.data$ranef_twin, .data$ranef_IBI) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .data$ranef_twin, y = .data$ranef_IBI), size = 0.05) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::scale_y_continuous(breaks = seq(-1.5, 1.5, 0.5), limits = c(-1.5, 1.55)) +
    ggplot2::scale_x_continuous(breaks = seq(0.7, 0.95, 0.05), limits = c(0.7, 0.95)) +
    theme_twin() +
    ggplot2::labs(x = "Random effect on twinning probability", y = "Random effect on interbirth interval") -> p6

  cowplot::plot_grid(p1, p2, p3, p4, p5, p6, labels = "auto", label_size = 12, nrow = 2)
}


#' @describeIn figures draw fig. S5
#' @export
#'
draw_fig_S5 <- function(data) {

  draw_fig_2(data$data_fig_2) +
    ggplot2::geom_line(data = data$data_fig_S5, linetype = "dashed", lwd = 1) +
    ggplot2::geom_line(data = data$data_fig_S5, ggplot2::aes(y = .data$lwr), linetype = "dotted", lwd = 1) +
    ggplot2::geom_line(data = data$data_fig_S5, ggplot2::aes(y = .data$upr), linetype = "dotted", lwd = 1)
}


#' @describeIn figures draw fig. S6A
#' @export
#'
draw_fig_S6A <- function(data) {

  data %>%
    dplyr::group_by(.data$maternal_id, .data$scenario) %>%
    dplyr::summarise(total_birth = max(.data$parity, na.rm = TRUE),
                     twinner = factor(ifelse(any(.data$twin), "twinner", "non-twinner"),
                                      levels = c("non-twinner", "twinner"))) %>%
    dplyr::group_by(.data$total_birth, .data$scenario, .data$twinner) %>%
    dplyr::count() -> data_sample_sizes

  data_sample_sizes %>%
    ggplot2::ggplot() +
    ggplot2::aes(.data$total_birth, .data$n) +
    ggplot2::geom_line(ggplot2::aes(col = .data$scenario, linetype = .data$twinner)) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$twinner)) +
    ggplot2::scale_y_continuous("Number of observations", trans = "log",
                                breaks = c(1, 10, 100, 1000, 10000),
                                labels = c("1", "10", "100", "1000", "10000"),
                                limits = c(1, 10000)) +
    ggplot2::scale_color_manual("", values = c("darkgreen", "lightgreen")) +
    ggplot2::scale_shape_discrete("", solid = FALSE) +
    ggplot2::scale_linetype_discrete("") +
    ggplot2::scale_x_continuous("Maternal total births", breaks = c(1, 5, 10, 15, 18),
                                labels = c("1", "5", "10", "15", "18")) +
    theme_twin() +
    ggplot2::guides(col = ggplot2::guide_legend(override.aes = list(shape = 15, size = 4)),
                    linetype = ggplot2::guide_legend(ovveride.aes = list(size = 8)),
                    shape = ggplot2::guide_legend(ovveride.aes = list(size = 8))) +
    ggplot2::theme(
      legend.position = c(.15, 0.01),
      legend.justification = c("left", "bottom"),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.title = ggplot2::element_text(size = 6),
      legend.text  = ggplot2::element_text(size = 6),
      legend.background = ggplot2::element_blank(),
      legend.spacing.y = ggplot2::unit(0.05, "cm"),
      legend.key.size = ggplot2::unit(1, "lines"))

}


#' @describeIn figures draw fig. S6B
#' @export
#'
draw_fig_S6B <- function(data) {

  data %>%
    dplyr::group_by(.data$maternal_id, .data$scenario) %>%
    dplyr::summarise(twinner = any(.data$twin)) %>%
    dplyr::group_by(.data$twinner, .data$scenario) %>%
    dplyr::count() -> data_sample_sizes

  data_sample_sizes %>%
    ggplot2::ggplot() +
    ggplot2::aes(.data$twinner, .data$n, fill = .data$scenario) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_y_continuous("Number of observations", trans = "log",
                                breaks = c(1, 10, 100, 1000, 10000)) +
    ggplot2::scale_fill_manual("Scenario", values = c("darkgreen", "lightgreen"), guide = ggplot2::guide_legend(title.vjust = 0.5, nrow = 2)) +
    ggplot2::scale_x_discrete("Lifetime twin. status", breaks = c(0, 1),
                              labels = c("Non-twinner", "Twinner")) +
    theme_twin() +
    ggplot2::theme(legend.position = "none")

}


#' @describeIn figures draw fig. S6C
#' @export
#'
draw_fig_S6C <- function(data) {

  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(.data$age, col = .data$scenario) +
    ggplot2::geom_line(stat = "density") +
    ggplot2::scale_y_continuous("Density", breaks = c(0.00, 0.02, 0.04, 0.06),
                                limits = c(0.00, 0.06)) +
    ggplot2::scale_x_continuous("Maternal age at birth") +
    ggplot2::scale_color_manual(values = c("darkgreen", "lightgreen")) +
    theme_twin() +
    ggplot2::theme(legend.position = "none")

}


#' @describeIn figures draw fig. S6D
#' @export
#'
draw_fig_S6D <- function(data) {

  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(.data$twin, fill = .data$scenario) +
    ggplot2::geom_bar(position = "dodge") +
    ggplot2::scale_y_continuous("Number of observations", trans = "log",
                                breaks = c(1, 10, 100, 1000, 10000, 100000),
                                labels = c("1", "10", "100", "1000", "10000", "100000")) +
    ggplot2::scale_fill_manual("Scenario", values = c("darkgreen", "lightgreen"), guide = ggplot2::guide_legend(title.vjust = 0.5, nrow = 2)) +
    ggplot2::scale_x_discrete("Birth outcome", breaks = c(0, 1),
                              labels = c("Singleton", "Twin")) +
    theme_twin() +
    ggplot2::theme(legend.position = "none")

}

