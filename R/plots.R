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
      title = ggplot2::element_text(colour = black, size = 10, family = base_family),
      axis.text.x = ggplot2::element_text(colour = black, size = 8, family = base_family),
      axis.text.y = ggplot2::element_text(colour = black, size = 8, family = base_family),
      axis.title.x = ggplot2::element_text(colour = black, size = 10, family = base_family, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)),
      axis.title.y = ggplot2::element_text(colour = black, size = 10,  family = base_family, margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 10)),
      plot.margin = ggplot2::margin(6, 12, 6, 6)
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


#' Functions producing the figures
#'
#' @param data a `data.frame` or a `list` containing the data to be plotted
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

  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = factor(.data$twinner),
                 y = .data$estimate,
                 ymin = .data$lwr,
                 ymax = .data$upr) +
    ggplot2::geom_col(color = "black",
                      fill = c("lightgray", "white"), width = .6) +
    ggplot2::geom_errorbar(size = 0.5, width = 0.1, linetype = 1) +
    ggplot2::labs(y = "Maternal total births", x = "Lifetime twin. status") +
    ggplot2::scale_x_discrete(labels = c("Non-twinner", "Twinner")) +
    ggplot2::scale_y_continuous(limits = c(0, 7), breaks = c(0, 2, 4, 6)) +
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
    ggplot2::expand_limits(y = 0) +
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
    ggplot2::scale_y_continuous(limits = c(0, 7), breaks = c(0, 2, 4, 6)) +
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
    ggplot2::expand_limits(y = 0) +
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
    ggplot2::scale_x_continuous(breaks = c(1, 5, 10, 15, 18),
                                labels = c("1", "5", "10", "15", "18")) +
    ggplot2::scale_y_continuous(trans = "logit",
                                breaks = c(0.01, 0.015, 0.02, 0.025),
                                labels = c("0.010", "0.015", "0.020", "0.025")) +
    theme_twin() +
    ggplot2::coord_cartesian()
}



