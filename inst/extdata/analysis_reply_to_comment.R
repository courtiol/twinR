## Analysis for reply to comment in NatCom

library(twinR)
library(spaMM)
library(doSNOW)
library(dplyr)
library(ggplot2)
library(cowplot)

nb_cores <- min(c(50L, parallel::detectCores() - 1))
spaMM.options(nb_cores = nb_cores)

# Functions definition -----------------------------------------------------

create_data_plots <- function(data, nb_boot){

  ## original data
  data_births_monthly <- data
  data_mothers_monthly <- aggregate_data(data_births_monthly)

  ## original data after dropping last births
  data |>
    mutate(last_birth = row_number() == n(), .by = "maternal_id") |>
    filter(!last_birth) -> data_births_all_noLB

  data_mothers_monthly_noLB <- aggregate_data(data_births_all_noLB)

  ## data without mothers born before 1850
  data_births_all_post1850 <- data[data$maternal_birthyear >= 1850, ]
  data_mothers_monthly_post1850 <- aggregate_data(data_births_all_post1850)

  ## data without LB and mothers born before 1850
  data_births_all_noLB_post1850 <- data_births_all_noLB[data_births_all_noLB$maternal_birthyear >= 1850, ]
  data_mothers_monthly_noLB_post1850 <- aggregate_data(data_births_all_noLB_post1850)

  ## original fit
  fit <- fit_twinning.binomial(data_mothers_monthly)

  ## original fit without last birth
  fit_noLB <- fit_twinning.binomial(data_mothers_monthly_noLB)

  ## original fit without data from mothers born before 1850
  fit_post1850 <- fit_twinning.binomial(data_mothers_monthly_post1850)

  ## original fit without last births and without data from mothers born before 1850
  fit_noLB_post1850 <- fit_twinning.binomial(data_mothers_monthly_noLB_post1850)

  ## original plot
  min_births <- min(data_mothers_monthly$births_total)
  max_births <- max(data_mothers_monthly$births_total)

  data_fig_2 <- compute_predictions(fit,
                                    newdata = data.frame(births_total = min_births:max_births),
                                    nb_boot = nb_boot,
                                    random = length(unique(data$pop)) > 1)

  ## predictions without last births
  min_births_noLB <- min(data_mothers_monthly_noLB$births_total)
  max_births_noLB <- max(data_mothers_monthly_noLB$births_total)

  data_fig_2_noLB <- compute_predictions(fit_noLB,
                                         newdata = data.frame(births_total = min_births_noLB:max_births_noLB),
                                         nb_boot = nb_boot,
                                         random = length(unique(data$pop)) > 1)

  ## predictions without mothers born before 1850
  min_births_post1850 <- min(data_mothers_monthly_post1850$births_total)
  max_births_post1850 <- max(data_mothers_monthly_post1850$births_total)

  data_fig_2_post1850 <- compute_predictions(fit_post1850,
                                             newdata = data.frame(births_total = min_births_post1850:max_births_post1850),
                                             nb_boot = nb_boot,
                                             random = length(unique(data$pop)) > 1)

  ## predictions without LB and without mothers born before 1850
  min_births_noLB_post1850 <- min(data_mothers_monthly_noLB_post1850$births_total)
  max_births_noLB_post1850 <- max(data_mothers_monthly_noLB_post1850$births_total)

  data_fig_2_noLB_post1850 <- compute_predictions(fit_noLB_post1850,
                                                  newdata = data.frame(births_total = min_births_noLB_post1850:max_births_noLB_post1850),
                                                  nb_boot = nb_boot,
                                                  random = length(unique(data$pop)) > 1)

  ## return
  list(data_fig_2 = data_fig_2,
       data_fig_2_noLB = data_fig_2_noLB,
       data_fig_2_post1850 = data_fig_2_post1850,
       data_fig_2_noLB_post1850 = data_fig_2_noLB_post1850,
       fits = list(fit = fit,
                   fit_noLB = fit_noLB,
                   fit_post1850 = fit_post1850,
                   fit_noLB_post1850 = fit_noLB_post1850))
}

draw_plot <- function(data, title = "") {
  ggplot(data) +
    aes(x = births_total, y = estimate, colour = pop) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = pop), alpha = 0.3) +
    geom_line(linewidth = 1) +
    labs(title = title) +
    scale_x_continuous(breaks = seq(1, 20, 2)) +
    scale_y_continuous(trans = "logit",
                       breaks = seq(0.005, 0.03, by = 0.005)) +
    scale_color_manual(values = c("purple4", "azure3")) +
    scale_fill_manual(values = c("purple4", "azure3")) +
    theme_twin(larger = 0.8) +
    coord_cartesian(xlim = c(1, 18), ylim =  c(0.008, 0.03)) +
    labs(y = "Per-birth twin. prob.",
         x = "Maternal total births",
         colour = "Populations",
         fill = "Populations") +
    theme(legend.direction = "vertical", legend.title.align = 0.5)
}


# Fitting the models --------------------------------------------------------

data_births_all_EE <- readRDS(paste0(system.file(package = "twinR"), "/extdata/data_births_all_EE.rds"))
data_births_all_EE$monthly <- TRUE
data_births_all_EE$pop <- "EE"

data_births_all |>
  count(maternal_id, maternal_birthyear) |>
  count(maternal_id) |>
  filter(n > 1) |>
  pull(maternal_id) -> ID_wrong_maternal_year ## a few data preparation error exist for Norway
                                              ## this applies to maternal birth years which we did not use before

data_births_all |>
  filter(!maternal_id %in% ID_wrong_maternal_year) -> data_births_all2

nb_boot <- 5000 ## use > 1000 for plots
result_estonian_study <- create_data_plots(data = data_births_all_EE, nb_boot = nb_boot)
result_original_study <- create_data_plots(data = data_births_all2, nb_boot = nb_boot)


# Extracting info on datasets ---------------------------------------------

## Estonia all data
length(unique(result_estonian_study$fits$fit$data$maternal_id)) # 125575 mothers
sum(result_estonian_study$fits$fit$data$births_total) # 417418 birth events

## Estonia without last births
length(unique(result_estonian_study$fits$fit_noLB$data$maternal_id)) # 98183 mothers
sum(result_estonian_study$fits$fit_noLB$data$births_total) # 291843 birth events

## Original all data
length(unique(result_original_study$fits$fit$data$maternal_id)) # 23267 mothers
sum(result_original_study$fits$fit$data$births_total) # 115963 birth events

## Original without last births
length(unique(result_original_study$fits$fit_noLB$data$maternal_id)) # 20309 mothers
sum(result_original_study$fits$fit_noLB$data$births_total) # 92696 birth events

## Original no pre 1850
length(unique(result_original_study$fits$fit_post1850$data$maternal_id)) # 5410 mothers
sum(result_original_study$fits$fit_post1850$data$births_total) # 24735 birth events

## Original no pre 1850 without last births
length(unique(result_original_study$fits$fit_noLB_post1850$data$maternal_id)) # 4549 mothers
sum(result_original_study$fits$fit_noLB_post1850$data$births_total) # 19325 birth events

## Mothers born before 1850 in original data:
data_births_all2 |>
  select(-maternal_age) |>
  mutate(post_1850 = maternal_birthyear >= 1850) |>
  summarise(n = length(unique(maternal_id)), .by = c("post_1850")) |>
  mutate(pct = 100 * n / sum(n))
# A tibble: 2 × 3
# post_1850     n   pct
# <lgl>     <int> <dbl>
#   1 FALSE     17857  76.7
#   2 TRUE       5410  23.3


# Compare datasets using models with last births -------------------------------

data_all_pre1850 <- bind_rows(result_original_study$fits$fit_post1850$data,
                              result_estonian_study$fits$fit_post1850$data) |>
  mutate(dataset = ifelse(pop == "EE", "EE", "original"))

fit_all_pre1850 <- fitme(cbind(twin_total, singleton_total) ~ 1 + births_total*dataset + (1 | pop),
                         family = binomial(link = "logit"),
                         data = data_all_pre1850)

fit_all_pre1850_no_int <- fitme(cbind(twin_total, singleton_total) ~ 1 + births_total + (1 | pop),
                                family = binomial(link = "logit"),
                                data = data_all_pre1850)

anova(fit_all_pre1850, fit_all_pre1850_no_int)
# chi2_LR df  p_value
# p_v 3.129245  2 0.209167


# Compare datasets using models without last births -------------------------------

data_all_noLB_pre1850 <- bind_rows(result_original_study$fits$fit_noLB_post1850$data,
                                   result_estonian_study$fits$fit_noLB_post1850$data) |>
  mutate(dataset = ifelse(pop == "EE", "EE", "original"))

fit_all_noLB_pre1850 <- fitme(cbind(twin_total, singleton_total) ~ 1 + births_total*dataset + (1 | pop),
                              family = binomial(link = "logit"),
                              data = data_all_noLB_pre1850)

fit_all_noLB_pre1850_no_int <- fitme(cbind(twin_total, singleton_total) ~ 1 + births_total + (1 | pop),
                                     family = binomial(link = "logit"),
                                     data = data_all_noLB_pre1850)

anova(fit_all_noLB_pre1850, fit_all_noLB_pre1850_no_int)
#         chi2_LR df   p_value
# p_v 0.003886339  2 0.9980587


# Preparing data for plots -------------------------------------------------

result_original_study$data_fig_2$results$last_birth <- TRUE
result_original_study$data_fig_2$results$pop <- "9 other European"
result_original_study$data_fig_2$results$pre1850_included <- TRUE

result_original_study$data_fig_2_noLB$results$last_birth <- FALSE
result_original_study$data_fig_2_noLB$results$pop <- "9 other European"
result_original_study$data_fig_2_noLB$results$pre1850_included <- TRUE

result_original_study$data_fig_2_post1850$results$last_birth <- TRUE
result_original_study$data_fig_2_post1850$results$pop <- "9 other European"
result_original_study$data_fig_2_post1850$results$pre1850_included <- FALSE

result_original_study$data_fig_2_noLB_post1850$results$last_birth <- FALSE
result_original_study$data_fig_2_noLB_post1850$results$pop <- "9 other European"
result_original_study$data_fig_2_noLB_post1850$results$pre1850_included <- FALSE

result_estonian_study$data_fig_2$results$last_birth <- TRUE
result_estonian_study$data_fig_2$results$pop <- "Estonian"
result_estonian_study$data_fig_2$results$pre1850_included <- FALSE

result_estonian_study$data_fig_2_noLB$results$last_birth <- FALSE
result_estonian_study$data_fig_2_noLB$results$pop <- "Estonian"
result_estonian_study$data_fig_2_noLB$results$pre1850_included <- TRUE

result_estonian_study$data_fig_2_post1850$results$last_birth <- TRUE
result_estonian_study$data_fig_2_post1850$results$pop <- "Estonian"
result_estonian_study$data_fig_2_post1850$results$pre1850_included <- TRUE

result_estonian_study$data_fig_2_noLB_post1850$results$last_birth <- FALSE
result_estonian_study$data_fig_2_noLB_post1850$results$pop <- "Estonian"
result_estonian_study$data_fig_2_noLB_post1850$results$pre1850_included <- FALSE


# Plotting ----------------------------------------------------------------

draw_plot(bind_rows(result_original_study$data_fig_2$results,
                    result_estonian_study$data_fig_2$results) |>
            mutate(pop = relevel(as.factor(pop), ref = "Estonian")),
          title = "Complete datasets\n (no date restriction)") -> plot1

draw_plot(bind_rows(result_original_study$data_fig_2_noLB$results,
                    result_estonian_study$data_fig_2_noLB$results) |>
            mutate(pop = relevel(as.factor(pop), ref = "Estonian")),
          title = "Without last births\n (no date restriction)") +
  theme(legend.position = "none") -> plot2

draw_plot(bind_rows(result_original_study$data_fig_2_post1850$results,
                    result_estonian_study$data_fig_2_post1850$results) |>
            mutate(pop = relevel(as.factor(pop), ref = "Estonian")),
          title = "Complete datasets\n (mothers born 1850-1899)") +
  theme(legend.position = "none") -> plot3

draw_plot(bind_rows(result_original_study$data_fig_2_noLB_post1850$results,
                    result_estonian_study$data_fig_2_noLB_post1850$results) |>
            mutate(pop = relevel(as.factor(pop), ref = "Estonian")),
          title = "Without last births\n (mothers born 1850-1899)") +
  theme(legend.position = "none") -> plot4

legend <- get_legend(plot1 +
                     guides(color = guide_legend(nrow = 1)) +
                     theme(legend.position = "bottom", plot.margin = margin(t = 0, b = 0)))
plot1 <- plot1 + theme(legend.position = "none")

plot_grid(legend,
          plot_grid(plot1, plot2, plot3, plot4, nrow = 2, labels = "auto") + theme(plot.margin = margin(r = 5)),
          nrow = 2, rel_heights = c(0.15, 1))
ggsave(file = "figures/fig1_reply.pdf", width = 180, height = 180, units = "mm")
ggsave(file = "figures/fig1_reply.png", width = 180, height = 180, units = "mm")

