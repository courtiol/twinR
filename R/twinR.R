#' Welcome to the R package twinR
#'
#' This package contains the code necessary to produce the results of the paper
#' _"Twinners in pre-industrial Europe: more babies but fewer births"_. See section __Examples__
#' below.
#'
#' @name twinR-package
#' @aliases twinR-package twinR
#' @docType package
#'
#' @keywords package
#'
#' @examples
#'
#' \dontrun{
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Loading packages --------------------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' library(spaMM)  ## for manipulating mixed effects models
#' # library(doSNOW) ## (optional) for better load-balancing during parallel computations in spaMM
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Setting packages options ------------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' ## Number of bootstrap replicates to perform:
#' nb_boot <- 1000L
#'
#' ## Identify number of CPU cores available for parallel computing,
#' ## note: using a large number may lead RAM to max out, so you may have to adjust that according
#' ## to your infrastructure:
#' nb_cores <- min(c(50L, parallel::detectCores() - 1))
#'
#' ## Set option in spaMM:
#' spaMM.options(nb_cores = nb_cores)
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Preparing datasets ------------------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' ## Filter the raw data to only keep data with monthly resolution:
#' data_births_monthly <- filter_data(data_births_all) # see ?filter_data
#'
#' ## Aggregate the data at the level of mothers:
#' data_mothers_all <- aggregate_data(data_births_all) # see ?aggregate_data
#' data_mothers_monthly <- aggregate_data(data_births_monthly)
#'
#' ## Expand the birth level data for the fit of statistical models:
#' data_births_monthly.complete <- expand_data(data_births_monthly) # see ?expand_data
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Table 1 & S16: data summary ---------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' dir.create("tables") # create a folder to store the tables
#'
#' ## Create table 1:
#'
#' table1 <- build_data_summary.table(data_births_monthly)
#' table1
#'
#' export_table_xlsx(table1, file = "tables/table1.xlsx")
#'
#'
#' ## Create table S16:
#'
#' tableS16 <- build_data_summary.table(data_births_all)
#' tableS16
#'
#' export_table_xlsx(tableS16, file = "tables/tableS16.xlsx")
#' write(format_data_summary.table_2_LaTeX(tableS16), file = "tables/tableS16.tex")
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Fitting models ----------------------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#'    #### Note: the fitted models are also saved and available on GitHub for you not to have to
#'    #### refit them all, see next section.
#'
#' fit_01 <- fit_totalbirths(data_mothers_monthly, when_twinner = "allbirths")
#' fit_02 <- fit_twinner.allbirths(data_mothers_monthly)
#' fit_03 <- fit_totalbirths(data_mothers_monthly, when_twinner = "firstbirth")
#' fit_04 <- fit_twinner.firstbirth(data_mothers_monthly)
#' fit_05 <- fit_twinning.binomial(data_mothers_monthly)
#'
#' ## we refit fit_05 on the complete dataset to study the effect of dropping observations:
#' fit_05bis <- fit_twinning.binomial(data_mothers_all) # for Fig S5
#'
#' ## we refit fit_05 with different parameterisation (for discussing interaction in Methods):
#' fit_05_pop_fixed <- fit_twinning.binomial_with_pop_as_fixed(data_mothers_all)
#' fit_05_pop_inter <- fit_twinning.binomial_with_pop_interaction(data_mothers_all)
#' `#`(anova(fit_05_pop_fixed, fit_05_pop_inter)) ## --> test interaction pop:births_total
#' #     chi2_LR df   p_value
#' #p_v 11.56422  8 0.1717319
#'
#' fit_06 <- fit_PP(data_births_monthly.complete)
#' fit_07 <- fit_IBI(data_births_monthly.complete)
#' fit_08 <- fit_twinning.binary(data_births_monthly.complete)
#' fit_09 <- fit_AFB(data_mothers_monthly)
#' fit_10 <- fit_PP(data_births_monthly.complete, twin_as.predictor = FALSE)
#' fit_11 <- fit_IBI(data_births_monthly.complete, twin_as.predictor = FALSE)
#' fit_12 <- fit_twinning.binary(data_births_monthly.complete, poly_order = 0L) # no age and parity
#' fit_13 <- fit_twinning.binary(data_births_monthly.complete, poly_order = 0L, # no age and parity
#'                               maternal_ID_as.predictor = FALSE)
#' fit_14 <- fit_twinning.binary(data_births_monthly.complete, maternal_ID_as.predictor = FALSE)
#'
#' ## for getting tibble summary output of a model fit, do e.g.:
#' build_fit_summary.table(fit_01)
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Saving fitted models ----------------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#'    #### Note: this is how we saved the models.
#'    #### To directly load them, just download them from our Github repository
#'    #### (folder "fitted_models") and open them in RStudio.
#'
#' dir.create("fitted_models") # create a folder to store the fitted models
#' save(fit_01, file = "fitted_models/fit_01.rda", compress = "xz")
#' save(fit_02, file = "fitted_models/fit_02.rda", compress = "xz")
#' save(fit_03, file = "fitted_models/fit_03.rda", compress = "xz")
#' save(fit_04, file = "fitted_models/fit_04.rda", compress = "xz")
#' save(fit_05, file = "fitted_models/fit_05.rda", compress = "xz")
#' save(fit_05bis, file = "fitted_models/fit_05bis.rda", compress = "xz")
#' save(fit_06, file = "fitted_models/fit_06.rda", compress = "xz")
#' save(fit_07, file = "fitted_models/fit_07.rda", compress = "xz")
#' save(fit_08, file = "fitted_models/fit_08.rda", compress = "xz")
#' save(fit_09, file = "fitted_models/fit_09.rda", compress = "xz")
#' save(fit_10, file = "fitted_models/fit_10.rda", compress = "xz")
#' save(fit_11, file = "fitted_models/fit_11.rda", compress = "xz")
#' save(fit_12, file = "fitted_models/fit_12.rda", compress = "xz")
#' save(fit_13, file = "fitted_models/fit_13.rda", compress = "xz")
#' save(fit_14, file = "fitted_models/fit_14.rda", compress = "xz")
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Computing effect sizes from fitted models -------------------
#' #------------------------------------------------------------------------------------------------
#'
#'    #### Note: see ?predictions for details on the two main underlying functions doing the job
#'
#' ## Computing extra (total) births to twinners (at any birth):
#'
#' effect_twinner_on_births <- compare_predictions(fit_01,
#'                                                 newdata = data.frame(twinner = c(TRUE, FALSE)),
#'                                                 nb_boot = nb_boot)
#'
#' `#`(effect_twinner_on_births$results, digits = 3L) # `#`() modifies display in Console, see ?`#`
#' #  estimate  lwr  upr
#' #1     1.43 1.22 1.65
#'
#'
#' ## Computing increase in odds of becoming a twinner (at any birth) with each additional birth:
#'
#' effect_births_on_twinner <- compare_predictions(fit_02,
#'                                                 newdata = data.frame(births_total = c(2L, 1L)),
#'                                                 oddsratio = TRUE,
#'                                                 nb_boot = nb_boot)
#'
#' `#`(effect_births_on_twinner$results, digits = 3L)
#' #  estimate  lwr  upr
#' #1     1.17 1.16 1.19
#'
#'
#' ## Computing extra births to twinners at first birth:
#'
#' effect_first.twinner_on_births <- compare_predictions(fit_03,
#'                                             newdata = data.frame(first_twinner = c(TRUE, FALSE)),
#'                                             nb_boot = nb_boot)
#'
#' `#`(effect_first.twinner_on_births$results, digits = 3L)
#' #  estimate    lwr    upr
#' #1    -0.55 -0.881 -0.247
#'
#'
#' ## Computing increase in odds of becoming a twinner at first birth with each additional birth:
#'
#' effect_births_on_first.twinner <- compare_predictions(fit_04,
#'                                                 newdata = data.frame(births_total = c(2L, 1L)),
#'                                                 oddsratio = TRUE,
#'                                                 nb_boot = nb_boot)
#'
#' `#`(effect_births_on_first.twinner$results, digits = 3L)
#' #  estimate   lwr   upr
#' #1    0.934 0.895 0.971
#'
#'
#' ## Slope of the relationship between per-birth twinning probability and maternal births:
#'
#' main_slope <- fixef(fit_05)["births_total"]
#'
#' `#`(main_slope, digits = 3L)
#' #births_total
#' #    -0.0338
#'
#' main_slope_CI <- confint(fit_05, "births_total")$interval
#'
#' `#`(main_slope_CI, digits = 3L)
#' #lower births_total upper births_total
#' #           -0.0508            -0.0170
#'
#'
#' ## Same using the full dataset (legend Fig S5):
#'
#' main_slope_full <- fixef(fit_05bis)["births_total"]
#'
#' `#`(main_slope_full, digits = 3L)
#' #' #births_total
#' #     -0.0346
#'
#' main_slope_CI_full <- confint(fit_05bis, "births_total")$interval
#'
#' `#`(main_slope_CI_full, digits = 3L)
#' #lower births_total upper births_total
#' #           -0.0509            -0.0185
#'
#'
#' ## Computing increase in odds of twinning at a given birth with each additional birth:
#'
#' effect_births_on_twinning <- compare_predictions(fit_05,
#'                                                  newdata = data.frame(births_total = c(2L, 1L)),
#'                                                  oddsratio = TRUE,
#'                                                  nb_boot = nb_boot)
#'
#' `#`(effect_births_on_twinning$results, digits = 3L)
#' #  estimate   lwr   upr
#' #1    0.967 0.952 0.983
#'
#'
#' ## Computing range of per-birth twinning probability for 1 and 18 births:
#'
#' predictions_twinning_range_births <- compute_predictions(fit_05,
#'                                                  newdata = data.frame(births_total = c(1L, 18L)),
#'                                                  nb_boot = nb_boot)
#'
#' `#`(predictions_twinning_range_births$results, digits = 2L)
#' #  births_total estimate    lwr   upr
#' #1            1    0.021 0.0170 0.025
#' #2           18    0.012 0.0088 0.015
#'
#'
#' ## Increase in duration of interbirth interval after a twinning event at mean age and parity:
#' ## Note: this computation takes ~ 11 days on a single CPU
#'
#' effect_twinning_on_IBI <- compare_predictions(fit_07,
#'                                        newdata = data.frame(
#'                                              age = mean(data_births_monthly.complete$age),
#'                                              parity = mean(data_births_monthly.complete$parity),
#'                                              twin = c(TRUE, FALSE)),
#'                                        nb_boot = nb_boot)
#'
#' `#`(effect_twinning_on_IBI$results, digits = 3L) # in months
#' #  estimate   lwr    upr
#' #1    -1.03 -1.98 -0.194
#'
#'
#' ## Computing AFB for twinners and non-twinners with one and two total births (legend Fig S1):
#'
#' predictions_AFB_1_2_births <- compute_predictions(fit_09,
#'                                                  newdata = data.frame(
#'                                                        births_total_fac = c("1", "1", "2", "2"),
#'                                                        twinner = c(TRUE, FALSE, TRUE, FALSE)),
#'                                                  nb_boot = nb_boot)
#'
#' `#`(predictions_AFB_1_2_births$results) # in months
#' #  births_total_fac twinner estimate      lwr      upr
#' #1                1    TRUE 379.2339 361.8971 396.3609
#' #2                1   FALSE 351.7503 343.5182 360.2318
#' #3                2    TRUE 355.9884 342.6304 370.0890
#' #4                2   FALSE 338.0832 329.8782 346.0908
#'
#' `#`(predictions_AFB_1_2_births$results[, c("estimate", "lwr", "upr")]/12, digits = 3L) # in years
#' #  estimate  lwr  upr
#' #1     31.6 30.2 33.0
#' #2     29.3 28.6 30.0
#' #3     29.7 28.6 30.8
#' #4     28.2 27.5 28.8
#'
#'
#' ## Computing delay in AFB for twinners compared to non-twinners (legend Fig S1):
#'
#' effect_twinner_on_AFB_1_birth <- compare_predictions(fit_09,
#'                                        newdata = data.frame(
#'                                                        births_total_fac = "1",
#'                                                        twinner = c(TRUE, FALSE)),
#'                                        nb_boot = nb_boot)
#'
#' effect_twinner_on_AFB_2_births <- compare_predictions(fit_09,
#'                                        newdata = data.frame(
#'                                                        births_total_fac = "2",
#'                                                        twinner = c(TRUE, FALSE)),
#'                                        nb_boot = nb_boot)
#'
#' `#`(effect_twinner_on_AFB_1_birth$results, digits = 3L) # in months
#' #  estimate  lwr  upr
#' #1     27.5 11.9 43.7
#'
#' `#`(effect_twinner_on_AFB_2_births$results, digits = 3L)
#' #  estimate  lwr  upr
#' #1     17.9 6.55 29.4
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Plotting predictions from model fits ------------------------
#' #------------------------------------------------------------------------------------------------
#'
#'    #### Note: see ?figures for details on the underlying functions doing the job.
#'
#' dir.create("figures") # create a folder to store the figures
#'
#' min_births <- min(data_mothers_monthly$births_total)
#' max_births <- max(data_mothers_monthly$births_total)
#'
#'
#' ## Figure 1:
#'
#' data_fig_1A <- compute_predictions(fit_01,
#'                                    newdata = data.frame(twinner = c(TRUE, FALSE)),
#'                                    nb_boot = nb_boot)
#' data_fig_1B <- compute_predictions(fit_02,
#'                                    newdata = data.frame(births_total = min_births:max_births),
#'                                    nb_boot = nb_boot)
#' data_fig_1C <- compute_predictions(fit_03,
#'                                    newdata = data.frame(first_twinner = c(TRUE, FALSE)),
#'                                    nb_boot = nb_boot)
#' data_fig_1D <- compute_predictions(fit_04,
#'                                    newdata = data.frame(births_total = min_births:max_births),
#'                                    nb_boot = nb_boot)
#'
#' fig_1A <- draw_fig_1A(data_fig_1A)
#' fig_1B <- draw_fig_1B(data_fig_1B)
#' fig_1C <- draw_fig_1C(data_fig_1C)
#' fig_1D <- draw_fig_1D(data_fig_1D)
#'
#' plot_grid(fig_1A, fig_1B, fig_1C, fig_1D,
#'           labels = "AUTO", label_size = 12, align = "v", axis = "l")
#'
#' ggsave(file = "figures/fig1.pdf", width = 11.4, height = 8.5, units = "cm") # export as PDF
#' ggsave(file = "figures/fig1.png", width = 11.4, height = 8.5, units = "cm") # export as PNG
#'
#'
#' ## Figure 2:
#'
#' data_fig_2 <- compute_predictions(fit_05,
#'                                   newdata = data.frame(births_total = min_births:max_births),
#'                                   nb_boot = nb_boot)
#'
#' draw_fig_2(data_fig_2)
#'
#' ggsave(file = "figures/fig2.pdf", width = 8.7, height = 8.7*8.5/11.4, units = "cm")
#' ggsave(file = "figures/fig2.png", width = 8.7, height = 8.7*8.5/11.4, units = "cm")
#'
#'
#' ## Figure 3:
#'
#' data_fig_3A <- prepare_data_fig_3A(fit_06)
#' data_fig_3B <- prepare_data_fig_3B(fit_07)
#' data_fig_3C <- prepare_data_fig_3C(fit_08)
#'
#' fig_3A <- draw_fig_3A(data_fig_3A)
#' fig_3B <- draw_fig_3B(data_fig_3B)
#' fig_3C <- draw_fig_3C(data_fig_3C)
#' plot_grid(fig_3A, fig_3B, fig_3C,
#'           labels = "AUTO", nrow = 3, label_size = 12, align = "v", axis = "lr")
#'
#' ggsave(file = "figures/fig3.pdf", width = 8.7, height = 20, units = "cm")
#' ggsave(file = "figures/fig3.png", width = 8.7, height = 20, units = "cm")
#'
#'
#' ## Figure S1:
#'
#' data_fig_S1 <- prepare_data_fig_S1(fit_09)
#'
#' draw_fig_S1(data_fig_S1)
#'
#' ggsave(file = "figures/figS1.pdf", width = 11.4, height = 6, units = "cm")
#'
#'
#' ## Figure S2:
#'
#' data_fig_S2 <- prepare_data_fig_S2(fit_PP = fit_06, fit_IBI = fit_07, fit_twin = fit_08,
#'                                    mother_level_data = data_mothers_monthly)
#'
#' draw_fig_S2(data_fig_S2)
#'
#' ggsave(file = "figures/figS2.pdf", width = 17.8, height = 10, units = "cm", scale = 1.3)
#'
#'
#'
#' ## Figure S5:
#'
#' data_fig_S5 <- compute_predictions(fit_05bis,
#'                                    newdata = data.frame(births_total = min_births:max_births),
#'                                    nb_boot = nb_boot)
#'
#' draw_fig_S5(list(data_fig_2 = data_fig_2$results, data_fig_S5 = data_fig_S5$results))
#'
#' ggsave(file = "figures/figS5.pdf", width = 8.7, height = 8.7*8.5/11.4, units = "cm")
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Simulating the life history of mothers ----------------------
#' #------------------------------------------------------------------------------------------------
#'
#' ## Example of how to run one simulation using directly the R6 class,
#' ## (here considering the full models for PP, IBI and the per-birth twinning probability, i.e.
#' ## the scenario ABCD):
#'
#' simu_test <- life_histories$new(fit_PP = fit_06,           # prepare the simulation
#'                                 fit_IBI = fit_07,
#'                                 fit_twinning.binary = fit_08,
#'                                 birth_level_data = data_births_monthly.complete)
#'
#' set.seed(123) # fix the seed of the random generator for reproducibility
#'
#' simu_test$run() # run the simulation (here takes < 10 sec but timing depends on fitted models)
#'
#' simu_test$birth_level_data.simulated # check the simulated data
#'
#' simu_test$slope # check the slope of interest as measured on the simulated data
#'
#'
#' ## Example of how to fit the three life history models to the observed data according to a
#' ## scenario (here AC, which takes around 20 min on a single core):
#'
#' fits_AC_obs <- fit_life_histories(scenario = "AC",
#'                                   birth_level_data = data_births_monthly.complete)
#'
#'
#' ## Example of how to run one simulation using the wrapper:
#'
#' simu_AC <- run_simulation(birth_level_data = data_births_monthly.complete,
#'                           scenario = "AC",
#'                           life_history_fits = fits_AC_obs,
#'                           output = list(birth_level_data.simulated = TRUE,
#'                                         slope = TRUE,
#'                                         fits = FALSE))
#'
#'
#' ## Figure S6:
#'
#' data_fig_S6 <- prepare_data_fig_S6(simulation_obj = simu_AC,
#'                                    birth_level_data = data_births_monthly.complete)
#'
#' fig_S6A <- draw_fig_S6A(data_fig_S6)
#' fig_S6B <- draw_fig_S6B(data_fig_S6)
#' fig_S6C <- draw_fig_S6C(data_fig_S6)
#' fig_S6D <- draw_fig_S6D(data_fig_S6)
#'
#' plot_grid(fig_S6A, fig_S6B, fig_S6C, fig_S6D,
#'           labels = "AUTO", label_size = 12, align = "v", axis = "l")
#'
#' ggsave(file = "figures/figS6.pdf", width = 13, height = 10, units = "cm", scale = 1.3)
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Goodness of fit ---------------------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' ## Generate data to test scenario AC (example):
#'
#' ### IMPORTANT: while all the steps above should work no matter the operating system and whether
#' ### you are using R within a GUI (e.g. RStudio) or not, the following step is very
#' ### computationally intensive and has been tailored to be used on a Unix system (e.g. Linux) and
#' ### directly within a terminal. It may work in RStudio, but this is not guarantied, nor
#' ### recommended. If you run this code using Windows, it should fallback to running the
#' ### computation sequentially instead of in parallel across multiple CPU cores, which should work
#' ### fine at the cost of requiring probably weeks of running time.
#'
#' ## Generate data to test all scenarios (takes ca. 150 hours using 50 core CPUs at 2.9 GHz!!!):
#'
#' scenarios_to_do <- c("base_model", "A", "B", "C", "D", "AB", "AC", "AD", "BC", "BD", "CD",
#'                      "ABC", "ABD", "ACD", "BCD", "ABCD")
#'
#' dir.create("slopes_under_scenarios") # create a folder to store the simulated slopes
#'
#' ### Run scenarios one by one and save the output in a rda file:
#'
#' ### Note: contrary to the previous example, the first fit of the life history models is not
#' ### provided here but done internally, which avoids us to store all these large objects in the
#' ### package.
#'
#' #library(twinR)
#' #data_births_monthly <- filter_data(data_births_all)
#'
#' ## list scenarios to do:
#' scenarios_to_do <- c("base_model", "A", "B", "C", "D", "AB", "AC", "AD", "BC", "BD", "CD",
#'                      "ABC", "ABD", "ACD", "BCD", "ABCD")
#'
#' dir.create("slopes_under_scenarios")
#'
#' for (scenario in scenarios_to_do) {
#'
#'   ## set nb_cores depending on the scenarios due to RAM limitation
#'   nb_cores <- ifelse(grepl("D", scenario), 40, 50)
#'
#'   slopes_under_scenario <- simulate_slopes_for_GOF(N_replicates_level1 = 200L,
#'                                                    N_replicates_level2 = 49L,
#'                                                    birth_level_data = data_births_monthly,
#'                                                    scenario = scenario,
#'                                                    nb_cores = nb_cores,
#'                                                    timeout = 16 * 60 * 60,
#'                                                    .log = TRUE)
#'   name_obj <- paste0("slopes_under_", scenario)
#'   assign(name_obj, value = slopes_under_scenario)
#'   save(list = name_obj, file = paste0("slopes_under_scenarios/", name_obj, ".rda"))
#'   rm(list = name_obj) # remove the object behind the name!
#'   rm(slopes_under_scenario, scenario, name_obj) # remove the object directly
#' }
#'
#'
#' ## Combine simulated slopes:
#'
#' all_slopes <- combine_simulated_slopes()
#'
#'
#' ## Figure 4:
#'
#' draw_fig_4(all_slopes, width = 0.3)
#'
#' ggsave(file = "figures/fig4.pdf", width = 8.7, height = 8.7*8.5/11.4, units = "cm")
#'
#'
#' ## test scenarios (table S15):
#'
#' tableS15 <- goodness_of_fit(all_slopes)
#' tableS15
#'
#' write(format_goodness_of_fit.table_2_LaTeX(tableS15), file = "tables/tableS15.tex")
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #--- Studying the effect of twinning propensity on the number of offspring using simulations  ---
#' #------------------------------------------------------------------------------------------------
#'
#' ## Simulate the reproductive life history of mothers so to study the influence of  a simulation
#' ## scenario on the total number of offspring produced as well as on other metrics about the
#' ## reproductive life of mothers.
#'
#' ## Part one: simulation under AC:
#' simu_baseline <- simulate_reproduction(birth_level_data = data_births_monthly,
#'                                        scenario = "AC",
#'                                        life_history_fits = fits_AC_obs,
#'                                        effect_pt = 0,
#'                                        nb_cores = nb_cores)
#'
#'  `#`(rbind(apply(simu_baseline, 2L, mean),
#'            apply(simu_baseline, 2L, quantile, probs = 0.025),
#'            apply(simu_baseline, 2L, quantile, probs = 0.975)), digits = 3L)
#'
#' #     twinning_rate twinner_rate total_offsprings total_births  seed
#' #[1,]        0.0167       0.0772             4.91         4.83 50.50
#' #[2,]        0.0158       0.0734             4.88         4.80  3.48
#' #[3,]        0.0175       0.0813             4.95         4.87 97.50
#'
#'
#' ## Part two: simulation under AC with an increased probability of twinning:
#' simu_more_twins <- simulate_reproduction(birth_level_data = data_births_monthly,
#'                                          scenario = "AC",
#'                                          life_history_fits = fits_AC_obs,
#'                                          effect_pt = 2.5,
#'                                          nb_cores = nb_cores)
#'
#'  `#`(rbind(apply(simu_more_twins, 2L, mean),
#'            apply(simu_more_twins, 2L, quantile, probs = 0.025),
#'            apply(simu_more_twins, 2L, quantile, probs = 0.975)), digits = 3L)
#'
#' #     twinning_rate twinner_rate total_offsprings total_births  seed
#' #[1,]         0.167        0.535             5.53         4.74 50.50
#' #[2,]         0.165        0.529             5.50         4.71  3.48
#' #[3,]         0.170        0.541             5.56         4.76 97.50
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Building the SI ---------------------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' ## Create the supplementary tables of the fitted models:
#' all_fits <- c(paste0("0", 1:9), 10:14)
#'
#' for (fit_nb in all_fits){
#'  load(paste0("fitted_models/fit_", fit_nb, ".rda"))
#'  write(format_fit_summary.table_2_LaTeX(build_fit_summary.table(get(paste0("fit_", fit_nb))),
#'                                                                 fit_n = as.numeric(fit_nb)),
#'        file = paste0("tables/tableS", fit_nb, ".tex"))
#' }
#'
#' ## Generate the Supplementary Information material:
#'
#' ## Note: This requires a full installation of the LaTeX system.
#'
#' build_SI()
#'
#' }
#'
NULL
