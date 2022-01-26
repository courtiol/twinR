#' Welcome to the R package twinR
#'
#' This package contains the code necessary to produce the results of the paper
#' _"Mothers with higher twinning propensity have lower fertility in pre-industrial Europe"_.
#' See section __Examples__ below.
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
#' #---------------------------------- Table 1 & S14: data summary ---------------------------------
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
#' ## Create table S14:
#'
#' tableS14 <- build_data_summary.table(data_births_all)
#' tableS14
#'
#' export_table_xlsx(tableS14, file = "tables/tableS14.xlsx")
#' write(format_data_summary.table_2_LaTeX(tableS14), file = "tables/tableS14.tex")
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
#' fit_03 <- fit_twinning.binomial(data_mothers_monthly)
#'
#' ## we refit fit_03 on the complete dataset to study the effect of dropping observations:
#' fit_03bis <- fit_twinning.binomial(data_mothers_all) # for Fig S5
#'
#' ## we refit fit_03 with different parameterisation (for discussing interaction in Methods):
#' fit_03_pop_fixed <- fit_twinning.binomial_with_pop_as_fixed(data_mothers_all)
#' fit_03_pop_inter <- fit_twinning.binomial_with_pop_interaction(data_mothers_all)
#' `#`(anova(fit_03_pop_fixed, fit_03_pop_inter)) ## --> test interaction pop:births_total
#' #     chi2_LR df   p_value
#' #p_v 11.56422  8 0.1717319
#'
#' fit_04 <- fit_PP(data_births_monthly.complete)
#' fit_05 <- fit_IBI(data_births_monthly.complete)
#' fit_06 <- fit_twinning.binary(data_births_monthly.complete)
#' fit_07 <- fit_AFB(data_mothers_monthly)
#' fit_08 <- fit_PP(data_births_monthly.complete, twin_as.predictor = FALSE)
#' fit_09 <- fit_IBI(data_births_monthly.complete, twin_as.predictor = FALSE)
#' fit_10 <- fit_twinning.binary(data_births_monthly.complete, poly_order = 0L) # no age and parity
#' fit_11 <- fit_twinning.binary(data_births_monthly.complete, poly_order = 0L, # no age and parity
#'                               maternal_ID_as.predictor = FALSE)
#' fit_12 <- fit_twinning.binary(data_births_monthly.complete, maternal_ID_as.predictor = FALSE)
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
#' save(fit_03bis, file = "fitted_models/fit_03bis.rda", compress = "xz")
#' save(fit_04, file = "fitted_models/fit_04.rda", compress = "xz")
#' save(fit_05, file = "fitted_models/fit_05.rda", compress = "xz")
#' save(fit_06, file = "fitted_models/fit_06.rda", compress = "xz")
#' save(fit_07, file = "fitted_models/fit_07.rda", compress = "xz")
#' save(fit_08, file = "fitted_models/fit_08.rda", compress = "xz")
#' save(fit_09, file = "fitted_models/fit_09.rda", compress = "xz")
#' save(fit_10, file = "fitted_models/fit_10.rda", compress = "xz")
#' save(fit_11, file = "fitted_models/fit_11.rda", compress = "xz")
#' save(fit_12, file = "fitted_models/fit_12.rda", compress = "xz")
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Computing effect sizes from fitted models -------------------
#' #------------------------------------------------------------------------------------------------
#'
#'    #### Note: see ?predictions for details on the two main underlying functions doing the job
#'
#' ## Computing extra (total) births to twinners:
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
#' ## Computing increase in odds of becoming a twinner with each additional birth:
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
#' ## Computing number of twinning events among twins
#' twin_counts <- table(data_mothers_all[data_mothers_all$twin_total > 0, "twin_total"])
#' twin_counts
#' `#`(twin_counts[1] / sum(twin_counts), digits = 3L) ## proportion twinners with one twin birth
#' #    1
#' #0.926
#'
#' singleton_twinners <- data_mothers_all[data_mothers_all$twin_total > 0,
#'                                       "singleton_total", drop = TRUE]
#' `#`(mean(singleton_twinners), digits = 3L) ## mean number of singleton per twinner
#' #[1] 5.22
#' `#`(length(singleton_twinners)) ## number of twinners
#' #[1] 1793
#'
#' singleton_nontwinners <- data_mothers_all[data_mothers_all$twin_total == 0,
#'                                           "singleton_total", drop = TRUE]
#' `#`(mean(singleton_nontwinners), digits = 3L) ## mean number of offspring per non-twinner
#' #[1] 4.88
#' `#`(length(singleton_nontwinners)) ## number of non-twinners
#' #[1] 21488
#' `#`(t.test(singleton_twinners, singleton_nontwinners)) ## difference in number of singletons
#' #	Welch Two Sample t-test
#' #
#' #data:  singleton_twinners and singleton_nontwinners
#' #t = 4.9241, df = 2122.1, p-value = 9.128e-07
#' #alternative hypothesis: true difference in means is not equal to 0
#' #95 percent confidence interval:
#' # 0.2061298 0.4789840
#' #sample estimates:
#' #mean of x mean of y
#' # 5.219186  4.876629
#' #
#'
#'
#' ## Slope of the relationship between lifetime twinning status and maternal births:
#'
#' aggregated_slope <- fixef(fit_02)["births_total"]
#'
#' `#`(aggregated_slope, digits = 3L)
#' #births_total
#' #       0.162
#'
#' aggregated_slope_CI <- confint(fit_02, parm = "births_total")$interval
#'
#' `#`(aggregated_slope_CI, digits = 3L)
#' #lower births_total upper births_total
#' #             0.145              0.178
#'
#'
#' ## Slope of the relationship between per-birth twinning probability and maternal births:
#'
#' main_slope <- fixef(fit_03)["births_total"]
#'
#' `#`(main_slope, digits = 3L)
#' #births_total
#' #    -0.0338
#'
#' main_slope_CI <- confint(fit_03, parm = "births_total")$interval
#'
#' `#`(main_slope_CI, digits = 3L)
#' #lower births_total upper births_total
#' #           -0.0510            -0.0168
#'
#'
#' ## Same using the full dataset (legend Fig S5):
#'
#' main_slope_full <- fixef(fit_03bis)["births_total"]
#'
#' `#`(main_slope_full, digits = 3L)
#' #' #births_total
#' #     -0.0346
#'
#' main_slope_CI_full <- confint(fit_03bis, "births_total")$interval
#'
#' `#`(main_slope_CI_full, digits = 3L)
#' #lower births_total upper births_total
#' #           -0.0511            -0.0183
#'
#'
#' ## Computing increase in odds of twinning at a given birth with each additional birth:
#'
#' effect_births_on_twinning <- compare_predictions(fit_03,
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
#' predictions_twinning_range_births <- compute_predictions(fit_03,
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
#' effect_twinning_on_IBI <- compare_predictions(fit_05,
#'                                        newdata = data.frame(
#'                                              age = mean(data_births_monthly.complete$age),
#'                                              parity = mean(data_births_monthly.complete$parity),
#'                                              twin = c(TRUE, FALSE)),
#'                                        nb_boot = nb_boot)
#'
#' `#`(effect_twinning_on_IBI$results, digits = 3L) # in months
#' #  estimate   lwr    upr
#' #1    -1.03 -1.95 -0.193
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Testing interaction births_total*pop ------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' fit_03fix    <- fitme(cbind(twin_total, singleton_total) ~ 1 + births_total+pop,
#'                       data = data_mothers_monthly, family = stats::binomial(link = "logit"),
#'                       method = "PQL/L")
#' fit_03intfix <- fitme(cbind(twin_total, singleton_total) ~ 1 + births_total*pop,
#'                       data = data_mothers_monthly, family = stats::binomial(link = "logit"),
#'                       method = "PQL/L")
#' fit_03intran <- fitme(cbind(twin_total, singleton_total) ~ 1 + births_total+(1+births_total|pop),
#'                       data = data_mothers_monthly, family = stats::binomial(link = "logit"),
#'                       method = "PQL/L")
#'
#' res_fix <- anova(fit_03fix, fit_03intfix, boot.repl = 1000, seed = 123)
#' `#`(res_fix$rawBootLRT, digits = 3L)
#' #    chi2_LR df p_value
#' #p_v    11.9  7   0.119
#'
#' res_ran <- anova(fit_03, fit_03intran, boot.repl = 1000, seed = 123)
#' `#`(res_ran$rawBootLRT, digits = 3L)
#' #    chi2_LR df p_value
#' #p_v    1.22 NA   0.313
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Results on age at first birth -------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' ## Computing AFB for twinners and non-twinners with one and two total births (legend Fig S1):
#'
#' predictions_AFB_1_2_births <- compute_predictions(fit_07,
#'                                                  newdata = data.frame(
#'                                                        births_total_fac = c("1", "1", "2", "2"),
#'                                                        twinner = c(TRUE, FALSE, TRUE, FALSE)),
#'                                                  nb_boot = nb_boot)
#'
#'`#`(predictions_AFB_1_2_births$results)
#' #  births_total_fac twinner estimate      lwr      upr
#' #1                1    TRUE 379.2339 362.5697 396.0314
#' #2                1   FALSE 351.7503 343.0546 360.2896
#' #3                2    TRUE 355.9884 342.4176 370.4585
#' #4                2   FALSE 338.0832 330.1087 346.3920
#'
#' `#`(predictions_AFB_1_2_births$results[, c("estimate", "lwr", "upr")]/12, digits = 3L) # in years
#' #  estimate  lwr  upr
#' #1     31.6 30.2 33.0
#' #2     29.3 28.6 30.0
#' #3     29.7 28.6 30.9
#' #4     28.2 27.5 28.9
#'
#'
#' ## Computing delay in AFB for twinners compared to non-twinners (legend Fig S1):
#'
#' effect_twinner_on_AFB_1_birth <- compare_predictions(fit_07,
#'                                        newdata = data.frame(
#'                                                        births_total_fac = "1",
#'                                                        twinner = c(TRUE, FALSE)),
#'                                        nb_boot = nb_boot)
#'
#' effect_twinner_on_AFB_2_births <- compare_predictions(fit_07,
#'                                        newdata = data.frame(
#'                                                        births_total_fac = "2",
#'                                                        twinner = c(TRUE, FALSE)),
#'                                        nb_boot = nb_boot)
#'
#' `#`(effect_twinner_on_AFB_1_birth$results, digits = 3L) # in months
#' #  estimate  lwr  upr
#' #1     27.5 12.5 43.0
#'
#' `#`(effect_twinner_on_AFB_2_births$results, digits = 3L)
#' #  estimate  lwr  upr
#' #1     17.9 6.24 30.1
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
#'
#' `#`(data_fig_1A$results, digits = 3L)
#' #  twinner estimate  lwr  upr
#' #1       1     6.29 5.89 6.72
#' #2       0     4.86 4.58 5.12
#'
#' data_fig_1B <- compute_predictions(fit_02,
#'                                    newdata = data.frame(births_total = min_births:max_births),
#'                                    nb_boot = nb_boot)
#'
#' fig_1A <- draw_fig_1A(data_fig_1A)
#' fig_1B <- draw_fig_1B(data_fig_1B)
#'
#' plot_grid(fig_1A, fig_1B, labels = "AUTO", label_size = 12, align = "v", axis = "l")
#'
#' ggsave(file = "figures/fig1.pdf", width = 11.4, height = 8.5/2, units = "cm") # export as PDF
#' ggsave(file = "figures/fig1.png", width = 11.4, height = 8.5/2, units = "cm") # export as PNG
#'
#'
#' ## Figure 2:
#'
#' data_fig_2 <- compute_predictions(fit_03,
#'                                   newdata = data.frame(births_total = min_births:max_births),
#'                                   nb_boot = nb_boot)
#'
#' draw_fig_2(data_fig_2)
#'
#' ggsave(file = "figures/fig2.pdf", width = 8.7, height = 8.7*8.5/11.4, units = "cm")
#' ggsave(file = "figures/fig2.png", width = 8.7, height = 8.7*8.5/11.4, units = "cm")
#'
#'
#' ## Figure 4:
#'
#' data_fig_4A <- prepare_data_fig_4A(fit_04)
#' data_fig_4B <- prepare_data_fig_4B(fit_05)
#' data_fig_4C <- prepare_data_fig_4C(fit_06)
#'
#' fig_4A <- draw_fig_4A(data_fig_4A)
#' fig_4B <- draw_fig_4B(data_fig_4B)
#' fig_4C <- draw_fig_4C(data_fig_4C)
#' plot_grid(fig_4A, fig_4B, fig_4C,
#'           labels = "AUTO", nrow = 3, label_size = 12, align = "v", axis = "lr")
#'
#' ggsave(file = "figures/fig4.pdf", width = 8.7, height = 20, units = "cm")
#' ggsave(file = "figures/fig4.png", width = 8.7, height = 20, units = "cm")
#'
#'
#' ## Figure S1:
#'
#' data_fig_S1 <- prepare_data_fig_S1(fit_07)
#'
#' draw_fig_S1(data_fig_S1)
#'
#' ggsave(file = "figures/figS1.pdf", width = 11.4, height = 6, units = "cm")
#'
#'
#' ## Figure S2:
#'
#' data_fig_S2 <- prepare_data_fig_S2(fit_PP = fit_04, fit_IBI = fit_05, fit_twin = fit_06,
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
#' data_fig_S5 <- compute_predictions(fit_03bis,
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
#' ## the scenario PISH):
#'
#' simu_test <- life_histories$new(fit_PP = fit_04,           # prepare the simulation
#'                                 fit_IBI = fit_05,
#'                                 fit_twinning.binary = fit_06,
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
#' ## scenario (here PIS, which takes around 20 min):
#'
#' fits_PIS_obs <- fit_life_histories(scenario = "PIS",
#'                                    birth_level_data = data_births_monthly.complete)
#'
#'
#' ## Example of how to run one simulation using the wrapper:
#'
#' simu_PIS <- run_simulation(birth_level_data = data_births_monthly.complete,
#'                            scenario = "PIS",
#'                            life_history_fits = fits_PIS_obs,
#'                            output = list(birth_level_data.simulated = TRUE,
#'                                          slope = TRUE,
#'                                          fits = FALSE))
#'
#'
#' ## Figure S6:
#'
#' data_fig_S6 <- prepare_data_fig_S6(simulation_obj = simu_PIS,
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
#' #-------------------------------- Goodness-of-fit tests -----------------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' ### IMPORTANT: while all the steps above should work no matter the operating system and whether
#' ### you are using R within a GUI (e.g. RStudio) or not, the following step is very
#' ### computationally intensive and has been tailored to be used on a Unix system (e.g. Linux) and
#' ### directly within a terminal. It may work in RStudio, but this is not guarantied, nor
#' ### recommended. If you run this code using Windows, it should fallback to running the
#' ### computation sequentially instead of in parallel across multiple CPU cores, which should work
#' ### fine at the cost of requiring probably weeks of running time.
#'
#' ### Run scenarios one by one and save the output in a rda file:
#'
#' #library(twinR)
#' #library(doSNOW)
#' nb_cores <- min(c(20L, parallel::detectCores() - 1)) ## only 20 since large memory footprint
#' timeout <- 1 * 60 * 60
#'
#' data_births_monthly <- filter_data(data_births_all)
#'
#' scenarios_to_do <- c("base_model", "P", "I", "S", "H",
#'                      "PI", "PS", "PH", "IS", "IH", "SH",
#'                      "PIS", "PIH", "PSH", "ISH", "PISH")
#'
#' dir.create("slopes_under_scenarios")
#'
#' ## fit all trios of models in parallel (for linux only, but easy to adjust for other OS):
#' pbmcapply::pbmclapply(scenarios_to_do, function(scenario) {
#'   fits <- fit_life_histories(scenario = scenario,
#'                              birth_level_data = data_births_monthly)
#'   save(fits, file = paste0("fitted_models/non_shared/fits_", scenario, "_obs.rda"), compress = "xz")
#'
#'   rm(fits)
#'   }, mc.cores = min(c(length(scenarios_to_do), nb_cores)), mc.preschedule = FALSE)
#'
#'
#'
#' for (scenario in scenarios_to_do) {
#'
#'    load(file = paste0("fitted_models/non_shared/fits_", scenario, "_obs.rda"))
#'
#'    slopes_under_scenario <- simulate_slopes_for_GOF(N_replicates_level1 = 200L,
#'                                                     N_replicates_level2 = 49L,
#'                                                     birth_level_data = data_births_monthly,
#'                                                     life_history_fits = fits,
#'                                                     scenario = scenario,
#'                                                     nb_cores = nb_cores,
#'                                                     timeout = timeout,
#'                                                     .log = TRUE, lapply_pkg = "pbmcapply",
#'                                                     verbose = list(fit = TRUE, simu = FALSE))
#'    rm(fits)
#'    name_obj <- paste0("slopes_under_", scenario)
#'    assign(name_obj, value = slopes_under_scenario)
#'    save(list = name_obj, file = paste0("slopes_under_scenarios/", name_obj, ".rda"))
#'    rm(list = name_obj) # remove the object behind the name!
#'    rm(slopes_under_scenario, scenario, name_obj) # remove the object directly
#' }
#'
#'
#'
#' ## Combine simulated slopes:
#'
#' all_slopes <- combine_simulated_slopes()
#'
#'
#' ## Figure 5:
#'
#' draw_fig_5(all_slopes, width = 1)
#'
#' ggsave(file = "figures/fig5.pdf", width = 8.7, height = 8.7*8.5/11.4, units = "cm")
#' ggsave(file = "figures/fig5.png", width = 8.7, height = 8.7*8.5/11.4, units = "cm")
#'
#'
#' ## test scenarios (table S13):
#'
#' tableS13 <- goodness_of_fit(all_slopes)
#' tableS13
#'
#' write(format_goodness_of_fit.table_2_LaTeX(tableS13), file = "tables/tableS13.tex")
#'
#'
#' ## computing time (for gof analysis):
#'
#' computing_time_analysis(all_slopes)
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
#' ## Part one: simulation under PIS:
#' simu_baseline <- simulate_reproduction(birth_level_data = data_births_monthly,
#'                                        scenario = "PIS",
#'                                        life_history_fits = fits_PIS_obs,
#'                                        effect_pt = 0,
#'                                        nb_cores = nb_cores)
#'
#'  `#`(rbind(apply(simu_baseline[, - 7], 2L, mean),
#'            apply(simu_baseline[, - 7], 2L, quantile, probs = 0.025),
#'            apply(simu_baseline[, - 7], 2L, quantile, probs = 0.975)), digits = 3L)
#'
#' #     twinning_rate twinner_rate total_offsprings total_offsprings_Helle_et_al
#' #[1,]        0.0167       0.0771             4.91                         3.98
#' #[2,]        0.0158       0.0737             4.88                         3.96
#' #[3,]        0.0176       0.0816             4.95                         4.00
#' # total_offsprings_Haukioja_et_al   total_births
#' #                            3.41           4.83
#' #                            3.39           4.81
#' #                            3.43           4.86
#'
#'
#' ## Part two: simulation under AC with an increased probability of twinning:
#' simu_more_twins <- simulate_reproduction(birth_level_data = data_births_monthly,
#'                                          scenario = "PIS",
#'                                          life_history_fits = fits_PIS_obs,
#'                                          effect_pt = 2.5,
#'                                          nb_cores = nb_cores)
#'
#'  `#`(rbind(apply(simu_more_twins[, - 7], 2L, mean),
#'            apply(simu_more_twins[, - 7], 2L, quantile, probs = 0.025),
#'            apply(simu_more_twins[, - 7], 2L, quantile, probs = 0.975)), digits = 3L)
#'
#' #     twinning_rate twinner_rate total_offsprings total_offsprings_Helle_et_al
#' #[1,]         0.167        0.535             5.54                         4.23
#' #[2,]         0.165        0.530             5.50                         4.20
#' #[3,]         0.169        0.541             5.58                         4.26
#' # total_offsprings_Haukioja_et_al total_births
#' #                            3.33         4.75
#' #                            3.31         4.72
#' #                            3.35         4.78
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Building the SI ---------------------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' ## Create the supplementary tables of the fitted models:
#' all_fits <- c(paste0("0", 1:9), 10:12)
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
