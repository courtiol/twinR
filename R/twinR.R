#' Welcome to the R package twinR
#'
#' This package contains the code necessary to produce the results of the paper
#' _"Twinners in pre-industrial Europe: more babies but fewer births"_.
#'
#' This document provides in its __Examples__ section the script producing all
#' the results of the paper.
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
#' library(doSNOW) ## for better load balancing in parallel computing done with spaMM
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Setting packages options ------------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' ## Number of bootstrap replicates to perform:
#' nb_boot <- 1000L
#'
#' ## Identify number of CPU cores available for parallel computing:
#' nb_cores <- min(c(100, parallel::detectCores() - 1))
#'
#' ## Set option in spaMM:
#' spaMM::spaMM.options(nb_cores = nb_cores)
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Preparing datasets ------------------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' ## Filter the raw data to only keep data with monthly resolution:
#' data_births_monthly <- filter_data(data_births_all) # See ?filter_data
#'
#' ## Aggregate the data at the level of mothers:
#' data_mothers_all <- aggregate_data(data_births_all) # See ?aggregate_data
#' data_mothers_monthly <- aggregate_data(data_births_monthly)
#'
#' ## Expand the birth level data for the fit of statistical models:
#' data_births_monthly.complete <- expand_data(data_births_monthly) # See ?expand_data
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
#' table1 <- build_summary_table(data_births_monthly)
#' table1
#' export_table_xlsx(table1, file = "tables/table1.xlsx")
#'
#' ## Create table S16:
#' tableS16 <- build_summary_table(data_births_all)
#' tableS16
#' export_table_xlsx(tableS16, file = "tables/tableS16.xlsx")
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Fitting models ----------------------------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' fit_01 <- fit_totalbirths(data_mothers_monthly, when_twinner = "allbirths")
#' fit_02 <- fit_twinner.allbirths(data_mothers_monthly)
#' fit_03 <- fit_totalbirths(data_mothers_monthly, when_twinner = "firstbirth")
#' fit_04 <- fit_twinner.firstbirth(data_mothers_monthly)
#' fit_05 <- fit_twinning.binomial(data_mothers_monthly)
#' fit_05bis <- fit_twinning.binomial(data_mothers_all) # for Fig S5
#' fit_06 <- fit_AFB(data_mothers_monthly)
#' fit_07 <- fit_PP(data_births_monthly.complete, poly_order = 5L) # use best polynomial order
#' fit_08 <- fit_IBI(data_births_monthly.complete, poly_order = 6L)
#' fit_09 <- fit_twinning.binary(data_births_monthly.complete, poly_order = 3L)
#' fit_10 <- fit_PP(data_births_monthly.complete, twin_as.predictor = FALSE, poly_order = 5L)
#' fit_11 <- fit_IBI(data_births_monthly.complete, twin_as.predictor = FALSE, poly_order = 6L)
#' fit_12 <- fit_twinning.binary(data_births_monthly.complete) # no age and parity
#' fit_13 <- fit_twinning.binary(data_births_monthly.complete, maternal_ID_as.predictor = FALSE)
#' fit_14 <- fit_twinning.binary(data_births_monthly.complete, poly_order = 3L,
#'                               maternal_ID_as.predictor = FALSE)
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Computing effect sizes from fitted models -------------------
#' #------------------------------------------------------------------------------------------------
#' # See ?predictions for details on the underlying functions doing the job
#'
#' ## Computing extra births to twinners (at any birth):
#' effect_twinner_on_births <- compare_predictions(fit_01,
#'                                                 newdata = data.frame(twinner = c(FALSE, TRUE)),
#'                                                 nb_boot = nb_boot)
#' `#`(effect_twinner_on_births$results) ## `#`() converts the output as text, see ?`#`
#' #  estimate  lwr  upr
#' #1     1.43 1.22 1.65
#'
#'
#' ## Computing increase in odds of becoming a twinner (at any birth) with each additional birth:
#' effect_births_on_twinner <- compare_predictions(fit_02,
#'                                                 newdata = data.frame(births_total = c(1, 2)),
#'                                                 oddsratio = TRUE,
#'                                                 nb_boot = nb_boot)
#' `#`(effect_births_on_twinner$results)
#' #  estimate  lwr  upr
#' #1     1.17 1.16 1.19
#'
#'
#' ## Computing extra births to twinners at first birth:
#'
#' ## Computing increase in odds of becoming a twinner at first birth with each additional birth:
#'
#' ## Slope of the relationship between per-birth twinning probability and maternal total births:
#'
#' ## Same using the full dataset (legend Fig S5):
#'
#' ## Computing increase in odds of twinning at a given birth with each additional birth:
#'
#' ## Computing range of per-birth twinning probability for 1 and 18 births:
#'
#' ## Increase in duration of interbirth interval after a twinning event at mean age and parity:
#'
#' ## Computing AFB for twinners and non-twinners with one and two total births (legend Fig S1):
#'
#' ## Computing delay in AFB for twinners compared to non-twinners (legend Fig S1):
#'
#'
#'
#' #------------------------------------------------------------------------------------------------
#' #---------------------------------- Plotting predictions from model fits ------------------------
#' #------------------------------------------------------------------------------------------------
#'
#' ## Figure 1:
#'
#'
#' ## Figure 2:
#'
#'
#' ## Figure 3:
#'
#'
#' ## Figure S1:
#'
#'
#' ## Figure S2:
#'
#'
#' }
#'
NULL
