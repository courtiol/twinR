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
#'
#' ## Computing extra births to twinners (at any birth):
#'
#' ## Computing increase in odds of becoming a twinner (at any birth) with each additional birth:
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
