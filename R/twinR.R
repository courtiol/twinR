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
#' #----------------------------------------------------------------------------
#' #----------------------- Preparing datasets ---------------------------------
#' #----------------------------------------------------------------------------
#'
#' ## Filter the raw data to only keep data with monthly resolution:
#' data_births_monthly <- filter_data(data_births_all)
#'
#' ## Aggregate the data at the level of mothers:
#' data_mothers_monthly <- aggregate_data(data_births_monthly)
#'
#'
#'
#' #----------------------------------------------------------------------------
#' #----------------------- Table 1 & S16: data summary ------------------------
#' #----------------------------------------------------------------------------
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
#' #----------------------------------------------------------------------------
#' #----------------------- Fitting models -------------------------------------
#' #----------------------------------------------------------------------------
#'
#' fit1 <- fit_totalbirths(data_mothers_monthly, when_twinner = "allbirths")
#' fit2 <- fit_twinner.allbirths(data_mothers_monthly)
#' fit3 <- fit_totalbirths(data_mothers_monthly, when_twinner = "firstbirth")
#'
#' }
#'
NULL
