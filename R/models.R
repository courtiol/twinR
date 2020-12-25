#' Fit the models
#'
#' These functions fit the models used in the paper.
#' They have been programmed to be used with our specific data structure.
#'
#' @name fit_models
#' @param mother_level_data a `tibble` or `data.frame` with mother level data
#' @param args_spaMM list of additional arguments to pass to the function [`fitme`][`spaMM::fitme`]
#' @param verbose whether to display the formula of the fit during the fitting procedure
#'
#' @return the fitted model
#'
NULL

#' @describeIn fit_models Fit the model predicting the total number of births per mother from her twinning status
#' @export
#'
fit_births.total <- function(mother_level_data = NULL, args_spaMM = list(), verbose = FALSE) {

  formula <- paste0("births_total ~ 1 + twinner + (1|pop)")

  if (verbose) print(paste0("Fitting model '", formula, "'... (be patient)"))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = spaMM::Tnegbin(link = "log"), method = "PQL/L")
  args <- c(args, args_spaMM)
  fit <- do.call(spaMM::fitme, args = args)

  if (verbose) print("done!")

  fit
}
