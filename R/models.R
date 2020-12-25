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
#' @param when_twinner a string of characters indicating if the twinner status is based on all births ('allbirths') or just on the first one ('firstbirth')
#' @export
#'
fit_births.total <- function(mother_level_data = NULL, when_twinner = "allbirths", args_spaMM = list(), verbose = TRUE) {

  if (when_twinner == "allbirths") {
    formula <- "births_total ~ 1 + twinner + (1|pop)"
    } else if (when_twinner == "firstbirth") {
    formula <- "births_total ~ 1 + first_twinner + (1|pop)"
    } else stop("argument 'when_twinner' in fit_births.total() must be 'life' or 'firstbirth'")

  if (verbose) print(paste0("Fitting model '", formula, "'... (be patient)"))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = spaMM::Tnegbin(link = "log"), method = "PQL/L")
  args <- c(args, args_spaMM)
  fit <- do.call(spaMM::fitme, args = args)

  if (verbose) print("done!")

  fit
}


#' @describeIn fit_models Fit the model predicting the twining status of the mother from her total number of births
#' @export
#'
fit_twinner <- function(mother_level_data = NULL, args_spaMM = list(), verbose = TRUE) {

  formula <- "twinner ~ 1 + births_total + (1|pop)"

  if (verbose) print(paste0("Fitting model '", formula, "'... (be patient)"))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = stats::binomial(link = "logit"), method = "PQL/L")
  args <- c(args, args_spaMM)
  fit <- do.call(spaMM::fitme, args = args)

  if (verbose) print("done!")

  fit
}
