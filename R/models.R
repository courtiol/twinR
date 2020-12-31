#' Fit the models
#'
#' These functions fit the models used in the paper.
#' They have been programmed to be used with our specific data structure.
#' See **Functions**, below, for details on each function.
#'
#' @name fit_models
#' @param mother_level_data a `tibble` or `data.frame` with mother level data
#' @param birth_level_data a `tibble` or `data.frame` with (expanded) birth level data
#' @param poly_order an integer value defining the polynomial order when considering the effect of age and parity (default = `NA`, find best value between 0 and 6)
#' @param twin_as.predictor whether to include the variable `twin` as a predictor or not in some models (default = `TRUE`)
#' @param maternal_ID_as.predictor whether to include the variable `maternal_ID` as a random effect predictor or not in some models (default = `TRUE`)
#' @param args_spaMM list of additional arguments to pass to the function [`fitme`][`spaMM::fitme`]
#' @param verbose whether to display the formula of the fit during the fitting procedure
#' @param scenario the scenario defining which models to be fitted: e.g. "ABCD", "AC"... (see paper for explanations)
#'
#' @return the fitted model
#' @examples
#' # See ?twinR
#'
NULL


#' @describeIn fit_models fit the model predicting the total number of births per mother from her twinning status
#' @param when_twinner a string of characters indicating if the twinning status is based on all births ('allbirths') or just on the first one ('firstbirth')
#' @export
#'
fit_totalbirths <- function(mother_level_data, when_twinner = "allbirths", args_spaMM = list(), verbose = TRUE) {

  if (when_twinner == "allbirths") {
    formula <- "births_total ~ 1 + twinner + (1|pop)"
    } else if (when_twinner == "firstbirth") {
    formula <- "births_total ~ 1 + first_twinner + (1|pop)"
    } else stop("argument 'when_twinner' in fit_births.total() must be 'life' or 'firstbirth'")

  if (verbose) print(paste0("Fitting model '", formula, "'..."))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = spaMM::Tnegbin(link = "log"), method = "PQL/L")
  args <- c(args, args_spaMM)
  fit <- do.call(spaMM::fitme, args = args)

  if (verbose) print("done!")

  fit
}


#' @describeIn fit_models fit the model predicting the twinning status of the mother at any birth during her life from her total number of births
#' @export
#'
fit_twinner.allbirths <- function(mother_level_data, args_spaMM = list(), verbose = TRUE) {

  formula <- "twinner ~ 1 + births_total + (1|pop)"

  if (verbose) print(paste0("Fitting model '", formula, "'..."))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = stats::binomial(link = "logit"), method = "PQL/L")
  args <- c(args, args_spaMM)
  fit <- do.call(spaMM::fitme, args = args)

  if (verbose) print("done!")

  fit
}


#' @describeIn fit_models fit the model predicting the twinning status of the mother at first birth from her total number of births
#' @export
#'
fit_twinner.firstbirth <- function(mother_level_data, args_spaMM = list(), verbose = TRUE) {

  formula <- "first_twinner ~ 1 + births_total + (1|pop)"

  if (verbose) print(paste0("Fitting model '", formula, "'..."))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = stats::binomial(link = "logit"), method = "PQL/L")
  args <- c(args, args_spaMM)
  fit <- do.call(spaMM::fitme, args = args)

  if (verbose) print("done!")

  fit
}


#' @describeIn fit_models fit the model predicting the probability of a birth to result in twins from the total number of births
#' @export
#'
fit_twinning.binomial <- function(mother_level_data, args_spaMM = list(), verbose = TRUE) {

  formula <- "cbind(twin_total, singleton_total) ~ 1 + births_total + (1|pop)"

  if (verbose) print(paste0("Fitting model '", formula, "'..."))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = stats::binomial(link = "logit"), method = "PQL/L")
  args <- c(args, args_spaMM)

  fit <- do.call(spaMM::fitme, args = args)

  if (verbose) print("done!")

  fit
}


#' @describeIn fit_models fit the model predicting the age at first birth from the twinning status and the total number of births
#' @export
#'
fit_AFB <- function(mother_level_data, args_spaMM = list(), verbose = TRUE) {

  formula <- "AFB ~ 1 + twinner * births_total_fac + (1|pop)"

  if (verbose) print(paste0("Fitting model '", formula, "'..."))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = spaMM::negbin(link = "log"), method = "PQL/L")
  args <- c(args, args_spaMM)

  fit <- do.call(spaMM::fitme, args = args)

  if (verbose) print("done!")

  fit
}


#' @describeIn fit_models fit the model predicting the probability of parity progression
#' @export
#'
fit_PP <- function(birth_level_data, poly_order = NA, twin_as.predictor = TRUE, args_spaMM = list(), verbose = TRUE) {

  ## if poly_order is NA, the order will be estimated as the one leading to best fit between 0 and 6:
  if (is.na(poly_order)) {

    possible_orders <- 0L:6L
    if (verbose) print("The polynomial order for the model has not been set and will thus been estimated through the refitting of the same models for different polynomial orders (be patient...)")

    ## we recall the function trying all polynomial orders between 0 and 6:
    all_fits <- lapply(possible_orders, function(order) {
      fit_order_0 <- fit_PP(birth_level_data = birth_level_data,
             poly_order = order,
             twin_as.predictor = twin_as.predictor,
             args_spaMM = args_spaMM,
             verbose = verbose)
    })

    ## we extract the marginal AIC for all fits:
    all_AICs <- lapply(all_fits, function(fit) spaMM::AIC.HLfit(fit, also_cAIC = FALSE, verbose = FALSE)[1])

    ## we identify the best fit and return the corresponding model:
    best_fit <- which.min(all_AICs)
    return(all_fits[best_fit][[1]])
  }

  if (poly_order > 0 && any(is.na(birth_level_data$age))) {
    birth_level_data <- birth_level_data[!is.na(birth_level_data$age), ]
    warning("the data contains missing values for age, so such rows have not been fitted")
  }

  if (poly_order > 0 && any(is.na(birth_level_data$parity))) {
    birth_level_data <- birth_level_data[!is.na(birth_level_data$parity), ]
    warning("the data contains missing values for parity, so such rows have not been fitted")
  }

  if (poly_order == 0) {
    formula <- "PP ~ 1 + (1|maternal_id) + (1|pop)"
  } else {
    formula <- paste0("PP ~ 1 + poly(cbind(age, parity), ", poly_order, ") + (1|maternal_id) + (1|pop)")
  }

  if (twin_as.predictor) formula <- paste(formula, " + twin")

  if (verbose) {
    if (poly_order > 1L) {
      print(paste0("Fitting model '", formula, "'... (be patient)"))
    } else {
      print(paste0("Fitting model '", formula, "'..."))
    }
  }

  args <- list(formula = stats::as.formula(formula), data = birth_level_data, family = stats::binomial(link = "logit"), method = "PQL/L")
  args <- c(args, args_spaMM)

  fit <- do.call(spaMM::fitme, args = args)

  if (verbose) print("done!")

  fit
}


#' @describeIn fit_models fit the model predicting the duration of the interbirth interval (minus 6 months)
#' @export
#'
fit_IBI <- function(birth_level_data, poly_order = NA, twin_as.predictor = TRUE, args_spaMM = list(), verbose = TRUE) {


  ## if poly_order is NA, the order will be estimated as the one leading to best fit between 0 and 6:
  if (is.na(poly_order)) {

    possible_orders <- 0L:6L
    if (verbose) print("The polynomial order for the model has not been set and will thus been estimated through the refitting of the same models for different polynomial orders (be patient...)")

    ## we recall the function trying all polynomial orders between 0 and 6:
    all_fits <- lapply(possible_orders, function(order) {
      fit_order_0 <- fit_IBI(birth_level_data = birth_level_data,
                             poly_order = order,
                             twin_as.predictor = twin_as.predictor,
                             args_spaMM = args_spaMM,
                             verbose = verbose)
    })

    ## we extract the marginal AIC for all fits:
    all_AICs <- lapply(all_fits, function(fit) spaMM::AIC.HLfit(fit, also_cAIC = FALSE, verbose = FALSE)[1])

    ## we identify the best fit and return the corresponding model:
    best_fit <- which.min(all_AICs)
    return(all_fits[best_fit][[1]])
  }

  if (poly_order > 0 && any(is.na(birth_level_data$age))) {
    birth_level_data <- birth_level_data[!is.na(birth_level_data$age), ]
    warning("the data contains missing values for age, so such rows have not been fitted")
  }

  if (poly_order > 0 && any(is.na(birth_level_data$parity))) {
    birth_level_data <- birth_level_data[!is.na(birth_level_data$parity), ]
    warning("the data contains missing values for parity, so such rows have not been fitted")
  }

  if (poly_order == 0) {
    formula <- "IBI ~ 1 + (1|maternal_id) + (1|pop)"
  } else {
    formula <- paste0("IBI ~ 1 + poly(cbind(age, parity), ", poly_order, ") + (1|maternal_id) + (1|pop)")
  }

  if (twin_as.predictor) formula <- paste(formula, " + twin")


  if (verbose) {
    if (poly_order > 1L) {
      print(paste0("Fitting model '", formula, "'... (be patient)"))
    } else {
      print(paste0("Fitting model '", formula, "'..."))
    }
  }

  ## We remove 6 months to the IBI in order to avoid numerical issues during bootstraps and simulation.
  ## The idea is that no interbirth interval should ever be predicted to be lower than 6 months and
  ## the model will actually predict the duration between those 6 months and the next birth.
  ## Importantly, this implies that 6 months should always be added to predictions from such models.
  ## Since as.integer() does floor when rounding and since a real double can be just its integer value,
  ## the correct way to remove 6 months is to actually remove a little less:
  birth_level_data$IBI <- as.integer(birth_level_data$IBI - 5.5)

  args <- list(formula = stats::as.formula(formula), data = birth_level_data, family = spaMM::negbin(link = "log"), method = "PQL/L")
  args <- c(args, args_spaMM)

  fit <- do.call(spaMM::fitme, args = args)

  if (verbose) print("done!")

  fit
}


#' @describeIn fit_models fit the model predicting the probability of twinning for a given birth event
#' @export
#'
fit_twinning.binary <- function(birth_level_data, poly_order = NA, maternal_ID_as.predictor = TRUE, args_spaMM = list(), verbose = TRUE) {

  ## if poly_order is NA, the order will be estimated as the one leading to best fit between 0 and 6:
  if (is.na(poly_order)) {

    possible_orders <- 0L:6L
    if (verbose) print("The polynomial order for the model has not been set and will thus been estimated through the refitting of the same models for different polynomial orders (be patient...)")

    ## we recall the function trying all polynomial orders between 0 and 6:
    all_fits <- lapply(possible_orders, function(order) {
      fit_order_0 <- fit_twinning.binary(birth_level_data = birth_level_data,
                                         poly_order = order,
                                         maternal_ID_as.predictor = maternal_ID_as.predictor,
                                         args_spaMM = args_spaMM,
                                         verbose = verbose)
    })

    ## we extract the marginal AIC for all fits:
    all_AICs <- lapply(all_fits, function(fit) spaMM::AIC.HLfit(fit, also_cAIC = FALSE, verbose = FALSE)[1])

    ## we identify the best fit and return the corresponding model:
    best_fit <- which.min(all_AICs)
    return(all_fits[best_fit][[1]])
  }

  if (poly_order > 0 && any(is.na(birth_level_data$age))) {
    birth_level_data <- birth_level_data[!is.na(birth_level_data$age), ]
    warning("the data contains missing values for age, so such rows have not been fitted")
  }

  if (poly_order > 0 && any(is.na(birth_level_data$parity))) {
    birth_level_data <- birth_level_data[!is.na(birth_level_data$parity), ]
    warning("the data contains missing values for parity, so such rows have not been fitted")
  }

  if (poly_order == 0) {
    formula <- "twin ~ 1 + (1|pop)"
  } else {
    formula <- paste0("twin ~ 1 + poly(cbind(age, parity), ", poly_order, ") + (1|pop)")
  }

  if (maternal_ID_as.predictor) formula <- paste(formula, " + (1|maternal_id)")

  if (verbose) {
    if (maternal_ID_as.predictor) {
      print(paste0("Fitting model '", formula, "'... (be patient)"))
    } else {
      print(paste0("Fitting model '", formula, "'..."))
    }
  }

  args <- list(formula = stats::as.formula(formula), data = birth_level_data, family = stats::binomial(link = "logit"), method = "PQL/L")
  args <- c(args, args_spaMM)

  fit <- do.call(spaMM::fitme, args = args)

  if (verbose) print("done!")

  fit
}
