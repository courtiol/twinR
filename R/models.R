#' Fit the models
#'
#' These functions fit the models used in the paper. They are all wrappers more or less simple
#' ultimately calling the function [`fitme`][`spaMM::fitme`] to fit the (G)LM(M)s we need. They have
#' been programmed to be used with our specific data structure. The functions `fit_PP`, `fit_IBI`
#' and `fit_twinning.binary` are those used to fit the three main life history traits. There are
#' called repeatedly when simulating slopes of interests so to perform the goodness of fit (the most
#' computationally demanding task of this project, see [`simulate_slopes_for_GOF`]). We programmed
#' them specially so that the polynomial order of the term ` poly(cbind(age, parity), order)` can be
#' automatically estimated if `poly_order` is set to `NA`. In such a case, models differing in their
#' polynomial orders will be fitted, starting from an order of 0 till reaching the order defined by
#' `max_order`, unless the `timeout` value is reached. When the later occurs, only the polynomial
#' orders below the critical value are being compared to identify the best order. Otherwise, they
#' are all compared and the model fits leading to the minimal marginal AIC values are retained. To
#' speed up the estimation of parameters, the fitting of more complex models reuse the model fitted
#' with the polynomial order just bellow. For example, when fitting a polynomial order of 3, the fit
#' obtained for a polynomial order of 2 is used to initialized the parameter values to be estimated.
#'
#' See **Functions**, below, for details on which model each function does fit.
#'
#' @name fit_models
#' @param mother_level_data a `tibble` or `data.frame` with mother level data
#' @param birth_level_data a `tibble` or `data.frame` with birth level data (expanded or not)
#' @param poly_order an integer value defining the polynomial order when considering the effect of
#'   age and parity (default = `NA`, find best value between 0 and `max_order`)
#' @param twin_as.predictor whether to include the variable `twin` as a predictor or not in some
#'   models (default = `TRUE`)
#' @param maternal_ID_as.predictor whether to include the variable `maternal_ID` as a random effect
#'   predictor or not in some models (default = `TRUE`)
#' @param max_order if `poly_order = NA`, the argument defines the maximum polynomial order to try
#'   (default = 6L)
#' @param timeout the maximal duration (in seconds) allowed for the fitting procedure (default =
#'   Inf)
#' @param verbose whether to display the formula of the fit during the fitting procedure
#' @param scenario the scenario defining which models to be fitted: e.g. "ABCD", "AC"... (see paper
#'   for explanations)
#' @param .args an internal list used to pass the argument to the function [`fitme`][`spaMM::fitme`]
#' @param .simpler_fit a simpler fit from which to extract fitted parameter values to use as initial
#'   parameter values in more complex fit (optional)
#'
#' @return the fitted model
#' @examples
#' # See ?twinR
#'
NULL


#' @describeIn fit_models fit the model predicting the total number of births per mother from her twinning status
#' @param when_twinner a string of characters indicating if the twinning status is based on all births ('allbirths') or just on the first one ('firstbirth')
#' @param with_age a logical indicating if the fit must be controlled for by mothers' age
#' @export
#'
fit_totalbirths <- function(mother_level_data, when_twinner = "allbirths", with_age = FALSE, timeout = Inf, verbose = TRUE) {

  if (when_twinner == "allbirths") {
    if (with_age) {
      formula <- "births_total ~ 1 + twinner * scale(AFB) + (1|pop)"
    } else {
      formula <- "births_total ~ 1 + twinner + (1|pop)"
    }
    } else if (when_twinner == "firstbirth") {
      if (with_age) {
        formula <- "births_total ~ 1 + first_twinner * scale(AFB) + (1|pop)"
      } else {
        formula <- "births_total ~ 1 + first_twinner + (1|pop)"
      }
    } else stop("argument 'when_twinner' in fit_births.total() must be 'life' or 'firstbirth'")

  if (verbose) print(paste0("Fitting model '", formula, "'..."))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = spaMM::Tnegbin(link = "log"), method = "PQL/L")

  ## fit the model:
  fit_model_safely(timeout = timeout, .args = args)
}


#' @describeIn fit_models fit the model predicting the twinning status of the mother at any birth during her life from her total number of births
#' @export
#'
fit_twinner.allbirths <- function(mother_level_data, with_age = FALSE, timeout = Inf, verbose = TRUE) {

  if (with_age) {
    formula <- "twinner ~ 1 + births_total*scale(AFB) + (1|pop)"
  } else {
    formula <- "twinner ~ 1 + births_total + (1|pop)"
  }

  if (verbose) print(paste0("Fitting model '", formula, "'..."))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = stats::binomial(link = "logit"), method = "PQL/L")

  ## fit the model:
  fit_model_safely(timeout = timeout, .args = args)
}


#' @describeIn fit_models fit the model predicting the twinning status of the mother at first birth from her total number of births
#' @export
#'
fit_twinner.firstbirth <- function(mother_level_data, timeout = Inf, verbose = TRUE) {

  formula <- "first_twinner ~ 1 + births_total + (1|pop)"

  if (verbose) print(paste0("Fitting model '", formula, "'..."))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = stats::binomial(link = "logit"), method = "PQL/L")

  ## fit the model:
  fit_model_safely(timeout = timeout, .args = args)
}


#' @describeIn fit_models fit the model predicting the probability of a birth to result in twins from the total number of births
#' @export
#'
fit_twinning.binomial <- function(mother_level_data, timeout = Inf, verbose = TRUE) {

  formula <- "cbind(twin_total, singleton_total) ~ 1 + births_total + (1|pop)"

  if (verbose) print(paste0("Fitting model '", formula, "'..."))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = stats::binomial(link = "logit"), method = "PQL/L")

  ## fit the model:
  fit_model_safely(timeout = timeout, .args = args)
}


#' @describeIn fit_models fit the model predicting the probability of a birth to result in twins from the total number of births with population considered as a fixed effect
#' @export
#'
fit_twinning.binomial_with_pop_as_fixed <- function(mother_level_data, timeout = Inf, verbose = TRUE) {

  formula <- "cbind(twin_total, singleton_total) ~ 1 + births_total + pop"

  if (verbose) print(paste0("Fitting model '", formula, "'..."))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = stats::binomial(link = "logit"), method = "PQL/L")

  ## fit the model:
  fit_model_safely(timeout = timeout, .args = args)
}


#' @describeIn fit_models fit the model predicting the probability of a birth to result in twins from the total number of births in interaction with the population
#' @export
#'
fit_twinning.binomial_with_pop_interaction <- function(mother_level_data, timeout = Inf, verbose = TRUE) {

  formula <- "cbind(twin_total, singleton_total) ~ 1 + births_total*pop"

  if (verbose) print(paste0("Fitting model '", formula, "'..."))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = stats::binomial(link = "logit"), method = "PQL/L")

  ## fit the model:
  fit_model_safely(timeout = timeout, .args = args)
}


#' @describeIn fit_models fit the model predicting the age at first birth from the twinning status and the total number of births
#' @export
#'
fit_AFB <- function(mother_level_data, timeout = Inf, verbose = TRUE) {

  formula <- "AFB ~ 1 + twinner * births_total_fac + (1|pop)"

  if (verbose) print(paste0("Fitting model '", formula, "'..."))

  args <- list(formula = stats::as.formula(formula), data = mother_level_data, family = spaMM::negbin(link = "log"), method = "PQL/L")

  ## fit the model:
  fit_model_safely(timeout = timeout, .args = args)
}


#' @describeIn fit_models fit the model predicting the probability of parity progression
#'
#' @export
#'
fit_PP <- function(birth_level_data, poly_order = NA, twin_as.predictor = TRUE, max_order = 6L, timeout = Inf, verbose = TRUE, .simpler_fit = NULL) {

  ## if poly_order is NA, the order will be estimated as the one leading to best fit between 0 and 6:
  if (is.na(poly_order)) {

    if (verbose) print("The polynomial order for the model has not been set and will thus been estimated through the refitting of the same models for different polynomial orders (be patient...)")

    all_fits <- list()
    ## we fit the most simple model:
    all_fits[[1]] <- fit_PP(birth_level_data = birth_level_data,
                            poly_order = 0,
                            twin_as.predictor = twin_as.predictor,
                            timeout = Inf, ## not timeout here since we need at least this fit!
                            verbose = verbose,
                            .simpler_fit = NULL)

    ## loop on orders greater than 0:
    for (order in seq_len(max_order)) {

      ## skip more complex models if previous order has time out:
      if (length(all_fits) < order) break()

      ## we recall the function trying all remaining polynomial orders:
      all_fits[[1 + order]] <- fit_PP(birth_level_data = birth_level_data,
                                      poly_order = order,
                                      twin_as.predictor = twin_as.predictor,
                                      timeout = timeout,
                                      verbose = verbose,
                                      .simpler_fit =  all_fits[[order]])
    }

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

  ## extract initial parameter values if simpler fit provided:
  inits <- NULL
    if (!is.null(.simpler_fit)) {
      inits <- spaMM::get_inits_from_fit(.simpler_fit)
    }

  ## prepare the model formula:
  if (twin_as.predictor) {
    formula <- sub(pattern = "1 + ", replacement = "1 + twin + ", x = formula, fixed = TRUE)
  }

  if (verbose) {
    if (poly_order > 1L) {
      print(paste0("Fitting model '", formula, "'... (be patient)"))
    } else {
      print(paste0("Fitting model '", formula, "'..."))
    }
  }

  args <- list(formula = stats::as.formula(formula), data = birth_level_data, family = stats::binomial(link = "logit"), method = "PQL/L", init = inits$init)

  ## fit the model:
  fit_model_safely(timeout = timeout, .args = args)
}


#' @describeIn fit_models fit the model predicting the duration of the interbirth interval (minus 6 months)
#' @export
#'
fit_IBI <- function(birth_level_data, poly_order = NA, twin_as.predictor = TRUE,  max_order = 6L, timeout = Inf, verbose = TRUE, .simpler_fit = NULL) {


  ## if poly_order is NA, the order will be estimated as the one leading to best fit between 0 and 6:
  if (is.na(poly_order)) {

    if (verbose) print("The polynomial order for the model has not been set and will thus been estimated through the refitting of the same models for different polynomial orders (be patient...)")

    all_fits <- list()
    ## we fit the most simple model:
    all_fits[[1]] <- fit_IBI(birth_level_data = birth_level_data,
                             poly_order = 0,
                             twin_as.predictor = twin_as.predictor,
                             timeout = Inf, ## not timeout here since we need at least this fit!
                             verbose = verbose,
                             .simpler_fit = NULL)

    ## loop on orders greater than 0:
    for (order in seq_len(max_order)) {

      ## skip more complex models if previous order has time out:
      if (length(all_fits) < order) break()

      ## we recall the function trying all remaining polynomial orders:
      all_fits[[1 + order]] <- fit_IBI(birth_level_data = birth_level_data,
                                       poly_order = order,
                                       twin_as.predictor = twin_as.predictor,
                                       timeout = timeout,
                                       verbose = verbose,
                                       .simpler_fit =  all_fits[[order]])
    }

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

  ## extract initial parameter values if simpler fit provided:
  inits <- NULL
    if (!is.null(.simpler_fit)) {
      inits <- spaMM::get_inits_from_fit(.simpler_fit)
    }

  ## prepare the model formula:
  if (twin_as.predictor) {
    formula <- sub(pattern = "1 + ", replacement = "1 + twin + ", x = formula, fixed = TRUE)
  }

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

  args <- list(formula = stats::as.formula(formula), data = birth_level_data, family = spaMM::negbin(link = "log"), method = "PQL/L", init = inits$init)

  ## fit the model:
  fit_model_safely(timeout = timeout, .args = args)
}


#' @describeIn fit_models fit the model predicting the probability of twinning for a given birth event
#' @export
#'
fit_twinning.binary <- function(birth_level_data, poly_order = NA, maternal_ID_as.predictor = TRUE, max_order = 6L, timeout = Inf, verbose = TRUE, .simpler_fit = NULL) {

  ## if poly_order is NA, the order will be estimated as the one leading to best fit between 0 and 6:
  if (is.na(poly_order)) {

    if (verbose) print("The polynomial order for the model has not been set and will thus been estimated through the refitting of the same models for different polynomial orders (be patient...)")

    all_fits <- list()
    ## we fit the most simple model:
    all_fits[[1]] <- fit_twinning.binary(birth_level_data = birth_level_data,
                                         poly_order = 0,
                                         maternal_ID_as.predictor = maternal_ID_as.predictor,
                                         timeout = Inf, ## not timeout here since we need at least this fit!
                                         verbose = verbose,
                                         .simpler_fit = NULL)

    ## loop on orders greater than 0:
    for (order in seq_len(max_order)) {

      ## skip more complex models if previous order has time out:
      if (length(all_fits) < order) break()

      ## we recall the function trying all remaining polynomial orders:
      all_fits[[1 + order]] <- fit_twinning.binary(birth_level_data = birth_level_data,
                                                   poly_order = order,
                                                   maternal_ID_as.predictor = maternal_ID_as.predictor,
                                                   timeout = timeout,
                                                   verbose = verbose,
                                                   .simpler_fit =  all_fits[[order]])
    }

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

  ## extract initial parameter values if simpler fit provided:
  inits <- NULL
    if (!is.null(.simpler_fit)) {
      inits <- spaMM::get_inits_from_fit(.simpler_fit)
    }

  ## prepare the model formula:
  if (poly_order == 0) {
    formula <- "twin ~ 1"
  } else {
    formula <- paste0("twin ~ 1 + poly(cbind(age, parity), ", poly_order, ")")
  }

  if (maternal_ID_as.predictor) {
    formula <- paste(formula, " + (1|maternal_id) + (1|pop)")
  } else {
    formula <- paste(formula, " + (1|pop)")
  }

  if (verbose) {
    if (maternal_ID_as.predictor) {
      print(paste0("Fitting model '", formula, "'... (be patient)"))
    } else {
      print(paste0("Fitting model '", formula, "'..."))
    }
  }

  args <- list(formula = stats::as.formula(formula), data = birth_level_data, family = stats::binomial(link = "logit"), method = "PQL/L", init = inits$init)

  ## fit the model:
  fit_model_safely(timeout = timeout, .args = args)
}


#' @describeIn fit_models fit all three life history models according to the simulation scenario
#'
#' This function fits all three life history models according to the simulation scenario provided.
#' The polynomial orders are estimated, which is why each of the three models has to be fitted seven
#' times under its default settings (order 0->6). It also returns the total time elapsed by the
#' function.
#'
#' @export
#'
fit_life_histories <- function(scenario, birth_level_data, max_order = 6L, timeout = Inf, verbose = TRUE) {

  ## expand the data:
  birth_level_data <- expand_data(birth_level_data)

  ## start stopwatch:
  time_begin <- Sys.time()

  ## fit parity progression with or without twin as predictor:
  fit_PP <- fit_PP(birth_level_data = birth_level_data, poly_order = NA, twin_as.predictor = grepl("P", scenario), max_order = max_order, timeout = timeout, verbose = verbose)

  ## fit IBI with or without twin as predictor:
  fit_IBI <- fit_IBI(birth_level_data = birth_level_data, poly_order = NA, twin_as.predictor = grepl("I", scenario), max_order = max_order, timeout = timeout, verbose = verbose)

  ## fit the probability of twinning for a given birth event with or without parity/age as fixed effects, and with and without maternal_id as random effect:
  fit_twinning.binary <- fit_twinning.binary(birth_level_data = birth_level_data,
                                             poly_order = ifelse(grepl("S", scenario), NA, 0L),
                                             maternal_ID_as.predictor = grepl("H", scenario),
                                             max_order = max_order,  timeout = timeout, verbose = verbose)

  ## stop stopwatch:
  time_end <- Sys.time()

  ## compute time elapsed:
  time_elapsed <- as.numeric(time_end - time_begin, units = "secs")

  ## return all 3 fits:
  list(fit_PP = fit_PP, fit_IBI = fit_IBI, fit_twinning.binary = fit_twinning.binary,
       time_elapsed = time_elapsed)
}




#' @describeIn fit_models compute the slope between the total number of births and the per-birth twinning probability from birth level data
#'
#' This function fits the model investigating the relationship between parity and twinning
#' probability using [`fit_twinning.binomial`] and retrieve the slope of interest. For this, it
#' aggregates the birth level data.
#'
#' @export
#'
compute_slope_from_birth.level.data = function(birth_level_data, timeout = Inf, verbose = TRUE) {

  ## expand the data:
  birth_level_data <- expand_data(birth_level_data)

  ## aggregated the data:
  mother_level_data <- aggregate_data(birth_level_data)

  ## fit the model:
  fit_twinning.binomial <- fit_twinning.binomial(mother_level_data = mother_level_data, timeout = timeout, verbose = verbose)

  ## extract and return the slope:
  spaMM::fixef(fit_twinning.binomial)[["births_total"]]
}


#' @describeIn fit_models internal function calling the fitting function from spaMM
#'
#' This internal function allows for the handling of messages and timeout threshold.
#' It is called by all the other fitting functions.
#'
#' @export
#'
fit_model_safely <- function(timeout, .args) {
  ## setting timeout:
  setTimeLimit(elapsed = timeout, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))

  ## fit the model while capturing messages produced by spaMM to prevent various displays (e.g. about testing separation)
  ## and displaying a message in case of fit failing:

  fit <- NULL # initialize fit, in case not created due to abort

  tryCatch(
      sink_messages <- utils::capture.output(fit <- do.call(spaMM::fitme, args = .args), type = "message"),
      error = function(ex) {message("\n The fitting of model(s) has aborted. This is possibly because the fitting time has exceeded the threshold set with 'timeout'. It could also be because you are using the wrong data structure (missing predictor?). For debugging purposes, the argument of the call has been saved in a file called timeout_xxx.rda storing a hidden object .args.")
                            save(.args, file = paste0("timeout_", round(as.numeric(Sys.time())), ".rda")) ## save call if problem (with IBI = IBI - 6)
                            return(NULL)})

  fit
}



#' Internal function for extracting the polynomial order of fitted models
#'
#' This internal function allows for identifying which polynomial order has actually been used in a
#' fit. This is used for debugging purposes only.
#'
#' @param fit a fitted model
#' @export
#'
extract_poly.order <- function(fit) {
  pattern <- ".*(\\d)(?=\\)).*"
  formula <- as.character(formula(fit)[3])
  found <- grepl(pattern = pattern, formula, perl = TRUE)
  ifelse(found, as.numeric(sub(pattern = pattern, replacement = "\\1", formula, perl = TRUE)), 0L)
}
