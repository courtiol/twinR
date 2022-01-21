#' Simulate slopes under a given scenario
#'
#' This functions allows for the production of slopes of interest (the one quantifying effect of
#' total_birth on the log odd of twinning) taking a simulation scenario as true. This thus
#' corresponds to the computation of the slope under the null hypothesis.
#' There is no reason to use this function directly. It is called by [`simulate_slopes_for_GOF`].
#'
#' @inheritParams run_simulation
#' @inheritParams fit_models
#' @param N_replicates the number of simulation replicates to run
#' @param .log a boolean indicating whether or not to write progress in a log file (for debugging purposes)
#' @param .log_file the name of the log file (with extension)
#' @seealso [`simulate_slopes_for_GOF`]
#'
#' @return a list containing the simulated slopes, the scenario, the input seed, and the time elapsed to do the job
#' @export
#' @examples
#' ## See ?twinR
#'
simulate_slopes <- function(birth_level_data, scenario, life_history_fits = NULL, seed = 123L,
                            N_replicates = 49L,
                            timeout = Inf, verbose = list(fit = FALSE, simu = FALSE),
                            .log = FALSE, .log_file = "log.txt") {

  ## start stopwatch:
  time_begin <- Sys.time()

  ## run simulation based on models fitted on observed data:
  if (.log) cat("start simulation level 1", scenario, seed, Sys.time(), "\n", sep = ";", file = .log_file, append = TRUE)
  simu_level1 <- run_simulation(birth_level_data = birth_level_data,
                                scenario = scenario,
                                life_history_fits = life_history_fits,
                                seed = seed,
                                output = list(birth_level_data.simulated = TRUE, slope = TRUE, fits = FALSE),
                                timeout = timeout,
                                verbose = verbose)
  if (.log) cat("end simulation level 1", scenario, seed, Sys.time(), "\n", sep = ";", file = .log_file, append = TRUE)


  ## remove first set of models to save memory:
  rm(life_history_fits)

  ## refit the life history models on the simulated data:
  if (.log) cat("start fit life history (in simulate_slopes)", scenario, seed, Sys.time(), "\n", sep = ";", file = .log_file, append = TRUE)
  life_history_fits.simulated <- fit_life_histories(scenario = scenario,
                                                    birth_level_data = simu_level1$birth_level_data,
                                                    timeout = timeout,
                                                    verbose = verbose$fit)
  if (.log) cat("end fit life history (in simulate_slopes)", scenario, seed, Sys.time(), "\n", sep = ";", file = .log_file, append = TRUE)


  ## run N_replicates simulations based on models fitted on simulated data and extract slopes:
  if (.log) cat("start simulation level 2", scenario, seed, Sys.time(), "\n", sep = ";", file = .log_file, append = TRUE)
  slopes_level2 <- sapply(seq_len(N_replicates), function(i) {
    simu <- run_simulation(birth_level_data = simu_level1$birth_level_data,
                           scenario = scenario,
                           life_history_fits = life_history_fits.simulated,
                           seed = i,
                           output = list(birth_level_data.simulated = FALSE, slope = TRUE, fits = FALSE),
                           verbose = verbose)
    simu$slope})
  if (.log) cat("end simulation level 2", scenario, seed, Sys.time(), "\n", sep = ";", file = .log_file, append = TRUE)



  ## extract polynomial orders:
  polys <- as.numeric(lapply(life_history_fits.simulated[1:3], extract_poly.order))

  ## stop stopwatch:
  time_end <- Sys.time()

  ## return:
  list_output <- list(slopes_level1 = simu_level1$slope, slopes_level2 = slopes_level2, scenario = scenario, seed = seed,
                      poly_order_PP = polys[1], poly_order_IBI = polys[2], poly_order_twin = polys[3],
                      time_elapsed = as.numeric(time_end - time_begin, units = "secs"))

  return(list_output)
}



#' Simulate slopes for the goodness of fit test
#'
#' This function creates the data required to apply a goodness of fit test (see
#' [`goodness_of_fit`]). It it the function of this package that is the most computationally
#' demanding. Depending on the number of cores CPU you use, the function call may lead to many days
#' of computation time or a large memory requirement.
#'
#' The function performs the double bootstrapping procedure and proceeds as follow:
#' 1. if `life_history_fits` is not provided, the function starts by fitting the life history
#' models (see [`fit_life_histories`]) on the input data (i.e. `birth_level_data` which unless
#' studying the robustness of the function, should be the observed data).
#' 2. the function then run a first series of simulation using the models fitted in step 1 (or the
#' set provided as an input using the argument `life_history_fits`) by calling [`simulate_slopes`]
#' `N_replicates_level1` times.
#' 3. on each simulation outcome, a new set of life history models is fitted (using again
#' [`fit_life_histories`]). This is the step that is computationally intensive.
#' 4. the function then run a second series of simulation using the models fitted during step 3 by
#' calling [`simulate_slopes`] `N_replicates_level2` times.
#'
#' For details on the implementation used for the parallel computing and its settings, see
#' [`test_parallel_computation`].
#'
#'
#' @param N_replicates_level1  the number of simulation replicates to run at the first level (see
#'   [`simulate_slopes`])
#' @param N_replicates_level2  the number of simulation replicates to run at the second level  (see
#'   [`simulate_slopes`])
#' @param seed a seed for the random number generator that will be added to the seeds for individual simulations; with such seeds corresponding to `1:N_replicates_level1`
#' @inheritParams simulate_slopes
#' @inheritParams test_parallel_computation
#'
#' @seealso [`simulate_slopes`], [`goodness_of_fit`], [`test_parallel_computation`]
#' @return a tibble containing all the results
#' @export
#' @examples
#' #See ?twinR
#'
simulate_slopes_for_GOF <- function(N_replicates_level1 = 200L,
                                    N_replicates_level2 = 49L,
                                    birth_level_data,
                                    scenario,
                                    life_history_fits = NULL,
                                    nb_cores = 2L,
                                    lapply_pkg = "pbmcapply",
                                    seed = 1L,
                                    timeout = Inf,
                                    verbose = list(fit = FALSE, simu = FALSE),
                                    .log = FALSE) {

  ## define log file using time stamp (whether it will be used or not):
  log_file <- paste0("log_", round(as.numeric(Sys.time())), ".txt")

  ## fit the life history model on the observed data, if not provided:
  if (is.null(life_history_fits)) {

    if (interactive()) print("Fit life history models on input data...")

    life_history_fits <- fit_life_histories(scenario,
                                            birth_level_data = birth_level_data,
                                            timeout = timeout,
                                            verbose = verbose$fit)
  }

  ## capture options of package spaMM:
  spaMM_options <- spaMM::spaMM.options()


  ## selecting function for lapply:
  if (nb_cores > 1L && lapply_pkg == "base") message("using the 'base' package does not allow for parallel computing; only 1 CPU core will be used and that means it will take a lot of time (perhaps weeks) to run till completion...")

  if (lapply_pkg == "pbmcapply" && !requireNamespace("pbmcapply", quietly = TRUE)) {
    message("to run parallel computing using the package {pbmcapply} you need to install this package; since you did not, {parallel} will be used instead.")
    lapply_pkg <- "parallel"
  }

  lapply_fn <- switch(lapply_pkg,
                      parallel = function(...) parallel::mclapply(..., mc.cores = nb_cores, mc.preschedule = FALSE),
                      pbmcapply = function(...) pbmcapply::pbmclapply(..., mc.cores = nb_cores, mc.preschedule = FALSE, mc.style = "txt", mc.substyle = 3),
                      base = function(...) lapply(...)
                      )

  ## run the job:
  if (interactive()) print("Double bootstraping procedure in progress... Note: this step is computationally demanding and if it requires more RAM than what is available on your system, it will crash your R session.")

  job <- lapply_fn(seq_len(N_replicates_level1), function(it) {

    ## display progress: (results is not ordered so now automatically handled with pbmcapply if selected)
    #cat("\r", "work in progress...  (iteration ", it, "/", N_replicates_level1, ")", sep = "")
    #utils::flush.console()

    ## activate spaMM options in each child node:
    spaMM::spaMM.options(spaMM_options, warn = FALSE)

    ## simulate slopes under a scenario:
    simu <- simulate_slopes(birth_level_data = birth_level_data,
                            scenario = scenario,
                            life_history_fits = life_history_fits,
                            seed = seed + -1L + it,
                            N_replicates = N_replicates_level2,
                            timeout = timeout,
                            verbose = verbose,
                            .log = .log,
                            .log_file = log_file)

    ## format the output as tibble:
    tibble::as_tibble(simu)

    })

  ## combine all outputs into a single tibble:
  if (interactive()) print("Processing the output...")
  job <- do.call(rbind, job)

  ## add the slope measured on the observed data and sort the output:
  job %>%
    dplyr::mutate(slope_observed = compute_slope_from_birth.level.data(birth_level_data), .before = 1L) %>%
    dplyr::arrange(.data$scenario, .data$seed, .data$slopes_level1, .data$slopes_level2)
}



#' Combine the simulated slopes across all scenarios
#'
#' @param path_slopes the path toward the folder where the objects containing the slopes have been
#'   saved
#'
#' @return a tibble with all simulated slopes
#' @export
#'
#' @examples
#' #See ?twinR
#'
combine_simulated_slopes <- function(path_slopes = "slopes_under_scenarios") {
  slopes_files <- normalizePath(list.files(path_slopes, full.names = TRUE), mustWork = TRUE)
  slopes_files <- slopes_files[grepl(pattern = "*.rda", x = slopes_files)]
  if (length(slopes_files) == 0L) stop("folder seems missing or empty; please check the argument 'path_slopes'")
  cat("combining outputs from files:\n")
  env_slopes <- new.env()
  for (file in slopes_files) {
    cat("- ", file, "\n")
    load(file, envir = env_slopes)
  }
  do.call("rbind", lapply(ls(envir = env_slopes), function(x) get(x, envir = env_slopes)))
}


#' Perform the goodness of fit test of simulation scenario(s)
#'
#' This functions perform the goodness of fit test on the data created by the functions
#' [`simulate_slopes_for_GOF`] or [`combine_simulated_slopes`]. The justification and formal
#' description of the test are given in the Appendix S1 of our Supplementary Material.
#'
#' @param slopes_obj an object returned by [`simulate_slopes_for_GOF`] or [`combine_simulated_slopes`]
#'
#' @return a tibble with the information about the scenario and the computed p-values
#' @export
#' @seealso [`simulate_slopes_for_GOF`], [`combine_simulated_slopes`]
#' @examples
#' # See ?twinR
#'
goodness_of_fit <- function(slopes_obj) {

  ## recursive call if the list contains multiple scenarios:
  if (length(unique(slopes_obj$scenario)) > 1L) {
    slopes_obj %>%
      dplyr::mutate(scenario = factor(.data$scenario, levels = c("P", "I", "S", "H", "PI", "PS", "PH", "IS", "IH", "SH", "PIS", "PIH", "PSH", "ISH", "PISH", "base_model"))) %>%
      dplyr::group_split(.data$scenario) -> list_slopes_obj

    all_gof <- lapply(list_slopes_obj, goodness_of_fit)
    return(do.call("rbind", all_gof))
  }

  ## we do not need the individual slopes for the 2nd level of bootstrapping but only their means,
  ## so we aggregate the data:
  slopes_obj %>%
    dplyr::group_by(.data$scenario, .data$seed) %>%
    dplyr::summarize(
      slope_observed = unique(.data$slope_observed),
      slopes_level1 = unique(.data$slopes_level1),
      mean_slopes_level2 = mean(.data$slopes_level2)) %>%
    dplyr::ungroup() -> slopes_obj_aggregated


  ## fit a model predicting slopes of first bootstrap based on the slopes at second bootstrap:
  fit <- stats::lm(slopes_level1 ~ mean_slopes_level2, data = slopes_obj_aggregated)

  ## compute residuals
  var_error <- summary(fit)$sigma
  residuals_slopes_level1 <- var_error*stats::rstudent(fit) # gives similar results than simply using residuals(fit) but more theoretically sound

  ## predict bias introduced by bootstrapping (based on the difference between the two levels):
  mean_slopes_level1 <- mean(slopes_obj_aggregated$slopes_level1)
  bootstrap_bias <- stats::predict(fit, newdata = data.frame(mean_slopes_level2 = mean_slopes_level1))[[1]]

  ## remove bias to observed slope:
  slope_observed <- unique(slopes_obj_aggregated$slope_observed)
  slope_observed_unbiased <- slope_observed - bootstrap_bias

  ## compute p-value (unilateral -> following the idea of the flower plot: we aim at predicting a negative slope, so scenario producing large positive plots should be rejected)
  pv_gof <- (1 + sum(residuals_slopes_level1 < slope_observed_unbiased)) / (1 + nrow(slopes_obj_aggregated))

  ## raw computation of p-value based on single bootstrap only (for comparison):
  pv_raw <- (1 + sum(slopes_obj_aggregated$slopes_level1 < slope_observed)) / (1 + nrow(slopes_obj_aggregated))

  ## output:
  tibble::tibble(scenario = as.character(unique(slopes_obj_aggregated$scenario)), pv_gof = pv_gof, pv_raw = pv_raw)

}


#' Extract computing time from the goodness-of-fit analysis
#'
#' This functions reads and summarizes the information about computing time contains in the objects created with [`simulate_slopes_for_GOF`] and [`combine_simulated_slopes`].
#'
#' @inheritParams goodness_of_fit
#'
#' @return a tibble with the information about the computing time in hours
#' @export
#' @seealso [`simulate_slopes_for_GOF`], [`combine_simulated_slopes`]
#' @examples
#' # See ?twinR
#'
computing_time_analysis <- function(slopes_obj) {
 slopes_obj %>%
   dplyr::group_by(.data$scenario) %>%
   dplyr::summarise(time = unique(.data$time_elapsed)) %>%
   dplyr::ungroup() %>%
   dplyr::summarise(time_hours = sum(.data$time)/3600)
}
