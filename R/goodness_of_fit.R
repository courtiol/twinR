#' Simulate slopes under a given scenario
#'
#' This functions allows for the production of slopes of interest (the one quantifying effect of
#' total_birth on the log odd of twinning) taking a simulation scenario as true. This thus
#' corresponds to the computation of the slope under the null hypothesis (see Details for more
#' information). There is no reason to use this function directly. It is called by
#' [`simulate_slopes_for_GOF`].
#'
#' @inheritParams run_simulation
#' @param N_replicates the number of simulation replicates to run
#' @seealso simulate_slopes_for_GOF
#'
#' @return a list containing the simulated slopes, the scenario, the input seed, and the time elapsed to do the job
#' @export
#' @examples
#' ## See ?twinR
#'
simulate_slopes <- function(birth_level_data, scenario, life_history_fits = NULL, seed = 123L,
                            N_replicates = 49L,
                            args_spaMM = list(), verbose = list(fit = FALSE, simu = FALSE)) {

  ## start stopwatch:
  time_begin <- Sys.time()

  ## run simulation based on models fitted on observed data:
  simu_level1 <- run_simulation(birth_level_data = birth_level_data,
                                scenario = scenario,
                                life_history_fits = life_history_fits,
                                seed = seed,
                                output = list(birth_level_data.simulated = TRUE, slope = TRUE, fits = FALSE),
                                args_spaMM = args_spaMM,
                                verbose = verbose)

  ## refit the life history models on the simulated data:
  life_history_fits.simulated <- fit_life_histories(scenario = scenario,
                                                    birth_level_data = simu_level1$birth_level_data,
                                                    args_spaMM = args_spaMM,
                                                    verbose = verbose$fit)

  ## run N_replicates simulations based on models fitted on simulated data and extract slopes:
  slopes_level2 <- sapply(seq_len(N_replicates), function (i) {
    simu <- run_simulation(birth_level_data = simu_level1$birth_level_data,
                           scenario = scenario,
                           life_history_fits = life_history_fits.simulated,
                           seed = i,
                           output = list(birth_level_data.simulated = FALSE, slope = TRUE, fits = FALSE),
                           args_spaMM = args_spaMM,
                           verbose = verbose)
    simu$slope})

  ## stop stopwatch:
  time_end <- Sys.time()

  ## return:
  list_output <- list(slopes_level1 = simu_level1$slope, slopes_level2 = slopes_level2, scenario = scenario, seed = seed, time_elapsed = as.numeric(time_end - time_begin, units = "secs"))

  return(list_output)
}



#' Simulate slopes for the goodness of fit test
#'
#' This function creates the data required to apply a goodness of fit test. It it the function of
#' this package that is the most computationally demanding. Depending on the number of cores CPU you
#' use, the function call may lead to many days of computation time or a large memory requirement.
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
#'
#' @param N_replicates_level1  the number of simulation replicates to run at the first level (see
#'   [`simulate_slopes`])
#' @param N_replicates_level2  the number of simulation replicates to run at the second level  (see
#'   [`simulate_slopes`])
#' @inheritParams simulate_slopes
#' @param nb_cores the number of CPU cores to use for parallel computing
#'
#' @seealso simulate_slopes
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
                                    nb_cores = 1L,
                                    seed = 123L,
                                    args_spaMM = list(),
                                    verbose = list(fit = FALSE, simu = FALSE)) {

  ## reduce the number of core to what is necessary (to avoid using extra memory for nothing):
  nb_cores <- min(c(N_replicates_level1, nb_cores))

  ## fit the life history model on the observed data, if not provided:
  if (is.null(life_history_fits)) {

    life_history_fits <- fit_life_histories(scenario,
                                            birth_level_data = birth_level_data,
                                            args_spaMM = args_spaMM,
                                            verbose = verbose$fit)
  }

  ##  setup for parallel computing:
  if (nb_cores > 1L) {
    cl <- do.call(parallel::makeCluster, list(spec = nb_cores))
    doSNOW::registerDoSNOW(cl)
  } else {
    cl <- NULL
  }

  ## initialize the progress bar:
  pb <- utils::txtProgressBar(max = N_replicates_level1, style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  snow_opts <- list(progress = progress)

  ## capture options of package spaMM:
  spaMM_options <- spaMM::spaMM.options()

  ## setup loop:
  it <- NULL # to please R CMD check
  loop <- foreach::foreach(it = seq_len(N_replicates_level1),
                           .inorder = FALSE, # do not constrain order, for speed up
                           .combine = "rbind", .options.snow = snow_opts)

  ## run the job:
  job <- foreach::`%dopar%`(loop, {

    ## activate spaMM options in each child node:
    spaMM::spaMM.options(spaMM_options, warn = FALSE)

    ## simulate slopes under a scenario:
    simu <- simulate_slopes(birth_level_data = birth_level_data,
                            scenario = scenario,
                            life_history_fits = life_history_fits,
                            seed = it,
                            N_replicates = N_replicates_level2,
                            args_spaMM = args_spaMM,
                            verbose = verbose)

    ## format the output as tibble:
    tibble::as_tibble(simu)
  })

  ## add the slope measured on the observed data and sort the output,
  ## note: sorting allows for the production of reproducible results despite using ".inorder = FALSE" in foreach:
  job %>%
    dplyr::mutate(slope_observed = compute_slope_from_birth.level.data(birth_level_data), .before = 1L) %>%
    dplyr::arrange(.data$scenario, .data$seed, .data$slopes_level1, .data$slopes_level2) -> job

  ##  stop parallel computing properly:
  if (nb_cores > 1L) {
    foreach::registerDoSEQ()
    parallel::stopCluster(cl)
  }

  job
}

