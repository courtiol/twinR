#' Simulate slopes under a given scenario
#'
#' This functions allows for the production of slopes of interest (the one quantifying effect of
#' total_birth on the log odd of twinning) taking a simulation scenario as true. This thus
#' corresponds to the computation of the slope under the null hypothesis (see Details for more
#' information).
#'
#' The function runs a first simulation according to the life history models fitted on the observed
#' data (level 1). Then, the three life history models are refitted on the simulated data that has
#' been produced and these new fits are used to simulated new data (level 2). The slope of interest
#' is then computed using each of the simulated dataset from level 2.
#'
#' @inheritParams run_simulation
#' @param N_replicates the number of simulation replicates to run
#' @seealso run_simulation
#'
#' @return a list containing the simulated slopes, the input seed, and the time elapsed to do the job
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
  list_output <- list(slope_level1 = simu_level1$slope, slopes_level2 = slopes_level2, seed = seed, time_elapsed = as.numeric(time_end - time_begin, units = "secs"))

  return(list_output)
}

