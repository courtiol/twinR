#' R6 class to simulate the life histories of mothers
#'
#' `life_histories` is an [`R6 class`][`R6::R6Class`] which we used to simulate the reproductive
#' life of mothers, according to specific hypotheses defined by the simulation scenarios. The class
#' contains the simulated data, as well as methods (i.e. functions) that can be applied on the R6
#' object after it has been created.
#'
#' We recommend you to look at the raw R code of this class definition on GitHub (file
#' '/R/simulations.R') to understand how simulations work. We commented the code to make this clear.
#' While you could directly look at the code of this class definition while using the package, mind
#' that the comments will have been stripped away during the installation process.
#'
#' What is R6? R6 is an R package that provides a system to define objects in R. Perhaps you have
#' already heard of some of the native systems for creating objects in R. Those are
#' [`S3`][`UseMethod`] (which is by far the dominant way of handling objects in R),
#' [`S4`][`Classes_Details`] (which is heavily used by some specific packages like Matrix and all
#' packages from Bioconductor) and [`ReferenceClasses`] (which is little used).
#'
#' R6 is a system tailored to define classes and methods following a traditional Object Oriented
#' Programming (OOP; as found in C++ & Java), while keeping with a very simple R syntax.
#'
#' We initially chose to use R6 because we thought of coding individual-based simulations as a
#' collection of R6 objects, wherein each object would represent a mother (and would thus be of the
#' same class). In the end, we chose not to do that, since the simulations are simple enough to
#' manipulate data frames, which is more efficient. Yet, the structure offered by R6 is still
#' suitable since it allows to code within a single well structured chunk of code, the entire
#' simulation and data representation. We believe that this should help those who want to understand
#' exactly how we performed our simulations.
#'
#' @name life_histories
#' @export
#' @examples
#' # See ?twinR
#'
life_histories <- R6::R6Class(

   classname = "life_histories",

   public = list(


      ## Slots for the data stored in the class #######################################################

      #'@field birth_level_data a `tibble` containing the birth-level data used to initialize the simulation (provided as input)
      birth_level_data = tibble::tibble(),

      #'@field  fit_PP a fitted model for predicting the probability of parity progression (see [`fit_models`]) (provided as input)
      fit_PP = NULL,

      #'@field  fit_IBI a fitted model for predicting the duration of the interbirth interval (minus 6 months) (see [`fit_models`]) (provided as input)
      fit_IBI  = NULL,

      #'@field  fit_twinning.binary a fitted model for predicting the probability of twinning for a given birth event (see [`fit_models`]) (provided as input)
      fit_twinning.binary = NULL,

      #'@field verbose a `list` of booleans indicating whether or not to display information at various steps (provided as input)
      verbose = list(),

      #'@field iteration the number of the current iteration of the simulation
      iteration = 0L,

      #'@field data_iteration a `tibble` storing the information corresponding to one simulation iteration (generated automatically)
      data_iteration = tibble::tibble(),

      #'@field data_simulated a `tibble` containing the complete simulated dataset (generated automatically)
      data_simulated = tibble::tibble(),

      #'@field birth_level_data.simulated a `tibble` containing the simulated data expressed as birth-level data (generated automatically)
      birth_level_data.simulated = tibble::tibble(),

      #'@field fit_twinning.binomial the fitted model producing the main slope of interest (see [`fit_models`]) (generated automatically)
      fit_twinning.binomial = NA_real_,

      #'@field slope the main slope of interest quantifying effect of total_birth on the log odd of twinning (generated automatically)
      slope = NA_real_,


      ## Slots for the methods stored in the class ####################################################

      ### Constructor function ########################################################################

      #' @description
      #' Create a new object from the class life_histories (constructor)
      #'
      #' This functions is used to import the users' inputs and prepare the object that will store the
      #' outcome of simulations.
      #'
      #' @param fit_PP  fit_PP a fitted model for predicting the probability of parity progression (see [`fit_models`])
      #' @param fit_IBI a fitted model for predicting the duration of the interbirth interval (minus 6 months) (see [`fit_models`])
      #' @param fit_twinning.binary a fitted model for predicting the probability of twinning for a given birth event (see [`fit_models`])
      #' @param birth_level_data a `tibble` containing the birth-level data used to initialize the simulation
      #' @param verbose a `list` of booleans indicating whether or not to display information at various steps
      #'
      #' @return an object of class `life_histories`
      #'
      initialize = function(fit_PP,
                            fit_IBI,
                            fit_twinning.binary,
                            birth_level_data,
                            verbose = list(fit = FALSE, simu = FALSE)) {

         self$birth_level_data <- expand_data(birth_level_data)
         self$fit_twinning.binary <- fit_twinning.binary
         self$fit_PP <- fit_PP
         self$fit_IBI <- fit_IBI
         self$verbose <- verbose
         self$build_initial_state()

      },


      ### Initializing function #######################################################################

      #' @description
      #' Build the initial state for the simulations
      #'
      #' To initialize the population, we use the observed dataset and retain the information of
      #' mothers at their first parity. That way, the age at first birth, their identity and the number
      #' of mothers from each population remains the same as the one from the real dataset. We do not
      #' keep the twinning status of the first birth. This is because we only keep the characteristics
      #' of the mothers that are not influenced by our simulation scenarios. Since our simulation
      #' scenarios influence the probability of twinning at all births, including the first one, we
      #' will thus simulate the twinning status at first birth. This will be done in steps beyond such
      #' building of the initial state of the population.
      #'
      build_initial_state = function() {

         self$birth_level_data %>%
            dplyr::filter(.data$parity == 1L) %>%
            dplyr::select(.data$parity, .data$age, .data$maternal_id, .data$pop) %>%
            dplyr::mutate(age_next = .data$age) -> self$data_iteration
         return(invisible(self))
      },


      ### Running functions ###########################################################################

      #' @description
      #' Simulate an iteration of the life history of the mothers
      #'
      #' This function simulates one iteration of the life history of the mothers.
      #' In the simulation, an iteration corresponds to three specific happenings:
      #'   - the simulation of the twinning status of the current birth based on the probability of twinning for a given birth event (pre-birth twinning probability)
      #'   - the simulation of whether the mother will go on producing a next birth based on the probability for parity progression
      #'   - and, if the mother does go on reproducing, the simulation of the duration of the interbirth interval between the current birth and the next one
      #'
      simulate_one_iteration = function() {

         ## increment the iteration counter:
         self$iteration <- self$iteration + 1L

         ## update the current age of the mothers by `age_next`, which is defined in the previous iteration:
         if (self$iteration > 1L) self$data_iteration$age <- self$data_iteration$age_next

         ## update the current parity of the individuals:
         if (self$iteration > 1L) self$data_iteration$parity <- self$data_iteration$parity + 1L

         ## simulate the twinning status of the current parity:
         sink_messages <- utils::capture.output( # prevents messages from spaMM
           twin <- spaMM::simulate.HLfit(self$fit_twinning.binary,
                                         nsim = 1L,
                                         newdata = self$data_iteration,
                                         type = "residual",
                                         verbose = self$verbose$simu),
           type = "message")

         ## update the current twinning status:
         self$data_iteration$twin <- as.logical(twin)

         ## simulate the parity progression between current and next parity:
         sink_messages <- utils::capture.output( # prevents messages from spaMM
           PP <- spaMM::simulate.HLfit(self$fit_PP,
                                       nsim = 1L,
                                       newdata = self$data_iteration,
                                       type = "residual",
                                       verbose = self$verbose$simu),
         type = "message")

         ## update the parity progression:
         self$data_iteration$PP <- as.logical(PP)

         ## simulate the IBI between current and next parity:
         sink_messages <- utils::capture.output( # prevents messages from spaMM
           IBI_minus6 <- spaMM::simulate.HLfit(self$fit_IBI,
                                               nsim = 1L,
                                               newdata = self$data_iteration,
                                               type = "residual",
                                               verbose = self$verbose$simu),
         type = "message")

         ## Since fit_IBI predicts the IBI from 6 months after the onset of pregnancy, we need to add
         ## back the missing 6 months here. We also add 0.5 to be sure that as.integer()
         ## does not round to the wrong number in case the value predicted
         ## by simulate.HLfit would be a real very close to the integer, but just epsilon underneath it.
         ## Finally, we want to avoid Inf or other very large values which would make the fitting of
         ## simulated data particularly difficult. Such large values may occur for badly fitted
         ## models (when models are fitted on data simulated under an unrealistic scenario as
         ## opposed to when they are fitted on the real dataset), so in such
         ## case we take a very large value for the IBI set to 30 years or 360 months:
         IBI <- as.integer(pmin(IBI_minus6 + 6.5, 360L))

         ## In case of badly fitted models, NA may also be produced by simulate.HLfit when numbers
         ## should be huge, so we also deal with this problem here:
         IBI[is.na(IBI)] <- 360L

         ## individuals that will not go on reproducing cannot have an IBI_next:
         IBI[!self$data_iteration$PP] <- NA_integer_

         ## update IBI_next:
         self$data_iteration$IBI_next <- IBI

         ## update age at next parity from IBI_next:
         self$data_iteration$age_next <- self$data_iteration$age + self$data_iteration$IBI_next/12L #note: the age of mothers is in years, but IBI is expressed in months
         return(invisible(self))
      },

      #' @description
      #' Update the simulated dataset
      #'
      #' This function adds data from a given iteration to the complete dataset (i.e. `data_simulated`).
      #'
      update_data_simulated = function() {

         ## add the data of the current iteration to a copy of `data_simulated` and sort the data:
         self$data_iteration %>%
            dplyr::bind_rows(self$data_simulated) %>%
            dplyr::arrange(.data$pop, .data$maternal_id, .data$parity) -> self$data_simulated
         return(invisible(self))
      },

      #' @description
      #' Filter the simulated dataset
      #'
      #' This function removes the mothers no longer reproducing, so they won't contribute to future
      #' iterations. Such mothers are those for which fit_PP predicts they won't go on reproducing.
      #' Additionally, we put a hard threshold at 60 years so that even under badly fitted models,
      #' the simulation will stop at some point. Some simulated mothers can still reach ages greater
      #' than 60, but no mother will reproduce passed 60.
      filter_data_simulated = function() {

         self$data_iteration %>%
            dplyr::filter(.data$PP,
                          .data$age < 60) -> self$data_iteration

         return(invisible(self))
      },

      #' @description
      #' Run the simulation
      #'
      #' This is the main function used to run the simulation. It calls, in turns,
      #' `simulate_one_iteration()`, `update_data_simulated()` and `filter_data_simulated()`, until
      #' no mother keeps reproducing.
      #'
      run = function() {

         ## prevent spaMM from displaying progression bars when predict.HLfit is called:
         spaMM_options <- spaMM::spaMM.options(barstyle = 0L)

         ## run the iterations:
         while (nrow(self$data_iteration) > 1L) {
            self$simulate_one_iteration()
            self$update_data_simulated()
            self$filter_data_simulated()
         }

         ## format the output:
         self$format_data_outputs()

         ## compute the slope of interest:
         self$slope <- compute_slope_from_birth.level.data(self$birth_level_data.simulated, verbose = self$verbose$fit)

         ## restore spaMM options as defined before launching the simuations:
         spaMM::spaMM.options(spaMM_options)

         return(invisible(self))
      },


      ### Accessory functions #########################################################################

      #' @description
      #' Format the simulated data as original data
      #'
      #' This function turns the simulated data into birth level data and mother level data that are structured as the original data.
      #'
      format_data_outputs = function() {

         ## extract info not simulated from the input data:
         self$birth_level_data %>%
            dplyr::filter(.data$parity == 1L) %>%
            dplyr::select(.data$maternal_id, .data$maternal_birthyear, .data$birth_year) -> original_data

         ## convert the simulated data into birth level data:
         self$data_simulated %>%
            dplyr::mutate(maternal_age = .data$age*12L,
                          monthly = TRUE) %>%
            dplyr::left_join(original_data, by = "maternal_id") %>%
            dplyr::group_by(.data$maternal_id) %>%
            dplyr::mutate(birth_year = c(.data$birth_year[1], .data$birth_year[1] + (.data$maternal_age[-1] - .data$maternal_age[1])/12L)) %>%
            dplyr::ungroup() %>%
            dplyr::select(.data$pop, .data$maternal_id, .data$maternal_birthyear, .data$maternal_age, .data$birth_year, .data$twin, .data$monthly) -> self$birth_level_data.simulated

         return(invisible(self))
      }
   ) ## end the public list created at the very beginning
)


#' Run a simulation
#'
#' This function runs a single simulation. If the fits of the life history models are not provided,
#' they will be fitted within the function call.
#'
#' The output is conditional on the input so to save memory footprint and only return whatever is
#' needed for a given application.
#'
#' @inheritParams fit_models
#' @inheritParams life_histories
#' @inheritParams predictions
#' @param life_history_fits a named list containing the fits of each three life history models
#' @param output a named list of booleans with elements `birth_level_data.simulated`, `slope` and `fit` indicating what to output
#'
#' @return a list with the output requested
#' @export
#' @seealso [`simulate_slopes`]
#' @examples
#' # See ?twinR
#'
run_simulation <- function(birth_level_data, scenario = NULL, life_history_fits = NULL, seed = 123L,
                           output = list(birth_level_data.simulated = FALSE, slope = TRUE, fits = FALSE),
                           timeout = Inf,
                           verbose = list(fit = FALSE, simu = FALSE)) {

   ## start stopwatch:
   time_begin <- Sys.time()

   ## fit the life history models, if not provided:
   if (is.null(life_history_fits)) {

      life_history_fits <- fit_life_histories(scenario,
                                              birth_level_data = birth_level_data,
                                              timeout = timeout,
                                              verbose = verbose$fit)
   }

   ## fix the seed of the random generator for reproducibility:
   set.seed(seed)

   ## call constructor of R6 object for simulations:
   simu <- life_histories$new(fit_PP = life_history_fits$fit_PP,
                              fit_IBI = life_history_fits$fit_IBI,
                              fit_twinning.binary = life_history_fits$fit_twinning.binary,
                              birth_level_data = birth_level_data,
                              verbose = verbose)

   ## run the simulation:
   simu$run()

   ## build the output according to what is needed:
   list_output <- list()
   if (output$birth_level_data.simulated) list_output[["birth_level_data.simulated"]] <- simu$birth_level_data.simulated
   if (output$slope) list_output[["slope"]] <- simu$slope
   if (output$fits) list_output[["fits"]] <- life_history_fits

   ## stop stopwatch:
   time_end <- Sys.time()

   ## compute time elapsed:
   list_output[["time_elapsed"]] <- as.numeric(time_end - time_begin, units = "secs")

   ## add scenario info:
   list_output[["scenario"]] <- scenario

   return(list_output)
}



#' Simulate data to study the reproductive life of mothers
#'
#' This function simulates the life history of mothers and extract information about simulated
#' individuals. The argument `effect_pt` can be used to simulate an increased (or reduced) twinning
#' propensity.
#'
#' @inheritParams simulate_slopes
#' @inheritParams simulate_slopes_for_GOF
#' @param effect_pt a numerical value defining by how much the intercept of `fit_pt` has to be
#'   increased (on the scale of the linear predictor)
#'
#' @return a tibble containing information about the reproductive life of the simulated mothers
#' @export
#' @examples
#' ## See ?twinR
#'
simulate_reproduction <- function(birth_level_data, scenario, life_history_fits = NULL,
                                  effect_pt = 0,
                                  N_replicates = 100L, seed = 1L,
                                  nb_cores = 2L, lapply_pkg = "pbmcapply") {


  ## modifying the fitted intercept of the model pt:
  life_history_fits$fit_twinning.binary$fixef[1] <- life_history_fits$fit_twinning.binary$fixef[1] + effect_pt

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
  if (interactive()) print("Simulations in progress...")

  job <- lapply_fn(seq_len(N_replicates), function(it) {

    ## run simulation based on models fitted on observed data:
    simu <- run_simulation(birth_level_data = birth_level_data,
                           scenario = scenario,
                           life_history_fits = life_history_fits,
                           seed = seed + -1L + it,
                           output = list(birth_level_data.simulated = TRUE, slope = FALSE, fits = FALSE),
                           timeout = Inf,
                           verbose =  list(fit = FALSE, simu = FALSE))

    simu$birth_level_data.simulated %>%
       dplyr::group_by(.data$maternal_id) %>%
       dplyr::summarize(twinner = any(.data$twin),
                        twin_births = sum(.data$twin),
                        all_births = dplyr::n(),
                        twins = sum(.data$twin_births * 2L), ## note: we don't simulate triplets and so forth
                        singletons = sum(!.data$twin),
                        offsprings = .data$twins + .data$singletons,
                        offsprings_Helle_et_al = .data$twins*0.603 + ifelse(.data$twinner, .data$singletons*0.838, .data$singletons*0.815), # surv prob from Helle et al. 2004 (18yrs) Fig. 3
                        offsprings_Haukioja_et_al = .data$twins*0.337 + .data$singletons*0.706) %>% # surv prob from Haukioja et al. 1988 (15yrs) Table 1
       dplyr::summarize(twinning_rate = sum(.data$twin_births)/sum(.data$all_births),
                        twinner_rate = mean(.data$twinner),
                        total_offsprings = mean(.data$offsprings),
                        total_offsprings_Helle_et_al = mean(.data$offsprings_Helle_et_al),
                        total_offsprings_Haukioja_et_al = mean(.data$offsprings_Haukioja_et_al),
                        total_births = mean(.data$all_births)) %>%
       dplyr::bind_cols(seed = seed + -1L + it)

  })

  ## combine all outputs into a single tibble:
  if (interactive()) print("Processing the output...")
  do.call(rbind, job)

}
