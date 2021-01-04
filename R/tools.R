#' Show an object as text output
#'
#' This function makes copying and pasting the results of the analyses more straightforward.
#' It adds a pound sign (`#`), or equivalent, before displaying each line of the output.
#'
#' @param result an object to output
#' @param digits the number of digits to display (default = 0)
#' @param prefix the prefix added to each line (the default is roxygen friendly)
#'
#' @export
#'
#' @examples
#' test <- runif(3)
#' test
#' round(test, digits = 2)
#' `#`(test, digits = 2)
#' `#`(test, prefix = "#")
#' `#`(data.frame(x = test, y = test), digits = 2, prefix = "#")
#'
#' \dontrun{
#' ## do not attempt to round a list:
#'   `#`(list(x = test, y = test), digits = 2, prefix = "#")
#' }
#'
#' `#`(list(x = test, y = test), prefix = "#")
#'
`#` <- function(result, digits = 0, prefix = "#' #") {

  if ("list" %in% class(result) && digits != 0) {
    digits <- 0
    warning("list object cannot be rounded with this function. Set 'digits = 0'; i.e. the default.")
  }

  .hash <- function(result, digits, prefix) {
    tc <- textConnection("str_temp", "w", local = TRUE)
    sink(tc)
    if (digits > 0) {
      print(signif(result, digits = digits))
    } else {
      print(result)
    }
    sink()
    close(tc)
    cat(paste0(prefix, str_temp, collapse = "\n"))
    invisible(result)
  }

  ## we create a safe wrapper since the occurrence of an error while running the function would leave
  ## the sink active, and no output would then be visible in the console:
  hash_safe <- function(result, digits, prefix) {
    tryCatch(.hash(result = result, digits = digits, prefix = prefix), error = function(e) {
      sink()
      stop("The function `#` crashed.")
    })
  }

  hash_safe(result = result, digits = digits, prefix = prefix)
}

globalVariables("str_temp")



#' Testing parallel computing
#'
#' This function is aimed at testing if the parallel computation run efficiently and you can use it
#' as a skeleton to benchmark alternative implementations. It simply runs loops that take, in turns,
#' `(1:iter)/cost` time to run. A large object may be supplied to the argument `list` to monitor how
#' the memory is being handled. For example, you may want to provided to `list` the output of the
#' function [`fit_life_histories`].
#'
#' We tried many implementation (using the R packages parallel, furrr, future.apply and foreach;
#' combined with the backends doSnow, doParallel, or doFuture; using either multi-threading or
#' multi-processing). At least on our linux system, the implementation used here simply using
#' [`mclapply`][`parallel::mclapply`] outperformed all these alternatives. The function must run under
#' a Unix based system and directly in a terminal (as opposed within R-GUI or RStudio) but it does
#' combine the different features:
#' - no time seems wasted doing heavy handed communication between tasks
#' - the handling of the memory is best: objects that can be shared between tasks are indeed shared and when a job
#' is done, the memory is immediately released
#' - progress can be displayed (not in GUI)
#'
#' And all of these nice features come without the need for any additional package!
#'
#' @param iter the number of iteration to perform
#' @param nb_cores the number of CPU core(s) to use
#' @param list a list which length will be measured (optional)
#' @param cost the cost for the time threshold (default = 1; increase to speed up test, decrease to lengthen it)
#' @export
#' @examples
#' test_parallel_computation(iter = 4L, nb_cores = 2L)
#'
test_parallel_computation <- function(iter = 20L,
                                      nb_cores = 1L,
                                      list = NULL,
                                      cost = 1) {

  if (cost < 0) stop("the argument cost must be positive")

  ## start stopwatch for whole job:
  time_begin <- Sys.time()

  job <- parallel::mclapply(seq_len(iter), function(it) {

    ## display progress:
    cat("\r", "work in progress...  (iteration ", it, "/", iter, ")", sep = "")
    utils::flush.console()

    ## start stopwatch for iteration:
    time_begin <- Sys.time()

    ## the job:
    while(difftime(Sys.time(), time_begin, units ="secs") < it/cost){
    }

    ## measuring the length of the large list:
    length_list <- length(list)

    ## measure time of iteration:
    time_end <- Sys.time()
    time_elapsed <- as.numeric(time_end - time_begin, units = "secs")

    ## output of the iteration:
    c(threshold = it/cost, length_list = length_list, time_elapsed = time_elapsed)

    }, mc.cores = nb_cores, mc.preschedule = FALSE)

   ## stop stopwatch:
   time_end <- Sys.time()

   ## add newline in console:
   cat("\n")

    ## combine outputs:
   output <- as.data.frame(do.call("rbind", job))

   ## add time elapsed:
   output$time_elapsed_sum <- sum(output$time_elapsed)
   output$time_elapsed_job <- as.numeric(time_end - time_begin, units = "secs")

   output
}

