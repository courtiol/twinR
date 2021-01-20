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
#' [`mclapply`][`parallel::mclapply`] outperformed all these alternatives. Using this function
#' introduces some restrictions: you must run it under a Unix based system and it is best to run it
#' directly in a terminal (as opposed within R-GUI or RStudio). Yet, it does combine keys features
#' suiting our purpose:
#' - no time seems wasted doing heavy handed communication between tasks
#' - the handling of the memory is best: objects that can be shared between tasks are indeed shared
#' and when a job is done, the memory is immediately released
#' - it does not require any additional package.
#'
#' Yet, since it is a little difficult to display progress properly using
#' [`mclapply`][`parallel::mclapply`], we used a small wrapper around it provided by
#' [`pbmclapply`][`pbmcapply::pbmclapply`]. You can alternate between these two implementations by
#' setting the argument `lapply_pkg` to either `"parallel"` or `"pbmcapply"`. To use the function
#' sequentially you can also set the argument `lapply_pkg` to  `"base"`. This latter possibility
#' will run fine under Windows, but won't perform parallel computing.
#'
#' @param iter the number of iteration to perform
#' @param nb_cores the number of CPU core(s) to use
#' @param list a list which length will be measured (optional)
#' @param cost the cost for the time threshold (default = 1; increase to speed up test, decrease to
#'   lengthen it)
#' @param lapply_pkg the R package used to implement a `lapply()` kind of function (default =
#'   "pbmcapply"; other possibilities are "parallel" and "base")
#' @export
#' @examples
#' ## sequential version, for reference:
#' test_parallel_computation(iter = 4L, nb_cores = 2L, lapply_pkg = "base")
#'
#' ## parallel version, using the R package parallel:
#' test_parallel_computation(iter = 4L, nb_cores = 2L, lapply_pkg = "parallel")
#'
#' ## parallel version, using the R package pbmcapply (if available):
#' ## same with progression bar if pkg pbmcapply installed:
#' if (requireNamespace("pbmcapply", quietly = TRUE)){
#'   test_parallel_computation(iter = 4L, nb_cores = 2L)
#' }
#'
test_parallel_computation <- function(iter = 20L,
                                      nb_cores = 1L,
                                      list = NULL,
                                      cost = 1,
                                      lapply_pkg = "pbmcapply") {

  if (cost < 0) stop("the argument cost must be positive")

  ## start stopwatch for whole job:
  time_begin <- Sys.time()

  ## selecting function for lapply:
  if (nb_cores > 1L && lapply_pkg == "base") message("using the 'base' package does not allow for parallel computing; only 1 CPU core will be used.")

  if (lapply_pkg == "pbmcapply" && !requireNamespace("pbmcapply", quietly = TRUE)) {
    message("to run parallel computing using the package {pbmcapply} you need to install this package; since you did not, {parallel} will be used instead.")
    lapply_pkg <- "parallel"
  }

  lapply_fn <- switch(lapply_pkg,
                      parallel = function(...) parallel::mclapply(..., mc.cores = nb_cores, mc.preschedule = FALSE),
                      pbmcapply = function(...) pbmcapply::pbmclapply(..., mc.cores = nb_cores, mc.preschedule = FALSE, mc.style = "txt", mc.substyle = 3),
                      base = function(...) lapply(...)
                      )

  job <- lapply_fn(seq_len(iter), function(it) {

    ## start stopwatch for iteration:
    time_begin <- Sys.time()

    ## the job:
    while(difftime(Sys.time(), time_begin, units = "secs") < it/cost){
    }

    ## measuring the length of the large list:
    length_list <- length(list)

    ## measure time of iteration:
    time_end <- Sys.time()
    time_elapsed <- as.numeric(time_end - time_begin, units = "secs")

    ## output of the iteration:
    c(threshold = it/cost, length_list = length_list, time_elapsed = time_elapsed)

    })

   ## stop stopwatch:
   time_end <- Sys.time()

    ## combine outputs:
   output <- as.data.frame(do.call("rbind", job))

   ## add time elapsed:
   output$time_elapsed_sum <- sum(output$time_elapsed)
   output$time_elapsed_job <- as.numeric(time_end - time_begin, units = "secs")

   output
}

