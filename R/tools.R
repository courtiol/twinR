#' Show an object as text output
#'
#' This function makes copying and pasting the results of the analyses more straightforward.
#' It adds a pound sign (`#`) or equivalent before displaying each line of the output.
#'
#' @param result an object to output
#' @param digits the number of digits to display (default = 2)
#' @param prefix the prefix added to each line (the default is roxygen friendly)
#'
#' @export
#'
#' @examples
#' test <- runif(3)
#' `#`(test)
#' `#`(test, prefix = "#")
#' `#`(data.frame(x = test, y = test), prefix = "#")
#'
#' \dontrun{
#' ## do not attempt to round a list:
#'   `#`(list(x = test, y = test), prefix = "#")
#' }
#'
#' `#`(list(x = test, y = test), digits = 0, prefix = "#")
#'
`#` <- function(result, digits = 2, prefix = "#' #") {

  if ("list" %in% class(result) && digits != 0) {
    digits <- 0
    warning("list object cannot be rounded with this function. Set 'digits = 0'.")
  }

  .hash <- function(result, digits, prefix) {
    tc <- textConnection("str_temp", "w", local = TRUE)
    sink(tc)
    if (digits > 0) {
      print(round(result, digits = digits))
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
