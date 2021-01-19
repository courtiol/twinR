#' Display welcome message
#'
#' This function should not be called by the user.
#' It displays a message when the package is being loaded.
#'
#' @param libname argument needed but automatically defined
#' @param pkgname argument needed but automatically defined
#'
#' @export
#'
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(## display message
    '\n Welcome to twinR!',
    '\n To start, simply type ?twinR and follow the examples',
    '\n'
  )
}

#' Import the pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL



## export some variables names to please R CMD check

## for tidyselect (due to https://github.com/r-lib/tidyselect/issues/201):
utils::globalVariables("where")

## for magrittr (see function build_fit_summary.table):
utils::globalVariables(".")

## for temporary text connection (see function `#`):
globalVariables("str_temp")

