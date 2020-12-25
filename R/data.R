#' The complete dataset at the level of births
#'
#' This dataset contains the raw data associated with 116,082 birth events.
#' It contains the following variables:
#' - `pop` a factor with 9 levels defining the population
#' - `maternal_id` a factor with 23,281 levels defining the identity of the mothers
#' - `maternal_birthyear` the year when the mother was born
#' - `maternal_age` the age of the mother (in months)
#' - `birth_year` the year when a given birth occurred
#' - `birth_twin` the outcome of the birth in terms of twinning (`TRUE` or `FALSE`)
#' - `monthly` whether the observations were collected at a monthly resolution (`TRUE` or `FALSE`)
#'
#' @name data_births_all
#' @docType data
#' @format a `tibble`
#' @references this study
#' @keywords datasets
#' @examples
#' data_births_all
#'
NULL


#' Filter the full data to retain full records
#'
#' This function only keeps information from mothers with complete life history
#' and for which dates have been recorded at a monthly resolution.
#'
#' @param data the dataset to filter

#' @importFrom rlang .data
#'
#' @return a `tibble`
#' @export
#'
#' @examples
#' filter_data(data_births_all)
#'
filter_data <- function(data) {
  data %>%
    dplyr::filter(.data$monthly) %>%
    dplyr::group_by(.data$maternal_id) %>%
    dplyr::filter(dplyr::across(tidyselect::everything(), ~ !is.na(.x))) %>%
    dplyr::ungroup() %>%
    droplevels()
}
