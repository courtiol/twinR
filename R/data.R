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
#' @param birth_level_data the dataset to filter

#' @importFrom rlang .data
#'
#' @return a `tibble`
#' @export
#'
#' @examples
#' filter_data(data_births_all)
#'
filter_data <- function(birth_level_data) {
  birth_level_data %>%
    dplyr::filter(.data$monthly) %>%
    dplyr::group_by(.data$maternal_id) %>%
    dplyr::filter(dplyr::across(tidyselect::everything(), ~ !is.na(.x))) %>%
    dplyr::ungroup() %>%
    droplevels()
}


#' Aggregate birth-level data into mother-level data
#'
#' This function aggregates the data so that each row corresponds to a single mother.
#'
#' @param birth_level_data the dataset to aggregate
#'
#' @return a `tibble`
#' @export
#'
#' @examples
#' aggregate_data(data_births_all)
#'
aggregate_data <- function(birth_level_data) {
  birth_level_data %>%
    dplyr::group_by(.data$maternal_id) %>%
    dplyr::summarize(births_total = dplyr::n(),
                     births_total_fac = as.factor(ifelse(.data$births_total < 10, as.character(.data$births_total), "10+")),
                     twinner = any(.data$birth_twin),
                     first_twinner = .data$birth_twin[1],
                     twin_total = sum(.data$birth_twin),
                     singleton_total = sum(!.data$birth_twin),
                     AFB = .data$maternal_age[1]/12) %>%
    dplyr::ungroup()
}

