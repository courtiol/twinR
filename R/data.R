#' The complete dataset at the level of births
#'
#' This dataset contains the raw data associated with 116,082 birth events.
#' It contains the following variables:
#' - `pop` a factor with 9 levels defining the population
#' - `maternal_id` a factor with 23,281 levels defining the identity of the mothers
#' - `maternal_birthyear` the year when the mother was born
#' - `maternal_age` the age of the mother (in months)
#' - `birth_year` the year when a given birth occurred
#' - `twin` the outcome of the birth in terms of twinning (`TRUE` or `FALSE`)
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
    dplyr::filter(dplyr::if_all(tidyselect::everything(), ~ !is.na(.x))) %>%
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
    dplyr::group_by(.data$pop, .data$maternal_id) %>%
    dplyr::summarize(births_total = dplyr::n(),
                     births_total_fac = factor(ifelse(.data$births_total < 10, as.character(.data$births_total), "10+"),
                                               levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10+")),
                     twinner = any(.data$twin),
                     first_twinner = .data$twin[1],
                     twin_total = sum(.data$twin),
                     singleton_total = sum(!.data$twin),
                     AFB = .data$maternal_age[1]) %>%
    dplyr::ungroup()
}


#' Create derived variables for birth-level data
#'
#' This function adds new columns to the birth level data, i.e.:
#' - `age` the age of the mother at each birth (in years)
#' - `parity` the rank of a given birth within the life history of a mother
#' - `PP` whether the mother go on reproducing after a given birth
#' - `IBI` the interbirth interval between a given birth and the next one (in months)
#'
#' These new columns are derived from existing columns and are required for the
#' fit of statistical models.
#'
#' @param birth_level_data the dataset to expand
#'
#' @return a `tibble`
#' @export
#'
#' @examples
#' expand_data(data_births_all)
#'
expand_data <- function(birth_level_data) {

  ## do nothing if data already expanded:
  if (all(c("PP", "IBI", "age", "parity") %in% colnames(birth_level_data))) {
    return(birth_level_data)
  }

  ## perform expansion:
  birth_level_data %>%
    dplyr::mutate(age = .data$maternal_age/12L, .after = .data$maternal_age) %>% ## turn the age in years
    dplyr::group_by(.data$maternal_id) %>%
    dplyr::mutate(parity = 1:dplyr::n(), ## birth rank
                  PP = dplyr::row_number() != dplyr::n(), ## parity progression Boolean
                  IBI = dplyr::lead(.data$maternal_age) - .data$maternal_age) %>%
    dplyr::ungroup()
}

