
#' Create data summary tables
#'
#' This function is used to create the tables providing the details of the data used in the study.
#'
#' @param birth_level_data the dataset to summarize
#'
#' @return a `tibble`
#' @export
#'
#' @examples
#' build_summary_table(data_births_all)
#'
build_summary_table <- function(birth_level_data) {

  ## add total births and twinning status to the data:
  birth_level_data %>%
    dplyr::group_by(.data$pop, .data$maternal_id) %>%
    dplyr::mutate(total_births = dplyr::n(),
                  twinners     = any(.data$birth_twin),
                  nontwinners  = all(!.data$birth_twin)) %>%
    dplyr::ungroup() -> data

  ## notes for ASCII encoding of unicodes:
  # - a umlaut  = \u00e4
  # - o umlaut  = \u00f6
  # - o with stroke = \u00f8
  # - oe        = \u0153
  # - per mille = \u2030

  ## build the part of the table that contains information for each population:
  data %>%
    dplyr::group_by(.data$pop) %>%
    dplyr::summarise(
      "Maternal birth period" = paste(min(floor(.data$maternal_birthyear), na.rm = TRUE), "-", max(floor(.data$maternal_birthyear), na.rm = TRUE), sep = ""),
      "Mothers" = length(unique(.data$maternal_id)),
      "Non-twinners" = length(unique(.data$maternal_id[.data$nontwinners])),
      "Twinners" = length(unique(.data$maternal_id[.data$twinners])),
      "Twinner rate (\u2030)" = round(.data$Twinners / (.data$Twinners + .data$`Non-twinners`)* 1000, digits = 2L),
      "Offspring birth period" = paste(min(.data$birth_year, na.rm = TRUE), "-", max(.data$birth_year, na.rm = TRUE), sep = ""),
      "Births" = dplyr::n(),
      "Singleton births" = sum(!.data$birth_twin, na.rm = TRUE),
      "Twin births" = sum(.data$birth_twin, na.rm = TRUE),
      "Twinning rate (\u2030)" = round(mean(.data$birth_twin, na.rm = TRUE) * 1000, digits = 2L),
      "Total births (min-median-max)" =  paste(1, "-", round(stats::median(.data$total_births, na.rm = TRUE), digits = 2L), "-", max(.data$total_births, na.rm = TRUE), sep = "")) %>%
    dplyr::mutate(
      "Population" = dplyr::case_when(
        .data$pop == "FinW" ~ "Finland West",
        .data$pop == "FinE" ~ "Finland East",
        .data$pop == "FinSWA" ~ "Finland SW-Archipelago",
        .data$pop == "FinL" ~ "Finland Lapland",
        .data$pop == "SweL" ~ "Sweden Lapland",
        .data$pop == "Nor" ~ "Norway",
        .data$pop == "SamL" ~ "Sami Lapland",
        .data$pop == "Kru" ~ "Krummh\u00f6rn",
        .data$pop == "Swi" ~ "Switzerland"),
      "Locations" = dplyr::case_when(
        .data$pop == "FinW" ~ "Ikaalinen, Pulkkila, Tyrv\u00e4\u00e4",
        .data$pop == "FinE" ~ "Jaakkima, Rautu",
        .data$pop == "FinSWA" ~ "Hiittinen, Kustavi, Rym\u00e4ttyl\u00e4",
        .data$pop == "FinL" ~ "Inari, Enonteki\u00f6 and Sodankyl\u00e4",
        .data$pop == "SweL" ~ "Karesuando, Jukkasj\u00e4rvi, Jokkmok, Vilhelmina and J\u00e4llivaara",
        .data$pop == "Nor" ~ "Sm\u00f8la and Soknedal",
        .data$pop == "SamL" ~ "Inari, Enonteki\u00f6 and Sodankyl\u00e4",
        .data$pop == "Kru" ~ "Lower Saxony, Germany",
        .data$pop == "Swi" ~ "Linthal, Elm"),
      "References" = dplyr::case_when(
        .data$pop == "FinW" ~ "Pettay et al. 2016; Pettay et al. 2018",
        .data$pop == "FinE" ~ "Pettay et al. 2016; Pettay et al. 2018",
        .data$pop == "FinSWA" ~ "Haukioja et al. 1989; Lummaa et al. 1998",
        .data$pop == "FinL" ~ "Helle et al. 2014; Helle 2019",
        .data$pop == "SweL" ~ "Helle et al. 2014; Sk\u00f6ld and Axelsson 2008; Sk\u00f6ld et al. 2011; Helle 2019",
        .data$pop == "Nor" ~ "Skj\u0153rv\u00f8 et al. 2009",
        .data$pop == "SamL" ~ "Helle et al. 2004; Helle 2019",
        .data$pop == "Kru" ~ "Gabler and Voland 1994",
        .data$pop == "Swi" ~ "Evans et al. 2018")) %>%
    dplyr::relocate(.data$Population, .before = 1) %>%
    dplyr::relocate(.data$Locations, .after = .data$Population) %>%
    dplyr::select(-.data$pop) %>%
    dplyr::arrange(.data$Population) -> tbl_by_pop

  ## build the part of the table that contains information for all populations combined:
  tibble::tibble("Population" = "All the above",
                 "Locations" = "All the above",
                 "Maternal birth period" = paste(min(floor(data$maternal_birthyear), na.rm = TRUE), "-", max(floor(data$maternal_birthyear), na.rm = TRUE), sep = ""),
                 "Mothers" = length(unique(data$maternal_id)),
                 "Non-twinners" = length(unique(data$maternal_id[data$nontwinners])),
                 "Twinners" = length(unique(data$maternal_id[data$twinners])),
                 "Twinner rate (\u2030)" = round(.data$Twinners / (.data$Twinners + .data$`Non-twinners`)* 1000, digits = 2L),
                 "Offspring birth period" = paste(min(data$birth_year, na.rm = TRUE), "-", max(data$birth_year, na.rm = TRUE), sep = ""),
                 "Births" = nrow(data),
                 "Singleton births" = sum(!data$birth_twin, na.rm = TRUE),
                 "Twin births" = sum(data$birth_twin, na.rm = TRUE),
                 "Twinning rate (\u2030)" = round(mean(data$birth_twin, na.rm = TRUE) * 1000, digits = 2L),
                 "Total births (min-median-max)" =  paste(1, "-", round(stats::median(data$total_births, na.rm = TRUE), digits = 2L), "-", max(data$total_births, na.rm = TRUE), sep = ""),
                 "References" = "This paper") -> tbl_total

  ## combine the 2 tables and output the result:
  dplyr::bind_rows(tbl_by_pop, tbl_total)
}


#' Export a table in the *.xlsx format
#'
#' This function is used to export tables into Microsoft Excel format.
#' It is a wrapper for the function [`write.xlsx`][`openxlsx::write.xlsx`].
#'
#' @param table a `tibble` or `data.frame` to be exported
#' @param file a string of characters indicating the name file to create (with path and extension required)
#' @return the table (but the return is invisible)
#' @export
#'
#' @examples
#' # See ?twinR
#'
export_table_xlsx <- function(table, file) {

  ## load openxlsx package if installed:
  if (requireNamespace("openxlsx", quietly = TRUE)) {

    tryCatch({## the function can crash if no zip program is known to R

      ## initialization:
      wb <- openxlsx::createWorkbook("Details of the data used in the study")
      openxlsx::addWorksheet(wb, 'Sheet 1', gridLines = FALSE)
      openxlsx::writeData(wb, sheet = 1, x = table)

      ## setup the header:
      header_style <- openxlsx::createStyle(border = "TopBottom",
                                            textDecoration = "Bold",
                                            halign = "center")
      openxlsx::addStyle(wb, sheet = 1, style = header_style,
                         rows = 1, cols = 1:ncol(table))

      ## setup the last row:
      bottom_style <- openxlsx::createStyle(border = "TopBottom")
      openxlsx::addStyle(wb, sheet = 1, style = bottom_style,
                         rows = (nrow(table) + 1), cols = 1:ncol(table))

      ## format the numeric column so that trailing zeros are not lost:
      numeric_cols <- which(lapply(table, class) == "numeric")
      generstyle <- openxlsx::createStyle(numFmt = '0.00', halign = 'right')
      openxlsx::addStyle(wb, sheet = 1, style = generstyle,
                         rows = 2:(nrow(table) + 1), cols = numeric_cols,
                         gridExpand = TRUE, stack = TRUE)

      ## write file on disk:
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

      ## message for success:
      message(paste("The table has been created and is saved at location:",
                    normalizePath(paste0(file))))
    },

    ## message for failure (usually a zip related issue):
    error = function(e) {
      print("Unfortunately, the export as *.xlsx did not work. The package openxlsx requires that a zip program is registered to R. Here is the original error message:")
      print(e)
      cat("\n")
    },

    ## return:
    finally = return(invisible(table)))
  } else {
    stop("to save table in *.xlsx format, you need to install the package openxlsx!")
  }
}
