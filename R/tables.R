
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
#' build_data_summary.table(data_births_all)
#'
build_data_summary.table <- function(birth_level_data) {

  ## add total births and twinning status to the data:
  birth_level_data %>%
    dplyr::group_by(.data$pop, .data$maternal_id) %>%
    dplyr::mutate(total_births = dplyr::n(),
                  twinners     = any(.data$twin),
                  nontwinners  = all(!.data$twin)) %>%
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
      "Twinner rate (\u2030)" = round(.data$Twinners / (.data$Twinners + .data$`Non-twinners`) * 1000, digits = 2L),
      "Offspring birth period" = paste(min(.data$birth_year, na.rm = TRUE), "-", max(.data$birth_year, na.rm = TRUE), sep = ""),
      "Births" = dplyr::n(),
      "Singleton births" = sum(!.data$twin, na.rm = TRUE),
      "Twin births" = sum(.data$twin, na.rm = TRUE),
      "Twinning rate (\u2030)" = round(mean(.data$twin, na.rm = TRUE) * 1000, digits = 2L),
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
        .data$pop == "FinL" ~ "Helle 2019",
        .data$pop == "SweL" ~ "Sk\u00f6ld and Axelsson 2008; Sk\u00f6ld et al. 2011; Helle 2019",
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
                 "Twinner rate (\u2030)" = round(.data$Twinners / (.data$Twinners + .data$`Non-twinners`) * 1000, digits = 2L),
                 "Offspring birth period" = paste(min(data$birth_year, na.rm = TRUE), "-", max(data$birth_year, na.rm = TRUE), sep = ""),
                 "Births" = nrow(data),
                 "Singleton births" = sum(!data$twin, na.rm = TRUE),
                 "Twin births" = sum(data$twin, na.rm = TRUE),
                 "Twinning rate (\u2030)" = round(mean(data$twin, na.rm = TRUE) * 1000, digits = 2L),
                 "Total births (min-median-max)" =  paste(1, "-", round(stats::median(data$total_births, na.rm = TRUE), digits = 2L), "-", max(data$total_births, na.rm = TRUE), sep = ""),
                 "References" = "This paper") -> tbl_total

  ## combine the 2 tables and output the result:
  dplyr::bind_rows(tbl_by_pop, tbl_total)
}




#' Create fit summary tables
#'
#' This function is used to create the tables providing the details of the fitted models used in the study.
#'
#' @param fit the output of one of the function fitting models (see [`fit_models`])
#'
#' @return a `tibble` with an additional attribute called "formula"
#' @export
#'
#' @examples
#' #See ?twinR
#'
build_fit_summary.table <- function(fit) {

  ## extract raw model output:
  utils::capture.output(fit_components <- spaMM::summary.HLfit(fit))

  ## extract model formula:
  model_formula <- paste(as.character(fit$call$formula)[c(2,1,3)], collapse = " ")

  ## extract and format info on fixed effects:
  tibble::as_tibble(fit_components$beta_table) %>%
    dplyr::rename("value" = .data$Estimate) %>%
    dplyr::mutate(name = rownames(fit_components$beta_table), .before = 1) %>%
    dplyr::bind_cols(object = c("fixed effects", rep("", max(c(0, nrow(fit_components$beta_table) - 1)))), .) -> fixed_effects

  ## extract and format info on random effects:
  tibble::as_tibble(fit_components$lambda_table) %>%
    dplyr::select("name" = .data$Group) %>%
    dplyr::mutate(name = ifelse(.data$name == "maternal_.", "maternal_id", .data$name),
                  name = paste0("variance between ", .data$name)) %>%
    dplyr::bind_cols(object = c("random effects", rep("", max(c(0, nrow(.) - 1)))), .) -> random_effects

  if (!is.null(fit$lambda)) {
    random_effects %>%
      dplyr::mutate(value = fit$lambda) -> random_effects
  }

  ## extract and format info on model family:
  tibble::tibble(object = "response family",
                 name = as.character(fit$family$family),
                 value = NA) %>%
    dplyr::mutate(name = ifelse(.data$name == "negbin", "negative binomial", .data$name),
                  name = paste(.data$name, "with", as.character(fit$family$link), "link")) ->  model_family

  if (fit$family$family == "negbin") {
    model_family %>%
      dplyr::mutate(name = paste0(ifelse(fit$family$zero_truncated, "truncated ", ""), .data$name)) %>%
      dplyr::bind_rows(tibble::tibble(object = "", name = "shape parameter",
                                      value = get("shape", envir = environment(fit$family$aic)))) -> model_family
  }

  ## extract and format info on number of parameters, likelihood, AICs:
  dplyr::bind_cols(K = nrow(fixed_effects) + nrow(random_effects) + nrow(model_family) - 1,
                   L = tibble::as_tibble_row(fit_components$likelihoods)[2][[1]]) -> model_stats

  utils::capture.output(AICs <- stats::AIC(fit, verbose = FALSE, also_cAIC = TRUE)[1:2])

  model_stats <- dplyr::bind_cols(model_stats, tibble::as_tibble_row(AICs))
  names(model_stats) <- c("number of model parameters", "marginal log Likelihood", "marginal AIC", "conditional AIC (cAIC)")
  model_stats <- dplyr::bind_cols(object = c("fit info", rep("", ncol(model_stats) - 1)),
                                  tidyr::pivot_longer(cols = 1:ncol(model_stats), model_stats))

  ## extract and format info on fitted data:
  tibble::tibble(object = "data info", name = "number of fitted observations (N)", value = nrow(fit$data)) -> fitted_data

  ## combine all formated info:
  dplyr::bind_rows(fixed_effects, random_effects, model_family, model_stats, fitted_data) %>%
    dplyr::rename(Type = .data$object,
                  "Variable" = .data$name,
                  Value = .data$value) -> all_results

  ## add formula as an atribute of the tibble:
  attr(all_results, "formula") <- model_formula

  all_results
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



#' Format to LaTeX
#'
#' These functions are helper functions for turning R text into LaTeX.
#' into (knitr) LaTeX code.
#'
#' The functions used to generate the tables using the LaTeX syntax are essentially wrappers to the
#' function [`kable`][`knitr::kable`] modified with the help of the package kableExtra.
#' They contains in the code the captions of the supplementary tables.
#'
#' @param fit_summary.table a table produced by [`build_fit_summary.table`]
#' @param data_summary.table a table produced by [`build_data_summary.table`]
#' @param goodness_of_fit.table a table produced by [`goodness_of_fit`]
#' @param fit_n an integer indicating which models the fit refers to (for defining captions)
#' @param text a string of characters to turn into LaTeX
#' @param size a parameter for the size in LaTeX
#'
#' @name format_to_LaTeX
#' @return a string of characters
#'
NULL


#' @describeIn format_to_LaTeX turn a model fit summary table into the *.tex format
#'
#' @export
#'
format_fit_summary.table_2_LaTeX <- function(fit_summary.table, fit_n = 0) {

  ## check that packages required for this function are installed (since they are declared in Suggests and not Imports)
  if (!requireNamespace("knitr", quietly = TRUE) && requireNamespace("kableExtra", quietly = TRUE)) {
    stop("this function requires you to install the packages {knitr} and {kableExtra} for it to run.")
  }

  ## define caption based on fit_n:
  if (fit_n == 1) {
    extra_caption <- " In all tables showing the summary of fits, values given in column 3 for fixed effects, random effects (if applicable) and the parameters of the response family (if applicable) are estimates. Other values are the results of computations."
  } else if (fit_n %in% c(5, 9)) {
    extra_caption <- " Note that the variable \\texttt{IBI} fitted in the model actually corresponds to the duration of interbirth interval minus six months. This rescaling prevents numerical issues during the simulations. When this fitted model is used for predictions (in plots or to compute effect sizes), the missing six months are reintroduced to produce correct results. See legend of Table S1 for other details."
  } else {
  extra_caption <- " See legend of Table S1 for more details."
  }

  ## adapt formatting to LaTeX:
  fit_summary.table %>%
    dplyr::mutate(dplyr::across(where(is.character), format_text_2_LaTeX)) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), prettyNum, digits = 3L, scientific = FALSE)) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), function(txt) ifelse(txt == "NA", "", txt))) -> fit_summary.table


  ## fill Type column for defining highlighting:
  fit_summary.table_types_filled <- fit_summary.table
  while (any(fit_summary.table_types_filled$Type == "")) {
    fit_summary.table_types_filled %>%
      dplyr::select(.data$Type) %>%
      dplyr::mutate(Type = ifelse(.data$Type == "", dplyr::lag(.data$Type), .data$Type)) -> fit_summary.table_types_filled
  }

  ## build table
  knitr::kable(fit_summary.table,
               format = "latex",
               linesep = "", ## turn off auto space every 5 rows
               escape = FALSE, ## interpret tex "as is"
               booktabs = TRUE, ## bold line as in usual tables
               table.envir = "table",
               position = "H",
               label = paste0("tab", fit_n),
               caption = paste0("Summary of the fit of model ", fit_n, ". The model fit corresponds to the fit of a model with the following formula: ",
                                format_formula_2_LaTeX(fit_summary.table, size = "small"), ".", extra_caption),
               align = "llrrr") %>%
        kableExtra::column_spec(2, width = "5cm") %>%
        kableExtra::column_spec(1, width = "3cm") %>%
        kableExtra::kable_styling(full_width = FALSE,
                                  latex_options = c("striped"), ## grey highlights
                                  font_size = 8,
                                  stripe_index = which(fit_summary.table_types_filled$Type %in% c("fixed effects", "response family", "data info"))) ## grey highlight locations

}




#' @describeIn format_to_LaTeX turn a data summary table into the *.tex format
#'
#' @export
#'
format_data_summary.table_2_LaTeX <- function(data_summary.table) {
  data_summary.table %>%
    knitr::kable(format = "latex",
                 linesep = "", ## turn off auto space every 5 rows
                 booktabs = TRUE, ## bold line as in usual tables
                 table.envir = "table",
                 label = "tab16",
                 caption = "Details of data used in the present study, for each population separately and for all populations combined. This table is the same as Table 1, but here we also include data from families with missing birth month information. Therefore this table includes an entry for the Norway dataset, in which birth month information was never available. All references are cited in main text.",
                 digits = 3L,
                align = "llcccccccccccl") %>%
  kableExtra::kable_styling(full_width = FALSE,
                            latex_options = c("striped"), ## grey highlights
                            font_size = 6) %>%
  kableExtra::column_spec(c(1, 2, 3, 8), width = "1.5cm") %>%
  kableExtra::column_spec(c(4:7, 9:13), width = "1cm") %>%
  kableExtra::column_spec(14, width = "2cm") %>%
  kableExtra::pack_rows(group_label = NULL, start_row = 10, end_row = 10, hline_before = TRUE, indent = FALSE) %>%
  kableExtra::landscape()
}




#' @describeIn format_to_LaTeX turn the table with the results from the goodness of fit into the *.tex format
#'
#' @export
#'
format_goodness_of_fit.table_2_LaTeX <- function(goodness_of_fit.table) {

  goodness_of_fit.table$scenario[goodness_of_fit.table$scenario == "base_model"] <- "0"

  goodness_of_fit.table %>%
    dplyr::rename(Scenario = .data$scenario,
                  `GOF p-value` = .data$pv_gof,
                  `Raw p-value` = .data$pv_raw) -> goodness_of_fit.table

  goodness_of_fit.table %>%
    knitr::kable(format = "latex",
                 linesep = "", ## turn off auto space every 5 rows
                 booktabs = TRUE, ## bold line as in usual tables
                 table.envir = "table",
                 label = "tab13",
                 caption = "Results of the goodness-of-fit tests. P-values underlined denote scenario simulations generating data for which the relationship between twinning propensity and fertility is similar to the one estimated on the raw data, using a threshold of 0.05. The two columns for p-values correspond, respectivelly from left to right, to p-values obtained in the case of the double-bootstrap or single-level bootstrap procedure  (see Supplementary Notes \\& Fig. S7).",
                 digits = 3L,
                 align = "lrr") %>%
    kableExtra::column_spec(2, underline = goodness_of_fit.table$`GOF p-value` >= 0.05) %>%
    kableExtra::column_spec(3, underline = goodness_of_fit.table$`Raw p-value` >= 0.05) %>%
    kableExtra::kable_styling(full_width = FALSE,
                              latex_options = c("striped"), ## grey highlights
                              font_size = 8)
}



#' @describeIn format_to_LaTeX extract a model formula from a fitted model and format it for LaTeX
#' @export
#'
format_formula_2_LaTeX <- function(fit_summary.table, size = NULL) {
  formula <- attr(fit_summary.table, "formula")
  if (grepl("twin ~", formula)) {
    formula <- sub("twin ~", "T ~", formula, fixed = TRUE)
  }
  text <- paste0("$\\mathtt{", formula, "}$")
  text <- sub("~", "\\sim", text, fixed = TRUE)
  text <- gsub("_", "\\_", text, fixed = TRUE)
  if (!is.null(size) && size == "small") {
    text <- paste0("{\\small", text, "}")
  }
  text
}


#' @describeIn format_to_LaTeX format text for LaTeX
#' @export
#'
format_text_2_LaTeX <- function(text) {
  text <- gsub("_", "\\_", text, fixed = TRUE)
  text <- sub("(Intercept)", "$\\beta_1$", text, fixed = TRUE)

  text <- unlist(lapply(text, function(x) {
    exp <- regmatches(x, gregexpr("\\d", x))[[1]]
    if (length(exp) > 1L) {
      age_exp <- exp[2]
      parity_exp <- exp[3]
      ifelse(grepl(pattern = "poly", x = x), paste0("$\\beta_{\\mathtt{age}^", age_exp, "\\times\\mathtt{parity}^", parity_exp, "}$"), x)
    } else {
      x
    }
    }
  ))

  text <- sub("\\mathtt{age}^0\\times", "", text, fixed = TRUE)
  text <- sub("\\times\\mathtt{parity}^0", "", text, fixed = TRUE)
  text <- sub("\\mathtt{age}^1", "\\mathtt{age}", text, fixed = TRUE)
  text <- sub("\\mathtt{parity}^1", "\\mathtt{parity}", text, fixed = TRUE)

  text <- sub("twinTRUE", "$\\beta_{\\mathtt{twin}}$", text, fixed = TRUE)
  text <- sub("first\\_twinnerTRUE", "$\\beta_{\\mathtt{first\\_twinner}}$", text, fixed = TRUE)
  text <- sub("^twinnerTRUE$", "$\\\\beta_{\\\\mathtt{twinner}}$", text, fixed = FALSE)

  text <- sub("^twinnerTRUE:births\\\\_total\\\\_fac(\\d+.*)$", "$\\\\beta_{\\\\mathtt{twinner:births\\\\_total\\\\_fac\\1}}$", text, fixed = FALSE)
  text <- sub("^births\\\\_total\\\\_fac(\\d+.*)$", "$\\\\beta_{\\\\mathtt{births\\\\_total\\\\_fac\\1}}$", text, fixed = FALSE)
  text <- sub("^births\\\\_total$", "$\\\\beta_{\\\\mathtt{births\\\\_total}}$", text, fixed = FALSE)

  text <- sub("pop", "$\\mathtt{pop}$", text, fixed = TRUE)
  text <- sub("maternal\\_id", "$\\mathtt{maternal\\_id}$", text, fixed = TRUE)

  text <- sub("number of fitted observations (N)", "number of fitted observations (\\emph{N})", text, fixed = TRUE)
  text
}



