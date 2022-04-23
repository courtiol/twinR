#' Build Supplementary Information
#'
#' This function compiles the SI. It relies on a template "SI.Rnw" located in the package folder
#' `extdata`. For the compilation to work, you need to have created all the SI figures and tables,
#' and you need your system to be properly setup for the creation of LaTeX-PDF using knitr.
#'
#' @param path_SI the computer path to the folder where the pdf of the compiled SI will be written
#' @param overwrite a boolean indicating whether or not to overwrite a potentially existing SI.pdf file (default = FALSE)
#' @export
#'
#' @examples
#' # See ?twinR
#'
build_SI <- function(path_SI = "SI", overwrite = FALSE) {

  ## check and strengthen path to SI:
  dir.create(path = path_SI, showWarnings = FALSE)
  path_SI <- normalizePath(path_SI, mustWork = TRUE)
  path_SI_file <- paste0(path_SI, "/SI.pdf")
  path_tex_file <- paste0(path_SI, "/SI.tex")


  ## we check if the SI has already been created:
  if (file.exists(path_SI_file) && interactive() && !overwrite) {
      choice <- utils::menu(
        choices = c("display that file", "recreate the file"),
        title = "A file SI.pdf has already been created before.\n\nWhat do you want to do?"
      )
    if (choice == 0) {
      message("you have aborted the SI creation by inputing 0")
      return(invisible(NULL))
    }
    if (choice == 1) {
      utils::browseURL(path_SI_file)
      return(invisible(path_SI_file))
    }
  }

  ## check and strengthen path to SI_template (the Rmd file used to create the SI):
  ## note: the template is stored at a different place depending on whether one
  ## uses the installed version of the package or the one loaded with devtools during
  ## the development of the package. We thus try both:
  path_source_SI_1 <- paste0(find.package("twinR"), "/inst/extdata")
  path_source_SI_2 <- paste0(find.package("twinR"), "/extdata")

  if (dir.exists(path_source_SI_1)) {
    path_source_SI <- path_source_SI_1
  } else if (dir.exists(path_source_SI_2)) {
    path_source_SI <- path_source_SI_2
  } else {
    stop("the template for building the SI (SI.Rnw) is not found")
  }

  path_source_SI <- normalizePath(path_source_SI, mustWork = TRUE)
  path_source_SI_file <- paste0(path_source_SI, "/SI.Rnw")

  ## check if the knitr is installed because we don't have it in IMPORTS:
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("you need to install the package 'knitr' to use this function")
  }

  ## actual job:
  message("the SI is being created (be patient)...")
  #system(paste0("cp ", path_source_SI, "/*.bst ", path_SI))
  system(paste0("cp ", path_source_SI, "/*.bib ", path_SI))
  vignette_path <- knitr::knit2pdf(input = path_source_SI_file, output = path_tex_file, quiet = FALSE, bib_engine = "biber", clean = TRUE)
  message("the SI should have been created and is stored at the following location:")
  message(vignette_path)

  ## We open the vignette:
  utils::browseURL(path_SI_file)

  ## We return the path:
  invisible(path_SI_file)
}

