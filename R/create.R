#' Create a project skeleton to compare bone microCT data between genotypes
#'
#' This function assumes you are in a project root directory and creates an R
#' Markdown file containing a pre-filled bone microCT analysis comparing two
#' genotypes. It also creates a `data` directory containing template files into
#' which to paste the microCT data. Additionally, the `data` directory contains
#' a template `key.csv` file to specify the appropriate sample information.
#'
#' The `data` directory contains the following files:
#'  - `key.csv`
#'  - `mfe.csv`
#'  - `trabecular.csv`
#'  - `twice1.csv`
#'  - `twice2.csv`
#'
#' @param file_name A string to name the project analysis R Markdown document.
#'   Defaults to a file in the style of
#'   `"YYYY-MM-DD-microCT-Genotype-Comparison.Rmd"`, where `YYYY-MM-DD`
#'   represents today's date.
#'
#' @return Returns `TRUE` if successful.
#' @export
#'
#' @examplesIf FALSE
#' create_genotype_comparison()
create_genotype_comparison <- function(file_name = paste0(format(Sys.Date(),
                                                                 "%F"),
                                                          "-microCT-Genotype-Comparison.Rmd")) {
    file.copy(from = system.file("rmarkdown",
                                 "templates",
                                 "comparing-genotypes",
                                 "skeleton",
                                 "skeleton.Rmd",
                                 package = "microCTr"),
              to = here::here(file_name))

    dir.create(here::here("data"))
    file.copy(from = system.file("templates",
                                 "genotype-key.csv",
                                 package = "microCTr"),
              to = here::here("data", "key.csv"))
    file.copy(from = system.file("templates",
                                 "trabecular.csv",
                                 package = "microCTr"),
              to = here::here("data", "trabecular.csv"))
    file.copy(from = system.file("templates",
                                 "twice1.csv",
                                 package = "microCTr"),
              to = here::here("data", "twice1.csv"))
    file.copy(from = system.file("templates",
                                 "twice2.csv",
                                 package = "microCTr"),
              to = here::here("data", "twice2.csv"))
    file.copy(from = system.file("templates",
                                 "mfe.csv",
                                 package = "microCTr"),
              to = here::here("data", "mfe.csv"))
}
