#' Create a project skeleton to compare bone microCT data between genotypes
#'
#' This function assumes you are in a project root directory and creates an R
#' Markdown file containing a pre-filled bone microCT analysis comparing two
#' genotypes. It also creates a `data.xlsx` template file into which to paste
#' the microCT data. The `data.xlsx` file additionally has a `Key` sheet to
#' specify the appropriate sample information.
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

    file.copy(from = system.file("templates",
                                 "genotype.xlsx",
                                 package = "microCTr"),
              to = here::here("data.xlsx"))
}
