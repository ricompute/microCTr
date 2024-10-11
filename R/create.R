#' Create a project skeleton to compare bone microCT data
#'
#' These functions assume you are in a project root directory and create an R
#' Markdown file containing a pre-filled bone microCT analysis comparing two
#' genotypes or two treatments. They also create a `data.xlsx` template file
#' into which to paste the microCT data. The `data.xlsx` file additionally has a
#' `Key` sheet to specify the appropriate sample information.
#'
#' @param file_name A string to name the project analysis R Markdown document.
#'   Defaults to a file in the style of `"YYYY-MM-DD-microCT-Analysis.Rmd"`,
#'   where `YYYY-MM-DD` represents today's date.
#'
#' @return Returns `TRUE` if successful.
#' @export
#'
#' @examplesIf FALSE
#' create_genotype_comparison()
#' create_treatment_comparison()
create_genotype_comparison <- function(file_name = paste0(format(Sys.Date(),
                                                                 "%F"),
                                                          "-microCT-Analysis.Rmd")) {
    file.copy(from = system.file("rmarkdown",
                                 "templates",
                                 "comparing-groups",
                                 "skeleton",
                                 "skeleton.Rmd",
                                 package = "microCTr"),
              to = here::here(file_name))

    file.copy(from = system.file("templates",
                                 "genotype.xlsx",
                                 package = "microCTr"),
              to = here::here("data.xlsx"))
}

#' @rdname create_genotype_comparison
#' @export
create_treatment_comparison <- function(file_name = paste0(format(Sys.Date(),
                                                                 "%F"),
                                                          "-microCT-Analysis.Rmd")) {
    file.copy(from = system.file("rmarkdown",
                                 "templates",
                                 "comparing-groups",
                                 "skeleton",
                                 "skeleton.Rmd",
                                 package = "microCTr"),
              to = here::here(file_name))

    file.copy(from = system.file("templates",
                                 "treatment.xlsx",
                                 package = "microCTr"),
              to = here::here("data.xlsx"))
}

#' Render an R Markdown document with colored text
#'
#' This function is a wrapper around [rmarkdown::render()] which supplies a
#' custom Lua Pandoc filter to color text.
#'
#' @param input The input file to be rendered.
#' @param ... Additional arguments passed on to [rmarkdown::render()].
#'
#' @return As for [rmarkdown::render()], when `run_pandoc = TRUE`, the compiled
#'   document is written into the output file, and the path of the output file
#'   is returned. When `run_pandoc = FALSE`, the path of the Markdown output
#'   file, with attributes `knit_meta` (the knitr meta data collected from code
#'   chunks) and `intermediates` (the intermediate files/directories generated
#'   by [rmarkdown::render()]).
#' @export
#'
#' @examplesIf FALSE
#' knit_with_colored_text("example_file.Rmd")
#'
knit_with_colored_text <- function(input, ...) {
    lua_filter <-  rmarkdown::pandoc_lua_filter_args(
            system.file("pandoc", "color-text.lua", package = "microCTr")
        )
    rmarkdown::render(
        input,
        output_options = list(pandoc_args = lua_filter),
        ...
    )
}
