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

#' Write organized data to an Excel file
#'
#' This function takes microCT data which has been read in and had measures
#' calculated (as by the `read_excel*()` functions), sorts it, and writes it to
#' an Excel file. Trabecular, cortical, and FEA data are reqired, and each is
#' written to its own sheet in the final Excel file.
#'
#' @param trab Trabecular microCT data, as formatted by the
#'   [read_trabecular_excel()] function.
#' @param cort Cortical microCT data, as formatted by the
#'   [read_cortical_excel()] function.
#' @param mfe FEA microCT data, as formatted by the [read_mfe_excel()] function.
#' @param file The name of the Excel file to write to. Defaults to `Organized
#'   Data.xlsx`.
#'
#' @export
#'
#' @examplesIf FALSE
#' gen_key <- read_key_csv(mctr_ex("example-gen-key.csv"))
#' gen_trab <- read_trabecular_csv(mctr_ex("example-trabecular.csv"), gen_key)
#' gen_cort <- read_cortical_csv(mctr_ex("example-twice1.csv"),
#'                               mctr_ex("example-twice2.csv"), gen_key)
#' gen_mfe <- read_mfe_csv(mctr_ex("example-mfe.csv"), gen_key)
#' create_organized_excel(gen_trab, gen_cort, gen_mfe)
create_organized_excel <- function(trab, cort, mfe, file = "Organized Data.xlsx") {
    if ("Genotype" %in% names(trab)) {
        trab <- trab |> dplyr::arrange(Site, Sex, Genotype, SampNo, MeasNo)
        cort <- cort |> dplyr::arrange(Site, Sex, Genotype, SampNo, MeasNo)
        mfe <- mfe |> dplyr::arrange(Site, Sex, Genotype, SampNo, MeasNo)
    } else if ("Treatment" %in% names(trab)) {
        trab <- trab |> dplyr::arrange(Site, Sex, Treatment, SampNo, MeasNo)
        cort <- cort |> dplyr::arrange(Site, Sex, Treatment, SampNo, MeasNo)
        mfe <- mfe |> dplyr::arrange(Site, Sex, Treatment, SampNo, MeasNo)
    }

    data <- list("Trabecular" = trab,
                 "Cortical" = cort,
                 "FEA" = mfe)

    openxlsx2::write_xlsx(data, file = file)
}

#' Convert a raw data Excel file to an organized Excel file
#'
#' This function is meant to be used as an RStudio addin. It uses `rstudioapi`
#' to ask the user to select an original/raw data Excel file, and then it asks
#' the user to create a file to save the organized Excel file.
#'
#' @export
#'
#' @examplesIf FALSE
#' excel_export_addin()
excel_export_addin <- function() {
    data_file <- rstudioapi::selectFile(
        caption = "Select File with Original Data",
        filter = "Excel Files (*.xlsx)"
        )

    key <- read_key_excel(data_file)
    trab <- read_trabecular_excel(data_file, key)
    cort <- read_cortical_excel(data_file, key)
    mfe <- read_mfe_excel(data_file, key)

    create_organized_excel(
        trab,
        cort,
        mfe,
        file = rstudioapi::selectFile(caption = "Create a File to Save Organized Data",
                                      filter = "Excel Files (*.xlsx)",
                                      existing = FALSE)
        )
}
