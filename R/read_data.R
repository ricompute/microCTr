#' Read in a sample definition key file
#'
#' This is a wrapper around `readr::read_csv()` to read in and process
#' a `key.csv` file that contains the lookup table defining each sample's
#' AS (key registry) number, sex, genotype (if applicable), treatment (if
#' applicable), sample number, and the measure number of each site scanned.
#'
#' @param file The file path to the `key.csv` file.
#' @param sites A character vector of the sites scanned, included as columns in
#'   the `key.csv`.
#' @param ... Additional arguments passed on to `readr::read_csv()`.
#'
#' @return A tibble/data frame containing the sample definitions.
#'   For example:
#'   ```
#'   |   AS|Sex |Genotype | SampNo|Site  | MeasNo|
#'   |----:|:---|:--------|------:|:-----|------:|
#'   | 1365|M   |Cre      |  10778|Spine |  31711|
#'   | 1365|M   |Cre      |  10778|Met   |  31710|
#'   | 1365|M   |Cre      |  10778|Dia   |  31712|
#'   | 1366|F   |Cre      |  10779|Spine |  31714|
#'   | 1366|F   |Cre      |  10779|Met   |  31713|
#'   | 1366|F   |Cre      |  10779|Dia   |  31715|
#'   ```
#'
#' @export
#'
#' @examples
#' key <- read_key_csv(microCTr_example("example-key.csv"))
read_key_csv <- function(file, sites = c("Spine", "Met", "Dia"), ...) {
    readr::read_csv(file, ...) |>
        tidyr::pivot_longer(cols = sites,
                            names_to = "Site",
                            values_to = "MeasNo")
}
