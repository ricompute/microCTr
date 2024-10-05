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
    key <- readr::read_csv(file, ...) |>
        tidyr::pivot_longer(cols = sites,
                            names_to = "Site",
                            values_to = "MeasNo")
    key
}

#' Read in trabecular data
#'
#' This is a wrapper around `readr::read_csv()` to read in and process
#' trabecular bone microCT data from a `trabecular.csv` file. It uses a
#' `key` object to look up and add sex, genotype, and site information.
#'
#' @param file The file path to the `trabecular.csv` file.
#' @param key The `key` object containing sample information, as created by
#'   the `read_key_csv()` function.
#' @param ... Additional arguments passed on to `readr::read_csv()`.
#'
#' @return A tibble/data frame containing trabecular bone data. For example:
#'   ```
#'   |   AS|Sex |Genotype | SampNo|Site  | MeasNo| BV/TV|   SMI|   Tb.N| Tb.Th| Tb.Sp|
#'   |----:|:---|:--------|------:|:-----|------:|-----:|-----:|------:|-----:|-----:|
#'   | 1365|M   |Cre      |  10778|Spine |  31711| 13.05| 1.631|  3.097| 0.051| 0.316|
#'   | 1365|M   |Cre      |  10778|Met   |  31710|  5.43| 3.227|  2.460| 0.062| 0.412|
#'   | 1366|F   |Cre      |  10779|Spine |  31714| 21.88| 0.987| 14.227| 0.047| 0.111|
#'   | 1366|F   |Cre      |  10779|Met   |  31713|  4.47| 3.725|  2.666| 0.059| 0.371|
#'   | 1367|F   |Cre;fl   |  10780|Spine |  31717| 14.47| 0.989|  2.551| 0.060| 0.398|
#'   | 1367|F   |Cre;fl   |  10780|Met   |  31716|  0.23| 4.172|  1.789| 0.034| 0.558|
#'   ```
#'
#' @export
#'
#' @examples
#' key <- microCTr_example("example-key.csv") |>
#'     read_key_csv()
#' trab <- microCTr_example("example-trabecular.csv") |>
#'     read_trabecular_csv(key = key)
read_trabecular_csv <- function(file, key, ...) {
    trab <- readr::read_csv(file, ...) |>
        dplyr::select(SampNo, MeasNo,
                      `VOX-BV/TV`,
                      `TRI-SMI`,
                      `DT-Tb.N`,
                      `DT-Tb.Th`,
                      `DT-Tb.Sp`) |>
        dplyr::mutate(`BV/TV` = `VOX-BV/TV` * 100) |>
        dplyr::rename(SMI = `TRI-SMI`,
                      Tb.N = `DT-Tb.N`,
                      Tb.Th = `DT-Tb.Th`,
                      Tb.Sp = `DT-Tb.Sp`) |>
        dplyr::select(SampNo, MeasNo, `BV/TV`, SMI, Tb.N, Tb.Th, Tb.Sp) |>
        dplyr::right_join(key, y = _,
                          by = dplyr::join_by(SampNo, MeasNo))
    trab
}
