#' Read in a sample definition key file
#'
#' This is a wrapper around [readr::read_csv()] to read in and process
#' a `key.csv` file that contains the lookup table defining each sample's
#' AS (key registry) number, sex, genotype (if applicable), treatment (if
#' applicable), sample number, and the measure number of each site scanned.
#'
#' @param file The file path to the `key.csv` file.
#' @param sites A character vector of the sites scanned, included as columns in
#'   the `key.csv`.
#' @param ... Additional arguments passed on to [readr::read_csv()].
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
#' gen_key <- read_key_csv(mctr_ex("example-gen-key.csv"))
read_key_csv <- function(file, sites = c("Spine", "Met", "Dia"), ...) {
    key <- readr::read_csv(file, ...) |>
        tidyr::pivot_longer(cols = sites,
                            names_to = "Site",
                            values_to = "MeasNo")
    key
}

#' Read in trabecular bone data
#'
#' This is a wrapper around [readr::read_csv()] to read in and process
#' trabecular bone microCT data from a `trabecular.csv` file. It uses a
#' `key` object to look up and add sex, genotype, and site information.
#'
#' @param file The file path to the `trabecular.csv` file.
#' @param key The `key` object containing sample information, as created by
#'   the [read_key_csv()] function.
#' @param ... Additional arguments passed on to [readr::read_csv()].
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
#' gen_key <- read_key_csv(mctr_ex("example-gen-key.csv"))
#' gen_trab <- read_trabecular_csv(mctr_ex("example-trabecular.csv"),
#'                                          key = gen_key)
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

read_twice1_csv <- function(twice1_file, key, ...) {
    twice1 <- readr::read_csv(twice1_file, ...) |>
        dplyr::select(SampNo, MeasNo,
                      `VOX-TV`) |>
        dplyr::rename(TV.1 = `VOX-TV`) |>
        dplyr::left_join(dplyr::filter(key, Site != "Spine"), y = _,
                         by = dplyr::join_by(SampNo, MeasNo))
    twice1
}

read_twice2_csv <- function(twice2_file, key, ...) {
    twice2 <- readr::read_csv(twice2_file, ...) |>
        dplyr::select(SampNo, MeasNo,
                      `VOX-TV`, `VOX-BV`, `VOX-BV/TV`, Mean2) |>
        dplyr::rename(TV.2 = `VOX-TV`,
                      BV = `VOX-BV`,
                      `BV/TV` = `VOX-BV/TV`) |>
        dplyr::left_join(dplyr::filter(key, Site != "Spine"), y = _,
                         by = dplyr::join_by(SampNo, MeasNo))
}

#' Read in cortical bone data
#'
#' This is a wrapper around [readr::read_csv()] to read in and process
#' cortical bone microCT data from two files: a `twice1.csv` and a
#' `twice2.csv` file. It uses a `key` object to look up and add sex,
#' genotype, and site information.
#'
#' @param twice1_file The file path to the `twice1.csv` file.
#' @param twice2_file The file path to the `twice2.csv` file.
#' @param key The `key` object containing sample information, as created
#'   by the [read_key_csv()] function.
#' @param ... Additional arguments passed on to [readr::read_csv()].
#'
#' @return A tibble/data frame containing cortical bone data. For example:
#'   ```
#'   |   AS|Sex |Genotype | SampNo|Site | MeasNo|  Ct.vBMD| Ct.Th| End.Circ| Peri.Circ| Ct.Po| Ct.Po.V|
#'   |----:|:---|:--------|------:|:----|------:|--------:|-----:|--------:|---------:|-----:|-------:|
#'   | 1365|M   |Cre      |  10778|Met  |  31710|  937.049| 0.129|    5.533|     6.342|  0.41|   0.002|
#'   | 1365|M   |Cre      |  10778|Dia  |  31712| 1114.873| 0.220|    3.963|     5.343|  0.11|   0.001|
#'   | 1366|F   |Cre      |  10779|Met  |  31713| 1011.473| 0.183|    4.337|     5.485|  0.26|   0.001|
#'   | 1366|F   |Cre      |  10779|Dia  |  31715| 1133.066| 0.232|    3.250|     4.705|  0.10|   0.000|
#'   | 1367|F   |Cre;fl   |  10780|Met  |  31716|  994.868| 0.145|    4.892|     5.806|  0.37|   0.002|
#'   | 1367|F   |Cre;fl   |  10780|Dia  |  31718| 1137.300| 0.209|    3.514|     4.827|  0.14|   0.001|
#'   ```
#'
#' @export
#'
#' @examples
#' gen_key <- read_key_csv(mctr_ex("example-gen-key.csv"))
#' gen_cort <- read_cortical_csv(mctr_ex("example-twice1.csv"),
#'                           mctr_ex("example-twice2.csv"),
#'                           gen_key)
read_cortical_csv <- function(twice1_file, twice2_file, key, ...) {
    twice1 <- read_twice1_csv(twice1_file, key, ...)
    twice2 <- read_twice2_csv(twice2_file, key, ...)

    cort <- dplyr::right_join(twice1, twice2,
                              by = dplyr::join_by(AS, Sex, Genotype,
                                                  SampNo, Site, MeasNo)) |>
        dplyr::mutate(Peri.V = TV.1,
                      Peri.Ar = Peri.V / (50 * 0.0105),
                      Peri.Rad = sqrt(Peri.Ar / pi),
                      Ct.V = TV.2,
                      Ct.Ar = Ct.V / (50 * 0.0105),
                      End.Ar = Peri.Ar - Ct.Ar,
                      End.Rad = sqrt(End.Ar / pi),
                      Ct.Po = 1 - `BV/TV`,
                      Peri.Circ = 2 * pi * Peri.Rad,
                      End.Circ = 2 * pi * End.Rad,
                      Ct.Th = Peri.Rad - End.Rad,
                      Ct.Po.V = Ct.Po * Ct.V,
                      Ct.vBMD = Mean2) |>
        dplyr::mutate(Ct.Po = Ct.Po * 100) |>
        dplyr::select(AS, Sex, Genotype, SampNo, Site, MeasNo,
                      Ct.vBMD, Ct.Th, End.Circ, Peri.Circ, Ct.Po, Ct.Po.V)
    cort
}

#' Read in finite element analysis data
#'
#' This is a wrapper around [readr::read_csv()] to read in and process
#' finite element analysis microCT data from a `mfe.csv` file. It uses
#' a `key` object to look up and add sex, genotype, and site information.
#'
#' @param file The file path to the `mfe.csv` file.
#' @param key The `key` object containing sample information, as created
#'   by the [read_key_csv()] function.
#' @param ... Additional arguments passed on to [readr::read_csv()].
#'
#' @return A tibble/data frame containing finite element analysis data.
#'   For example:
#'   ```
#'   |   AS|Sex |Genotype | SampNo|Site  | MeasNo|       S|  F.ult|
#'   |----:|:---|:--------|------:|:-----|------:|-------:|------:|
#'   | 1365|M   |Cre      |  10778|Spine |  31711| 11480.0| 32.257|
#'   | 1365|M   |Cre      |  10778|Met   |  31710| 19034.1| 57.483|
#'   | 1365|M   |Cre      |  10778|Dia   |  31712| 25405.7| 79.100|
#'   | 1366|F   |Cre      |  10779|Spine |  31714| 12449.0| 34.213|
#'   | 1366|F   |Cre      |  10779|Met   |  31713| 20679.9| 62.022|
#'   | 1366|F   |Cre      |  10779|Dia   |  31715| 22832.0| 70.721|
#'
#' @export
#'
#' @examples
#' gen_key <- read_key_csv(mctr_ex("example-gen-key.csv"))
#' gen_mfe <- read_mfe_csv(mctr_ex("example-mfe.csv"),
#'                                      gen_key)
read_mfe_csv <- function(file, key, ...) {
    mfe <- readr::read_csv(file, ...) |>
        dplyr::select(SampNo, MeasNo,
                      S, F.ult) |>
        dplyr::mutate(F.ult = -1 * F.ult) |>
        dplyr::right_join(key, y = _,
                          by = dplyr::join_by(SampNo, MeasNo))
    mfe
}
