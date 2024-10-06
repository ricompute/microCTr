#' Print bone microCT data
#'
#' This is a wrapper around [knitr::kable()] to print out bone microCT
#' data at one site by sex for use in an R Markdown document.
#'
#' @param data A data frame containing bone microCT data, formatted as is the
#'   output of [read_trabecular_csv()], [read_cortical_csv()], or
#'   [read_mfe_csv()].
#' @param ... Additional arguments passed on to [knitr::kable()]. This function
#'   uses a local default of `digits = 3` to specify how many significant digits
#'   to print. This can be modified by passing a user specified `digits` value.
#'
#' @return Text output which by default is a Pandoc markdown pipe table for each
#'   sex contained in the `data` supplied. For example:
#'   ```
#'   |   AS|Sex |Genotype | BV/TV|  SMI| Tb.N| Tb.Th| Tb.Sp|
#'   |----:|:---|:--------|-----:|----:|----:|-----:|-----:|
#'   | 1365|M   |Cre      |  5.43| 3.23| 2.46| 0.062| 0.412|
#'   | 1370|M   |Cre      |  6.39| 3.01| 2.59| 0.060| 0.373|
#'   | 1422|M   |Cre      |  8.65| 2.95| 2.81| 0.072| 0.351|
#'   | 1369|M   |Cre;fl   | 12.53| 2.44| 3.33| 0.066| 0.286|
#'   | 1420|M   |Cre;fl   |  6.58| 3.39| 2.68| 0.072| 0.353|
#'   | 1421|M   |Cre;fl   | 14.70| 1.79| 3.23| 0.070| 0.302|
#'
#'
#'
#'
#'   |   AS|Sex |Genotype | BV/TV|  SMI| Tb.N| Tb.Th| Tb.Sp|
#'   |----:|:---|:--------|-----:|----:|----:|-----:|-----:|
#'   | 1366|F   |Cre      |  4.47| 3.73| 2.67| 0.059| 0.371|
#'   | 1372|F   |Cre      |  1.33| 4.40| 1.55| 0.074| 0.666|
#'   | 1373|F   |Cre      |  2.27| 3.36| 2.69| 0.036| 0.373|
#'   | 1367|F   |Cre;fl   |  0.23| 4.17| 1.79| 0.034| 0.558|
#'   | 1368|F   |Cre;fl   |  0.77| 4.08| 2.10| 0.040| 0.482|
#'   | 1371|F   |Cre;fl   |  1.62| 3.43| 1.86| 0.039| 0.527|
#'   ```
#'
#' @export
#'
#' @examples
#' key <- read_key_csv(microCT_example("example-key.csv"))
#' trab <- read_trabecular_csv(microCT_example("example-trabecular.csv"),
#'                             key)
#' print_data(trab |> dplyr::filter(Site == "Met"))
print_data <- function(data, ...) {
    withr::local_options(list(digits = 3))
    sexes <- data$Sex |> unique()
    for (j in 1:length(sexes)) {
        print(knitr::kable(data |>
                               dplyr::filter(Sex == sexes[j]) |>
                               dplyr::select(-SampNo, -Site, -MeasNo) |>
                               dplyr::arrange(Genotype),
                           ...))
        cat("\n\n")
    }
}
