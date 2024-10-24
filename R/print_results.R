#' Print bone microCT data
#'
#' This is a wrapper around [knitr::kable()] to print out bone microCT
#' data at one site by sex for use in an R Markdown document. A code
#' chunk containing this function should probably have the `results = "asis"`
#' option set to allow for pretty formatting.
#'
#' @param data A data frame containing bone microCT data, formatted as is the
#'   output of [read_trabecular_csv()], [read_cortical_csv()], or
#'   [read_mfe_csv()] (or `read_*_excel()`).
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
#' gen_key <- read_key_csv(mctr_ex("example-gen-key.csv"))
#' gen_trab <- read_trabecular_csv(mctr_ex("example-trabecular.csv"),
#'                                 gen_key)
#' print_data(gen_trab |> dplyr::filter(Site == "Met"))
print_data <- function(data, ...) {
    withr::local_options(list(digits = 3))
    sexes <- data$Sex |> unique()
    for (j in 1:length(sexes)) {
        if ("Genotype" %in% names(data)) {
            print(knitr::kable(data |>
                                   dplyr::filter(Sex == sexes[j]) |>
                                   dplyr::select(-SampNo, -Site, -MeasNo) |>
                                   dplyr::arrange(Genotype),
                               ...))
        } else if ("Treatment" %in% names(data)) {
            print(knitr::kable(data |>
                                   dplyr::filter(Sex == sexes[j]) |>
                                   dplyr::select(-SampNo, -Site, -MeasNo) |>
                                   dplyr::arrange(Treatment),
                               ...))
        }

        cat("\n\n")
    }
}

#' Print bone mciroCT comparison analysis results
#'
#' This function is a wraps and extends [knitr::kable()] to format and print the
#' results of bone microCT comparison analyses at one site by sex for use in an
#' R Markdown document. A code chunk containing this function should probably
#' have the `results = "asis"` option to allow pretty formatting. Any `NA`
#' values are printed as an empty string.
#'
#' @param results A list of microCT comparison results, formatted as is the
#'   output of [compare_groups()].
#' @param sig_color What color to print a significant result. Defaults to
#'   `"red"`. Set to `NULL` to disable (and just print everything black).
#' @param ... Additional arguments passed on to [knitr::kable()]. This function
#'   uses a local default of `digits = 3` to specify how many significant digits
#'   to print. This can be modified by passing a user specified `digits` value.
#'
#' @return Text output which is by default a Pandoc markdown pipe table for each
#'   measure and each sex analyzed. If two sexes are analyzed, Pandoc fenced div
#'   syntax is used to print the tables in two columns by sex. For example:
#'   ```
#'   :::: {style="display: flex;"}
#'
#'   ::: {}
#'
#'   **BV/TV**
#'
#'   |Sex |Genotype |  n| Mean|  SEM|     P|Sig |
#'   |:---|:--------|--:|----:|----:|-----:|:---|
#'   |M   |Cre      |  3| 18.5| 1.58|      |    |
#'   |M   |Cre;fl   |  3| 19.9| 1.11| 0.701|    |
#'
#'
#'   :::
#'
#'   ::: {}
#'
#'   **BV/TV**
#'
#'   |Sex |Genotype |  n| Mean|   SEM|     P|Sig |
#'   |:---|:--------|--:|----:|-----:|-----:|:---|
#'   |F   |Cre      |  3| 18.3| 1.041|      |    |
#'   |F   |Cre;fl   |  3| 17.1| 0.936| 0.656|    |
#'
#'
#'   :::
#'
#'   ::::
#' @export
#'
#' @examples
#' gen_key <- read_key_csv(mctr_ex("example-gen-key.csv"))
#' gen_trab <- read_trabecular_csv(mctr_ex("example-trabecular.csv"),
#'                             gen_key)
#' Sp.trab <- gen_trab |> dplyr::filter(Site == "Spine") |> compare_groups()
#' print_results(Sp.trab)
print_results <- function(results, sig_color = "red", ...) {
    withr::local_options(list(digits = 3, knitr.kable.NA = ""))
    sexes <- names(results[[1]])
    if (length(sexes) == 2) {
        cat(":::: {style=\"display: flex;\"}\n\n")
        for (j in 1:length(sexes)) {
            cat("::: {}\n\n")
            for (i in 1:length(results)) {
                dat <- results[[i]][[sexes[j]]]
                if ((sum(dat$Sig == "") == 2) | is.null(sig_color)) {
                    cat("**", names(results)[i], "**", sep = "")
                } else {
                    cat("[**", names(results)[i], "**]{color=\"", sig_color, "\"}", sep = "")
                }
                print(knitr::kable(dat, ...))
                cat("\n\n")
            }
            cat(":::\n\n")
        }
        cat("::::\n\n")
    } else if (length(sexes) == 1) {
        for (j in 1:length(sexes)) {
            for (i in 1:length(results)) {
                dat <- results[[i]][[sexes[j]]]
                if ((sum(dat$Sig == "") == 2) | is.null(sig_color)) {
                    cat("**", names(results)[i], "**", sep = "")
                } else {
                    cat("[**", names(results)[i], "**]{color=\"", sig_color, "\"}", sep = "")
                }
                print(knitr::kable(dat, ...))
                cat("\n\n")
            }
        }
    } else {
        stop("The number of sexes is not 1 or 2!")
    }
}
