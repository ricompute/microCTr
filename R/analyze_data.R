get_sig <- function(t_test) {
    if (t_test$p.value < 0.001) {
        sig <- "***"
    } else if (t_test$p.value < 0.01) {
        sig <- "**"
    } else if (t_test$p.value < 0.05) {
        sig <- "*"
    } else {
        sig <- ""
    }
    sig
}

trabecular_measures <- c("BV/TV", "SMI", "Tb.N", "Tb.Th", "Tb.Sp")
cortical_measures <- c("Ct.vBMD",
                       "Ct.Th",
                       "End.Circ",
                       "Peri.Circ",
                       "Ct.Po",
                       "Ct.Po.V")
fea_measures <- c("S", "F.ult")

#' Compare microCT data between two genotypes or treatments
#'
#' This function takes in trabecular, cortical, or finite element analysis bone
#' microCT data at one site and compares it between two genotypes or two
#' treatments using a Student's *t*-test.
#'
#' If `trabecular` bone data are supplied, the measures compared include:
#'  - `BV/TV`
#'  - `SMI`
#'  - `Tb.N`
#'  - `Tb.Th`
#'  - `Tb.Sp`
#'
#' If `cortical` bone data are supplied, the measures compared include:
#'  - `Ct.vBMD`
#'  - `Ct.Th`
#'  - `End.Circ`
#'  - `Peri.Circ`
#'  - `Ct.Po`
#'  - `Ct.Po.V`
#'
#' If finite element analysis (`fea`) data are supplied, the measures compared
#' include:
#'  - `S`
#'  - `F.ult`
#'
#' @param data Trabecular bone microCT data in a data frame, formatted as is the
#'   output of [read_trabecular_csv()], [read_cortical_csv()], or
#'   [read_mfe_csv()] (or `read_*_excel()`). Note that data for only one Site
#'   can be supplied at a time.
#' @param type A string indicating the type of data supplied. Options include
#'   `trabecular`, `cortical`, or `fea`. Defaults to `NULL`, in which case the
#'   function will try to figure out which type of data it is.
#'
#' @return A list of each bone measure analyzed. Each measure is itself a list
#'   of each sex analyzed. Each sex is a tibble/data frame containing columns
#'   for `Sex`, `Genotype` (or `Treatment`), `n`, `Mean`, `SEM`, `P`, and `Sig`
#'   (a string containing `"*"` if *P* < 0.05, `"**"` if *P* < 0.01, or `"***"`
#'   if
#'   *P* < 0.001).
#' @export
#'
#' @examples
#' gen_key <- read_key_csv(mctr_ex("example-gen-key.csv"))
#' gen_trab <- read_trabecular_csv(mctr_ex("example-trabecular.csv"),
#'                             gen_key)
#' Spine.Tb <- gen_trab |> dplyr::filter(Site == "Spine") |>
#'                 compare_genotypes()
#' tx_key <- read_key_csv(mctr_ex("example-tx-key.csv"))
#' tx_trab <- read_trabecular_csv(mctr_ex("example-trabecular.csv"),
#'                                tx_key)
#' Spine.Tb <- tx_trab |> dplyr::filter(Site == "Spine") |>
#'                 compare_treatments()
compare_genotypes <- function(data, type = NULL) {
    if (is.null(type)) {
        if ("Tb.N" %in% names(data)) {
            measures <- trabecular_measures
        } else if ("Ct.vBMD" %in% names(data)) {
            measures <- cortical_measures
        } else if ("F.ult" %in% names(data)) {
            measures <- fea_measures
        } else {
            stop("Cannot figure out what type of data this is! ",
                 "Please specify type as trabecular, cortical, or fea.")
        }
    } else if (type == "trabecular") {
        measures <- trabecular_measures
    } else if (type == "cortical") {
        measures <- cortical_measures
    } else if (type == "fea") {
        measures <- fea_measures
    } else {
        stop("Type of data must be trabecular, cortical, or fea.")
    }

    res <- vector(mode = "list", length = length(measures))
    names(res) <- measures

    genotypes <- data$Genotype |> unique()
    if (length(genotypes) != 2) {
        stop("There are not 2 genotpyes!")
    }

    sites <- data$Site |> unique()
    if (length(sites) != 1) {
        stop("This function expects there to be one Site in the data supplied.")
    }

    sexes <- data$Sex |> unique()
    res_by_sex <- vector(mode = "list", length = length(sexes))
    names(res_by_sex) <- sexes

    for (m in measures) {
        for (s in sexes) {
            dat <- data |> dplyr::filter(Sex == s)

            g1 <- dat |>
                dplyr::filter(Genotype == genotypes[1]) |>
                dplyr::pull(var = m)
            g2 <- dat |>
                dplyr::filter(Genotype == genotypes[2]) |>
                dplyr::pull(var = m)

            if (stats::var.test(g1, g2)$p.val < 0.05) {
                t <- stats::t.test(g1, g2)
            } else {
                t <- stats::t.test(g1, g2, var.equal = TRUE)
            }

            sig <- get_sig(t)

            r <- dplyr::tibble(Sex = s,
                               Genotype = genotypes,
                               n = c(length(g1), length(g2)),
                               Mean = c(mean(g1), mean(g2)),
                               SEM = c(stats::sd(g1) / sqrt(length(g1)),
                                       stats::sd(g2) / sqrt(length(g2))),
                               P = c(NA, t$p.value),
                               Sig = c("", sig))

            res_by_sex[[s]] <- r
        }
        res[[m]] <- res_by_sex
    }
    res
}

#' @rdname compare_genotypes
#' @export
compare_treatments <- function(data, type = NULL) {
    if (is.null(type)) {
        if ("Tb.N" %in% names(data)) {
            measures <- trabecular_measures
        } else if ("Ct.vBMD" %in% names(data)) {
            measures <- cortical_measures
        } else if ("F.ult" %in% names(data)) {
            measures <- fea_measures
        } else {
            stop("Cannot figure out what type of data this is! ",
                 "Please specify type as trabecular, cortical, or fea.")
        }
    } else if (type == "trabecular") {
        measures <- trabecular_measures
    } else if (type == "cortical") {
        measures <- cortical_measures
    } else if (type == "fea") {
        measures <- fea_measures
    } else {
        stop("Type of data must be trabecular, cortical, or fea.")
    }

    res <- vector(mode = "list", length = length(measures))
    names(res) <- measures

    treatments <- data$Treatment |> unique()
    if (length(treatments) != 2) {
        stop("There are not 2 treatments!")
    }

    sites <- data$Site |> unique()
    if (length(sites) != 1) {
        stop("This function expects there to be one Site in the data supplied.")
    }

    sexes <- data$Sex |> unique()
    res_by_sex <- vector(mode = "list", length = length(sexes))
    names(res_by_sex) <- sexes

    for (m in measures) {
        for (s in sexes) {
            dat <- data |> dplyr::filter(Sex == s)

            g1 <- dat |>
                dplyr::filter(Treatment == treatments[1]) |>
                dplyr::pull(var = m)
            g2 <- dat |>
                dplyr::filter(Treatment == treatments[2]) |>
                dplyr::pull(var = m)

            if (stats::var.test(g1, g2)$p.val < 0.05) {
                t <- stats::t.test(g1, g2)
            } else {
                t <- stats::t.test(g1, g2, var.equal = TRUE)
            }

            sig <- get_sig(t)

            r <- dplyr::tibble(Sex = s,
                               Treatment = treatments,
                               n = c(length(g1), length(g2)),
                               Mean = c(mean(g1), mean(g2)),
                               SEM = c(stats::sd(g1) / sqrt(length(g1)),
                                       stats::sd(g2) / sqrt(length(g2))),
                               P = c(NA, t$p.value),
                               Sig = c("", sig))

            res_by_sex[[s]] <- r
        }
        res[[m]] <- res_by_sex
    }
    res
}
