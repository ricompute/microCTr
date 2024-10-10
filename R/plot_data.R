#' Plot bone microCT data by genotype or treatment
#'
#' This function takes in bone microCT data and produces boxplots comparing two
#' genotypes or treatments.
#'
#' If the two genotypes or treatments are significantly different, statistical
#' significance will be indicated with `"*"` if *P* < 0.05, `"**"` if *P* <
#' 0.01, or `"***"` if *P* < 0.001.
#'
#' @param data A tibble/data frame containing bone microCT data, formatted as is
#'   the output of [read_trabecular_csv()], [read_cortical_csv()], or
#'   [read_mfe_csv()] (or `read_*_excel()`).
#' @param type A string indicating the type of data supplied. Options include
#'   `trabecular`, `cortical`, or `fea`. Alternatively, a specific microCT
#'   measure of interest can be specified (e.g., `Ct.vBMD`). Defaults to `NULL`,
#'   in which case the function will try to figure out which type of data it is.
#' @param title A string indicating what type of title the plots should have.
#'   Defaults to `sex`, which is currently the only option implemented. To
#'   remove titles from the plots, set `title` to `NULL`.
#'
#' @return A list containing [ggplot2::ggplot()] objects for sex each analyzed.
#'   To print each plot without printing the list index, see [print_plots()].
#' @export
#'
#' @examples
#' gen_key <- read_key_csv(mctr_ex("example-gen-key.csv"))
#' gen_cort <- read_cortical_csv(mctr_ex("example-twice1.csv"),
#'                           mctr_ex("example-twice2.csv"),
#'                           gen_key)
#' plot_genotypes(gen_cort |> dplyr::filter(Site == "Dia"))
#' tx_key <- read_key_csv(mctr_ex("example-tx-key.csv"))
#' tx_cort <- read_cortical_csv(mctr_ex("example-twice1.csv"),
#'                           mctr_ex("example-twice2.csv"),
#'                           tx_key)
#' plot_treatments(tx_cort |> dplyr::filter(Site == "Dia"))
plot_genotypes <- function(data, type = NULL, title = "sex") {
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
    } else if ((length(type) == 1) & (type %in% c(trabecular_measures,
                                                   cortical_measures,
                                                   fea_measures))) {
        measures <- type
    } else {
        stop("Type of data must be trabecular, cortical, or fea.",
             "Alternatively, one specific measure of interest can be supplied.")
    }

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
    plot_by_sex <- vector(mode = "list", length = length(sexes))
    names(res_by_sex) <- names(plot_by_sex) <- sexes


    for (s in sexes) {
        dat <- data |> dplyr::filter(Sex == s)
        res <- dplyr::tibble()

        for (m in measures) {

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
                               Measure = m,
                               Value = c(max(g1) + 1.5 * (stats::sd(g1) / sqrt(length(g1))),
                                         max(g2) + 1.5 * (stats::sd(g2) / sqrt(length(g2)))),
                               P = c(NA, t$p.value),
                               Sig = c("", sig))

            res <- dplyr::bind_rows(res,
                                    r)
        }
        res_by_sex[[s]] <- res
    }

    for (s in sexes) {
        dat <- data |>
            dplyr::filter(Sex == s) |>
            tidyr::pivot_longer(cols = dplyr::all_of(measures),
                                names_to = "Measure",
                                values_to = "Value")

        p <- dat |>
            ggplot2::ggplot(ggplot2::aes(x = Genotype, y = Value)) +
            ggplot2::facet_wrap(ggplot2::vars(factor(Measure,
                                                     levels = measures)),
                                scales = "free_y",
                                nrow = 1) +
            ggplot2::geom_boxplot() +
            ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.2)) +
            ggplot2::expand_limits(y = 0) +
            ggplot2::geom_text(data = res_by_sex[[s]],
                               mapping = ggplot2::aes(x = Genotype, y = Value,
                                                      label = Sig), size = 9)

        if (!is.null(title)) {
            if (title == "sex") {
                p <- p + ggplot2::ggtitle(paste("Sex:", s))
            }
        }

        plot_by_sex[[s]] <- p
    }
    plot_by_sex
}

#' @rdname plot_genotypes
#' @export
plot_treatments <- function(data, type = NULL, title = "sex") {
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
    } else if ((length(type) == 1) & (type %in% c(trabecular_measures,
                                                  cortical_measures,
                                                  fea_measures))) {
        measures <- type
    } else {
        stop("Type of data must be trabecular, cortical, or fea.",
             "Alternatively, one specific measure of interest can be supplied.")
    }

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
    plot_by_sex <- vector(mode = "list", length = length(sexes))
    names(res_by_sex) <- names(plot_by_sex) <- sexes


    for (s in sexes) {
        dat <- data |> dplyr::filter(Sex == s)
        res <- dplyr::tibble()

        for (m in measures) {

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
                               Measure = m,
                               Value = c(max(g1) + 1.5 * (stats::sd(g1) / sqrt(length(g1))),
                                         max(g2) + 1.5 * (stats::sd(g2) / sqrt(length(g2)))),
                               P = c(NA, t$p.value),
                               Sig = c("", sig))

            res <- dplyr::bind_rows(res,
                                    r)
        }
        res_by_sex[[s]] <- res
    }

    for (s in sexes) {
        dat <- data |>
            dplyr::filter(Sex == s) |>
            tidyr::pivot_longer(cols = dplyr::all_of(measures),
                                names_to = "Measure",
                                values_to = "Value")

        p <- dat |>
            ggplot2::ggplot(ggplot2::aes(x = Treatment, y = Value)) +
            ggplot2::facet_wrap(ggplot2::vars(factor(Measure,
                                                     levels = measures)),
                                scales = "free_y",
                                nrow = 1) +
            ggplot2::geom_boxplot() +
            ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.2)) +
            ggplot2::expand_limits(y = 0) +
            ggplot2::geom_text(data = res_by_sex[[s]],
                               mapping = ggplot2::aes(x = Treatment, y = Value,
                                                      label = Sig), size = 9)

        if (!is.null(title)) {
            if (title == "sex") {
                p <- p + ggplot2::ggtitle(paste("Sex:", s))
            }
        }

        plot_by_sex[[s]] <- p
    }
    plot_by_sex
}

#' Print a list of plots
#'
#' This function iterates through a list of plots and prints them (without
#' printing the list index).
#'
#' @param plots A list of [ggplot2::ggplot()] objects.
#'
#' @return Returns NULL, since this function is used for its side effects.
#' @export
#'
#' @examples
#' gen_key <- read_key_csv(mctr_ex("example-gen-key.csv"))
#' gen_cort <- read_cortical_csv(mctr_ex("example-twice1.csv"),
#'                           mctr_ex("example-twice2.csv"),
#'                           gen_key)
#' plot_genotypes(gen_cort |> dplyr::filter(Site == "Dia")) |> print_plots()
print_plots <- function(plots) {
    for (p in plots) {
        print(p)
    }
}
