plot_genotypes <- function(data, type = NULL) {
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
    } else if ((length(type) == 1) & ((type %in% trabecular_measures) |
                                      (type %in% cortical_measures) |
                                      (type %in% fea_measures))) {
        measures <- type
    } else {
        stop("Type of data must be trabecular, cortical, or fea.",
             "Alternatively, one specific measure of interest can be supplied.")
    }

    # res <- vector(mode = "list", length = length(measures))
    # names(res) <- measures

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
                               Value = c(max(g1) + 1.5 * (sd(g1) / sqrt(length(g1))),
                                         max(g2) + 1.5 * (sd(g2) / sqrt(length(g2)))),
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
                                scales = "free_y") +
            ggplot2::geom_boxplot() +
            ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.2)) +
            ggplot2::expand_limits(y = 0) +
            ggplot2::geom_text(data = res_by_sex[[s]],
                               mapping = ggplot2::aes(x = Genotype, y = Value,
                                                      label = Sig), size = 9)

        plot_by_sex[[s]] <- p
    }
    plot_by_sex
}
