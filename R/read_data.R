read_key_csv <- function(file, ...) {
    readr::read_csv(file, ...) |>
        tidyr::pivot_longer(cols = c("Spine", "Met", "Dia"),
                            names_to = "Site",
                            values_to = "MeasNo")
}
