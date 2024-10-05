#' Read in a sample definition key file
#'
#' @param file The file path to the `key.csv` file.
#' @param ... Additional arguments passed on to `readr::read_csv()`.
#'
#' @return A tibble/data frame containing the sample definitions.
#' @export
#'
#' @examples
#' key <- read_key_csv("data/key.csv")
read_key_csv <- function(file, ...) {
    readr::read_csv(file, ...) |>
        tidyr::pivot_longer(cols = c("Spine", "Met", "Dia"),
                            names_to = "Site",
                            values_to = "MeasNo")
}
