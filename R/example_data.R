#' Get path to microCTr examples
#'
#' microCTr comes bundled with some example files in its `inst/extdata`
#' directory. This function makes them easy to access.
#'
#' Adapted from [readxl::readxl_example()].
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#'
#' @return A string containing the name of the file, or a character vector
#'   containing the names of all example files.
#' @export
#'
#' @examples
#' microCTr_example()
#' microCTr_example("example-key.csv")
microCTr_example <- function(path = NULL) {
    if (is.null(path)) {
        dir(system.file("extdata", package = "microCTr"))
    } else {
        system.file("extdata", path, package = "microCTr", mustWork = TRUE)
    }
}
