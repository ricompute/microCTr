#' Update microCTr
#'
#' This is a convenience function wrapper aroung [remotes::install_github()] to
#' install/update [microCTr]. Meant to be used as an RStudio addin.
#'
#' @export
#'
#' @examplesIf FALSE
#' update_microCTr()
update_microCTr <- function() {
    remotes::install_github("ricompute/microCTr")
}
