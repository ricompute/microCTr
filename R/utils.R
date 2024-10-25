#' Update microCTr
#'
#' This is a convenience function wrapper aroung [remotes::install_github()] to
#' install/update `microCTr`. Meant to be used as an RStudio addin.
#'
#' @export
#'
#' @examplesIf FALSE
#' update_microCTr()
update_microCTr <- function() {
    remotes::install_github("ricompute/microCTr")
}

# Housekeeping to silence note in R CMD check due to use of unquoted symbols in
# code using dplyr
utils::globalVariables(
    c(
        "AS",
        "BV/TV",
        "Ct.Ar",
        "Ct.Po",
        "Ct.Po.V",
        "Ct.Th",
        "Ct.V",
        "Ct.vBMD",
        "DT-Tb.N",
        "DT-Tb.Sp",
        "DT-Tb.Th",
        "End.Ar",
        "End.Circ",
        "End.Rad",
        "F.ult",
        "Genotype",
        "Group",
        "Mean2",
        "MeasNo",
        "Measure",
        "Peri.Ar",
        "Peri.Circ",
        "Peri.Rad",
        "Peri.V",
        "S",
        "SMI",
        "SampNo",
        "Sex",
        "Sig",
        "Site",
        "TRI-SMI",
        "TV.1",
        "TV.2",
        "Tb.N",
        "Tb.Sp",
        "Tb.Th",
        "Treatment",
        "VOX-BV",
        "VOX-BV/TV",
        "VOX-TV",
        "Value"
    )
)

# Use of these functions in the R Markdown template is integral to the package's
# core functionality
ignore_unused_imports <- function() {
    xfun::embed_file
    sessioninfo::session_info
}

