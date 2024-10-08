% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example_data.R
\name{mctr_ex}
\alias{mctr_ex}
\title{Get path to microCTr examples}
\usage{
mctr_ex(path = NULL)
}
\arguments{
\item{path}{Name of file. If \code{NULL}, the example files will be listed.}
}
\value{
A string containing the name of the file, or a character vector
containing the names of all example files.
}
\description{
microCTr comes bundled with some example files in its \code{inst/extdata}
directory. This function makes them easy to access.
}
\details{
Adapted from \code{\link[readxl:readxl_example]{readxl::readxl_example()}}.
}
\examples{
mctr_ex()
mctr_ex("example-gen-key.csv")
}
