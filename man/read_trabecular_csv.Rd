% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data.R
\name{read_trabecular_csv}
\alias{read_trabecular_csv}
\alias{read_trabecular_excel}
\title{Read in trabecular bone data}
\usage{
read_trabecular_csv(file, key, ...)

read_trabecular_excel(file, key, sheet = "Trabecular", ...)
}
\arguments{
\item{file}{The file path to the \code{trabecular.csv} or \code{data.xlsx} file.}

\item{key}{The \code{key} object containing sample information, as created by the
\code{\link[=read_key_csv]{read_key_csv()}} or \code{\link[=read_key_excel]{read_key_excel()}} function.}

\item{...}{Additional arguments passed on to \code{\link[readr:read_delim]{readr::read_csv()}} or
\code{\link[readxl:read_excel]{readxl::read_excel()}}.}

\item{sheet}{For \code{\link[=read_trabecular_excel]{read_trabecular_excel()}}, the sheet to read. Either a
string (the name of a sheet), or an integer (the position of the sheet).
Defaults to \code{"Trabecular"}.}
}
\value{
A tibble/data frame containing trabecular bone data. For example:

\if{html}{\out{<div class="sourceCode">}}\preformatted{|   AS|Sex |Genotype | SampNo|Site  | MeasNo| BV/TV|   SMI|   Tb.N| Tb.Th| Tb.Sp|
|----:|:---|:--------|------:|:-----|------:|-----:|-----:|------:|-----:|-----:|
| 1365|M   |Cre      |  10778|Spine |  31711| 13.05| 1.631|  3.097| 0.051| 0.316|
| 1365|M   |Cre      |  10778|Met   |  31710|  5.43| 3.227|  2.460| 0.062| 0.412|
| 1366|F   |Cre      |  10779|Spine |  31714| 21.88| 0.987| 14.227| 0.047| 0.111|
| 1366|F   |Cre      |  10779|Met   |  31713|  4.47| 3.725|  2.666| 0.059| 0.371|
| 1367|F   |Cre;fl   |  10780|Spine |  31717| 14.47| 0.989|  2.551| 0.060| 0.398|
| 1367|F   |Cre;fl   |  10780|Met   |  31716|  0.23| 4.172|  1.789| 0.034| 0.558|
}\if{html}{\out{</div>}}
}
\description{
These are wrappers around \code{\link[readr:read_delim]{readr::read_csv()}} and \code{\link[readxl:read_excel]{readxl::read_excel()}} to
read in and process trabecular bone microCT data from a \code{trabecular.csv} file
or the \code{Trabecular} sheet of a \code{data.xlsx} file. It uses a \code{key} object to
look up and add sex, genotype, and site information.
}
\examples{
gen_key <- read_key_csv(mctr_ex("example-gen-key.csv"))
gen_trab <- read_trabecular_csv(mctr_ex("example-trabecular.csv"),
                                        key = gen_key)
gen_trab <- read_trabecular_excel(mctr_ex("example-genotype.xlsx"),
                                          key = gen_key)
}
