% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data.R
\name{read_cortical_csv}
\alias{read_cortical_csv}
\alias{read_cortical_excel}
\title{Read in cortical bone data}
\usage{
read_cortical_csv(twice1_file, twice2_file, key, ...)

read_cortical_excel(
  data_file,
  key,
  twice1_sheet = "Twice1",
  twice2_sheet = "Twice2",
  ...
)
}
\arguments{
\item{twice1_file}{The file path to the \code{twice1.csv} file (for
\code{\link[=read_cortical_csv]{read_cortical_csv()}}).}

\item{twice2_file}{The file path to the \code{twice2.csv} file (for
\code{\link[=read_cortical_csv]{read_cortical_csv()}}).}

\item{key}{The \code{key} object containing sample information, as created by the
\code{\link[=read_key_csv]{read_key_csv()}} or \code{\link[=read_key_excel]{read_key_excel()}} function.}

\item{...}{Additional arguments passed on to \code{\link[readr:read_delim]{readr::read_csv()}} or
\code{\link[readxl:read_excel]{readxl::read_excel()}}.}

\item{data_file}{The path to the \code{data.xlsx} file (for
\code{\link[=read_cortical_excel]{read_cortical_excel()}}).}

\item{twice1_sheet, twice2_sheet}{For \code{\link[=read_cortical_excel]{read_cortical_excel()}}, the sheets to
read. Either strings (the name of the sheets), or integers (the positions
of the sheets). Default to \code{"Twice1"} and \code{"Twice2"}.}
}
\value{
A tibble/data frame containing cortical bone data. For example:

\if{html}{\out{<div class="sourceCode">}}\preformatted{|   AS|Sex |Genotype | SampNo|Site | MeasNo|  Ct.vBMD| Ct.Th| End.Circ| Peri.Circ| Ct.Po| Ct.Po.V|
|----:|:---|:--------|------:|:----|------:|--------:|-----:|--------:|---------:|-----:|-------:|
| 1365|M   |Cre      |  10778|Met  |  31710|  937.049| 0.129|    5.533|     6.342|  0.41|   0.002|
| 1365|M   |Cre      |  10778|Dia  |  31712| 1114.873| 0.220|    3.963|     5.343|  0.11|   0.001|
| 1366|F   |Cre      |  10779|Met  |  31713| 1011.473| 0.183|    4.337|     5.485|  0.26|   0.001|
| 1366|F   |Cre      |  10779|Dia  |  31715| 1133.066| 0.232|    3.250|     4.705|  0.10|   0.000|
| 1367|F   |Cre;fl   |  10780|Met  |  31716|  994.868| 0.145|    4.892|     5.806|  0.37|   0.002|
| 1367|F   |Cre;fl   |  10780|Dia  |  31718| 1137.300| 0.209|    3.514|     4.827|  0.14|   0.001|
}\if{html}{\out{</div>}}
}
\description{
These are wrappers around \code{\link[readr:read_delim]{readr::read_csv()}} and \code{\link[readxl:read_excel]{readxl::read_excel()}} to
read in and process cortical bone microCT data from two files: a \code{twice1.csv}
and a \code{twice2.csv} file, or the \code{Twice1} and \code{Twice2} sheets of a \code{data.xlsx}
file. It uses a \code{key} object to look up and add sex, genotype, and site
information.
}
\examples{
gen_key <- read_key_csv(mctr_ex("example-gen-key.csv"))
gen_cort <- read_cortical_csv(mctr_ex("example-twice1.csv"),
                              mctr_ex("example-twice2.csv"),
                              gen_key)
gen_cort <- read_cortical_excel(mctr_ex("example-genotype.xlsx"),
                                gen_key)
}
