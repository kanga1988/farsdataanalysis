% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/farsdataanalysis.R
\name{fars_read_years}
\alias{fars_read_years}
\title{fars_read_years}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{Vector of years' FARS files to open.
Valid vector element will be integer,string or numeric that can be coerced to integer.}
}
\value{
This function returns a list of tibbles with two variables : MONTH and year
Error handling using tryCatch to validate year input and return NULL for invalid year
}
\description{
\code{fars_read_years} produces a list of tibbles of FARS data as per input vector of years.
}
\details{
This function takes a vector of years and produces a list of tibbles,
  where each tibble is that year's FARS file year and MONTH observations.
  This function produce an output that retain only year and month from the original FARS dataset.
  To work, this function require a valid year as input argument. otherwise it will return a list with element value is NULL.
}
\examples{
\dontrun{
fars_read_years(years = c(2013, 2014, 2015))
}
}
