% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_function.R
\name{make_filename}
\alias{make_filename}
\title{Make Filename}
\usage{
make_filename(year)
}
\arguments{
\item{year}{A numeric vector of length 1}
}
\value{
A character string with format "accident_year.csv.bz2"
}
\description{
This function takes a year as input and returns a
string filename with the format "accident_y.csv.bz2" with y as the
input converted to an integer.
}
\examples{
\dontrun{
make_filename(2000)
make_filename(2012)
}

}
