% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_read_files.R
\name{get_income_proj}
\alias{get_income_proj}
\title{get_income_proj}
\usage{
get_income_proj(years)
}
\arguments{
\item{years}{The projected year for which we want the buckets}
}
\value{
Return a tibble of buckets by year of projection, and type of incomes
}
\description{
Return a tibble with the projections of incomes
}
\examples{
get_income_proj(years = c(2017,2030))
}
