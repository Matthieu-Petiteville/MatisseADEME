% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_functions_summary.R
\name{summarise_poverty}
\alias{summarise_poverty}
\title{summarise_poverty}
\usage{
summarise_poverty(
  MatisseData,
  scenario,
  price_deflator = 1,
  pondref = F,
  use_mean_threshhold = F
)
}
\arguments{
\item{MatisseData}{A MatisseData list}

\item{scenario}{A scenario}

\item{price_deflator}{The price deflator for scenario}
}
\value{
A tibble of the poverty rate
}
\description{
Calculates the poverty rate for a MatisseData. Definition at 60\% of the median RDB/UC
}
\examples{
summarise_poverty(MatisseData, scenario)
}
