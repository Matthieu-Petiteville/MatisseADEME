% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_functions_summary.R
\name{summarise_housephys}
\alias{summarise_housephys}
\title{summarise_housephys}
\usage{
summarise_housephys(
  MatisseData,
  price_vol_df,
  scenario,
  split_by = c(),
  subset_idx = c(),
  ener_vec = c("AutreEner", "Elec", "Fioul", "Gaz", "Solide")
)
}
\arguments{
\item{MatisseData}{A MatisseData list}

\item{price_vol_df}{A price vol tibble}

\item{scenario}{A scenario}

\item{split_by}{A column of menage to split}

\item{subset_idx}{An index of lines to be included}
}
\value{
A tibble with the sum of spendings on house energyu  by effect
}
\description{
Summarises the house spending per effect
}
\examples{
summarise_housephys(MatisseData, price_vol_df, scenario, split_by = c(), subset_idx = c())
}
