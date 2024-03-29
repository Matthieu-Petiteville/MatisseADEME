% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/5_apply_housing_equipement.R
\name{is_eligible_renov}
\alias{is_eligible_renov}
\title{is_eligible_renov}
\usage{
is_eligible_renov(MatisseData, year)
}
\arguments{
\item{MatisseData}{A MatisseData list of all the data from Matisse}

\item{year}{The year of application of the filter}
}
\value{
A elig_renov tibble
}
\description{
A function that applies the rules to say if a given household can renovate its home
Current rules : household is either renting except free rent and usufructuary or proprietary with
a minimum 10\% value for the ratio of Savings to Income
}
\examples{
is_eligible_renov(MatisseData, year_it)
}
