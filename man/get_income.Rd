% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_get_budget_part.R
\name{get_income}
\alias{get_income}
\title{get_income}
\usage{
get_income(MatisseData)
}
\arguments{
\item{MatisseData}{A MatisseData list of all the data from Matisse}
}
\value{
Returns a table with an IDENT_MEN column and all the incomes for all the households
}
\description{
This function extracts the table of incomes from the menage dataframe under the partition given by transco_rev
}
\examples{
get_income(MatisseData)
}
