% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_get_budget_part.R
\name{get_durable}
\alias{get_durable}
\title{get_durable}
\usage{
get_durable(MatisseData)
}
\arguments{
\item{MatisseData}{A MatisseData list of all the data from Matisse}
}
\value{
Returns a standard spending dataframe
}
\description{
This function extracts from a c05 standard dataframe the big spendings (car, house) of the household from the BDF agregation to the
Matisse agregation level. Uses mostly the aggregate_spending function that is used elsewhere
}
\examples{
get_durable(MatisseData)
}
