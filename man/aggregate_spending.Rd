% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_functions_Matisse.R
\name{aggregate_spending}
\alias{aggregate_spending}
\title{aggregate_spending}
\usage{
aggregate_spending(init_df, from, to, cat = c(), level = 4, silent = F)
}
\arguments{
\item{init_df}{A dataframe of the C05 format, with colnames expressed in the 'from' classification and an IDENT_MEN column}

\item{from}{The classification to sum from. Has to have the same name as the transco_sect column}

\item{to}{The classification to sum to. Has to have the same name as the transco_sect column}

\item{cat}{The categories in the 'to' format that we want to filter. Based on the 'toCat' column from transco_sect.
If empty, take every line.}

\item{level}{The level used for the filtering. Defaults at 4, the chosen level of agregation}
}
\value{
Returns a new df in the format of init_df but summed by 'to' classification
}
\description{
This function agregate the content from the init_df dataframe from the 'from' code to the 'to' code, based on the transco_sect file.
For every category in the 'to' classification, we extract the categories of the 'from'classification and sum it.
}
\examples{
aggregate_spending(c05, "BDF", "Matisse")
}
