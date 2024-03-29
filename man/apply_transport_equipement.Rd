% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/5_apply_transport_equipement.R
\name{apply_transport_equipement}
\alias{apply_transport_equipement}
\title{apply_transport_equipement}
\usage{
apply_transport_equipement(MatisseData)
}
\arguments{
\item{MatisseData}{A MatisseData list of all the data from Matisse}
}
\value{
A MatisseData list
}
\description{
Applies the effect of transportation equipement (changes in personal car) to the MatisseData and
returns a spending tibble updated with the new spending from the equipement
}
\examples{
apply_transport_equipement(MatisseData)
}
