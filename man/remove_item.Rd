% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_functions_utils.R
\name{remove_item}
\alias{remove_item}
\title{remove_item}
\usage{
remove_item(x = c(), item = c())
}
\arguments{
\item{x}{The vector}

\item{item}{The item to be removed}
}
\value{
The vector excluding the value
}
\description{
Removes a vector of items from a vector
}
\examples{
remove_item(colnames(temp_df), item = c("year"))
}
