% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2_adjust_budget.R
\name{project_savings}
\alias{project_savings}
\title{project_savings}
\usage{
project_savings(MatisseData, sav_proj)
}
\arguments{
\item{MatisseData}{A MatisseData list of all the data from Matisse}

\item{sav_proj}{The projected values for saving rate}
}
\value{
A saving_rate dataframe
}
\description{
This function projects the saving rate dataframe to year_hor, based on the initial savingrate, the projected values (proj_sav)
and the column for savings (Savings or SavingsExDurable)
}
\examples{
project_savings(MatisseData, sav_proj)
}
