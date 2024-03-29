% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_functions_summary.R
\name{summarise_enerphys_quartile}
\alias{summarise_enerphys_quartile}
\title{summarise_enerphys_quartile}
\usage{
summarise_enerphys_quartile(
  MatisseData,
  price_vol_df,
  scenario,
  split_by = c(),
  subset_idx = c(),
  ener_vec = c("Carbu", "Elec", "ElecVeh", "Fioul", "Gaz", "Solide", "AutreEner")
)
}
\arguments{
\item{MatisseData}{A MatisseData list}

\item{price_vol_df}{A price_vol tibble}

\item{scenario}{A scenario}

\item{split_by}{A split vector}

\item{subset_idx}{A subset of MatisseData}
}
\value{
A tibble of physical energy by quantiles (Q0 -> Q4)
}
\description{
summarise_enerphys_quartile
}
\examples{
summarise_enerphys_quartile(MatisseData, price_vol_df, scenario, split_by = c(), subset_idx = c())
}
