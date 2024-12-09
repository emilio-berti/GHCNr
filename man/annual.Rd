% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annual.R
\name{annual}
\alias{annual}
\title{Calculate Annual Summaries}
\usage{
annual(x)
}
\arguments{
\item{x}{Table of the daily timeseries.}
}
\value{
A tibble with the annual timeseries at the stations.
}
\description{
Calculate Annual Summaries
}
\details{
\emph{x} is the table returned from \code{get_ghcn_daily()} or
\code{remove_flagged()} or any subset of them.
}
\examples{
annual(CA003076680)
}
