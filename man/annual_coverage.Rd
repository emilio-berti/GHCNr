% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coverage.R
\name{annual_coverage}
\alias{annual_coverage}
\title{Calculate Annual Coverage}
\usage{
annual_coverage(x)
}
\arguments{
\item{x}{Object of class \code{ghcn_daily}. See \code{\link[=daily]{daily()}} for details.}
}
\value{
A table with annual coverage.
}
\description{
Calculate Annual Coverage
}
\details{
Calculates the annual coverage of one station.
}
\examples{
cleaned <- remove_flagged(CA003076680)
cover <- annual_coverage(cleaned)
cover
}
