% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pptd.R
\name{pptd}
\alias{pptd}
\title{CDF of Phase-Type distribution}
\usage{
pptd(u, pi, S)
}
\arguments{
\item{u}{Vector of quantiles}

\item{pi}{Initial distribution}

\item{S}{Subintensity matrix}
}
\value{
Returns the probability
}
\description{
CDF of Phase-Type distribution
}
\examples{
pptd(0.5, 1, -2)
}
