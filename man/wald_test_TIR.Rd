% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estTIR_comparegroups.R
\name{wald_test_TIR}
\alias{wald_test_TIR}
\title{Wald-type Test for Comparing Mean TIRs Across Groups}
\usage{
wald_test_TIR(estimates, boot_TIR_list)
}
\arguments{
\item{estimates}{A numeric vector of TIR estimates for each group (length \eqn{K}).}

\item{boot_TIR_list}{A list of numeric vectors of bootstrap estimates for each group (each of length \eqn{B}).}
}
\value{
A list with components:
\describe{
  \item{statistic}{The Wald test statistic (numeric).}
  \item{df}{Degrees of freedom (K - 1).}
  \item{p.value}{P-value based on the chi-squared distribution.}
}
}
\description{
Performs a Wald-type hypothesis test for the null hypothesis that the mean Time-in-Range (TIR)
values are equal across multiple groups, using bootstrap estimates.

The null hypothesis is:
\deqn{H_0: \mu_{W,1} = \mu_{W,2} = \ldots = \mu_{W,K}}

The test statistic is constructed as:
\deqn{\widehat{D}^\top \widehat{\Sigma}^{-1} \widehat{D}}
where \eqn{\widehat{D}} is the vector of differences in TIR estimates relative to group 1,
and \eqn{\widehat{\Sigma}} is the covariance matrix of the bootstrapped differences.
}
\examples{
\dontrun{
set.seed(123)
est <- c(0.75, 0.68, 0.72)
boot_TIR <- list(
  rnorm(100, 0.75, 0.02),
  rnorm(100, 0.68, 0.03),
  rnorm(100, 0.72, 0.025)
)
wald_test_TIR(est, boot_TIR)
}
}
