\name{hpd}
\alias{hpd}
\alias{hpd.coda}

\title{Computing Highest Posterior Density (HPD) Intervals}

\description{
Compute approximate HPD intervals out of MCMC-samples in BayesX
}

\usage{
hpd(data, alpha = 0.05, ...)
hpd.coda(data, alpha = 0.05, ...)
}

\arguments{
  \item{data}{Either the name of a file or a data frame containing the sample.}
  \item{alpha}{A numeric scalar in the interval (0,1) such that 1 - \code{alpha} is the target probability contained in the intervals.}
  \item{\dots}{Further parameters to be passed to the internal call of \code{optim} such has \code{start} and \code{integrate} such as \code{lower}.}
}

\details{
  \code{hpd} computes the HPD interval based on a kernel density estimate of the samples.
  \code{hpd.coda} computes the HPD interval with the function \code{HPDinterval} available in package \code{coda}.
}

\author{Nadja Klein}

\examples{
res <- read.table(system.file("examples/nonparametric_f_x_pspline_sample.raw",
                              package="BayesX"), header=TRUE)
hpd(res)
hpd.coda(res)
}
