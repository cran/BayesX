\name{extractSamples}
\alias{extractSamples}

\title{Extract MCMC samples from a BayesX results directory}
\description{
  This is a convenience function to extract samples from a BayesX
  results directory, which processes the log file to e.g. convert the
  spline coefficients samples to function values samples.
}
\usage{
extractSamples(directoryWithBasename, 
               logfile = file.path(dirname(directoryWithBasename), "log.txt"))
}

\arguments{
  \item{directoryWithBasename}{The BayesX results directory with
  basename for the files (e.g. "results/test", if this was specified as
  outfile in BayesX for the bayesreg object)}
  \item{logfile}{The log file of the MCMC run, defaults to log.txt in
  the results directory.}
}
\value{
  Returns a list with the extracted samples of effects and deviances as
  well as the prediction data.frame:
  \item{<function name>}{for P-Splines, Random Walks and spatial
  effects: a list with mcmc objects 'functionSamples' and
  'varianceSamples' containing the respective effects/function and
  variance parameter samples.} 
  \item{FixedEffects}{an mcmc object of all fixed simple parametric effects}
  \item{Deviance}{an mcmc object with the (unstandardized and saturated)
    deviance}
  \item{PredictMeans}{data.frame corresponding to the possible
    predictmean file in the BayesX directory}
}

\author{Daniel Sabanes Bove}
\section{Warning}{
You should be sure that only one MCMC run is saved in the given results
directory in order to get sensible results out of this function.
}

% the na.rm=TRUE for quantile is only needed for R CMD check ?!? Crazy...
\examples{
## get the samples
samples <- extractSamples(file.path(system.file("examples/samples", package="BayesX"),
                                     "res"))
str(samples)

## check deviance convergence
plot(samples$Deviance)

## fixed parametric effects
plot(samples$FixedEffects)

## nonparametric effects:

## handy plot function to get means and pointwise credible intervals
nonpPlot <- function(samplesMatrix,
                     ...)
{
    x <- as.numeric(colnames(samplesMatrix))

    yMeans <- colMeans(samplesMatrix)
    yCredible <- t(apply(samplesMatrix,
                         MARGIN=2,
                         FUN=quantile,
                         prob=c(0.025, 0.975),
                         na.rm=TRUE))
    
    matplot(x, cbind(yMeans, yCredible),
            type="l",
            lty=c(1, 2, 2),
            lwd=c(2, 1, 1),
            col=c(1, 2, 2),
            ...)
}

nonpPlot(samples$f_x1$functionSamples,
         xlab=expression(x[1]),
         ylab=expression(hat(f)(x[1])))
nonpPlot(samples$f_x2$functionSamples,
         xlab=expression(x[2]),
         ylab=expression(hat(f)(x[2])))

## spatial effect
tanzania <- read.bnd(file=system.file("examples/tanzania.bnd", package="BayesX"))
drawmap(map=tanzania,
        data=
        with(samples$f_district,
             data.frame(name=colnames(functionSamples),
                        estimate=colMeans(functionSamples))),
        regionvar="name",
        plotvar="estimate")
}

\keyword{file}

