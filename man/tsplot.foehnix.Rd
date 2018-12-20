\name{tsplot}
\alias{tsplot}
\alias{tsplot.foehnix}
\title{Time Series Plot of foehnix Models}
\description{
    Development time series plots of estimated \code{\link{foehnix}}
    models. TODO: they are very specific at the moment!
}
\usage{
    ## Basic usage, loops trough the time series used
    ## to estimate the models.
    tsplot(x)
    ## Specify where to start/end
    tsplot(x, start, end)
    ## Change the loop-interval
    tsplot(x, ndays = ndays)
    ## Add an extra zoo object with probabilities within
    ## [0,1] for comparison
    tsplot(x, xtra = xtra)
    ## Full options
    tsplot(x, start = NULL, end = NULL, ndays = 10, ..., xtra = NULL, ask = TRUE)
}
\arguments{
    \item{x}{object of type \code{foehnix}.}
    \item{start}{POSIXt object or an object which can be converted to POSIXt.}
    \item{end}{POSIXt object or an object which can be converted to POSIXt.}
    \item{ndays}{integer, number of days used when looping trough the time series.}
    \item{xtra}{optional zoo object with probabilities in \code{]0,1[} to compare
        two classification algorithms.}
    \item{ask}{logical, default is \code{TRUE}.}
}
\details{
    Development method to access plausability of the estimated foehn proabilities.
    For software release this method should either be removed or made much
    more general. At the moment the method heavily depends on the names of the
    data used as input for \code{\link{fohenix}}.
}
