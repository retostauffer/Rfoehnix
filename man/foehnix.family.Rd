\name{foehnix.family}
\alias{foehnix.family}
\alias{foehnix_gaussian}
\alias{foehnix_cgaussian}
\alias{foehnix_tgaussian}
\alias{foehnix_logistic}
\alias{foehnix_clogistic}
\alias{foehnix_tlogistic}
\title{Families for the foehinx Two-Component Mixture Models}
\usage{
    # Two-component Gaussian (with censored and truncated version)
    foehnix_gaussian()
    foehnix_cgaussian(left = -Inf, right = Inf)
    foehnix_tgaussian(left = -Inf, right = Inf)

    # Two-component logistic (with censored and truncated version)
    foehnix_logistic()
    foehnix_clogistic(left = -Inf, right = Inf)
    foehnix_tlogistic(left = -Inf, right = Inf)
}
\description{
    \code{\link{foehnix}} uses a set of family objects of class
    \code{foehnix.family} to estimate the mixture models.
    
    A \code{foehnix.family} object consists of a set of functions
    such as the probability density function, cumulative distribution
    function, log-likelihood function, or the function to calculate the
    posterior probabilities of the mixture model.
}
