\name{phoeton}
\alias{phoeton}
\title{Foehn Classification Based on a Two-Part Mixture Model}
\usage{
    phoeton( ... )
}
\arguments{
    \item{formula}{an object of class \code{formula} (or one that can be coerced to
          that class): a symbolic description of the model to be
          fitted.  The details of model specification are given under
          'Details'.}
    \item{data}{a time series object of class \code{zoo} containing the
        variables for the two-part mixture model.}
    \item{windsector}{either \code{NULL} or a \code{numeric} vector of length \code{2}.
        Defines the foehn wind sector used for filtering the data.
        Details can be found in the 'details' section.}
    \item{maxit}{control argument for the iterative solvers. Default is \code{100L},
        the maximum number of iterations for the EM algorithm and the IWLS backfitting
        algorithm for the concomitant model.
        If a vector of length two is provided the first value is used for the
        EM algorithm, the second for the IWLS backfitting.}
    \item{tol}{similar as for \code{maxit}. Used to identify convergence of the
        iterative solvers. Default is \code{1e-8}, if two values are given the
        first will be used for the EM algorithm, the second one for the IWLS backfitting
        procedure.}
    \item{lambda.min}{one of \code{"auto"} (default), \code{"loglik"}, \code{"BIC"}, or \code{"AIC"}.
        See details section for more information.}
        


}
\details{
    TODO: Details for formula

    TODO: Details for windsector

    Details for lambda. If \code{"auto"} the default is \code{"loglik"} except
    that the correlation between two or more covariates used in \code{formula}
    exceeds \code{0.75} (both positive or negative). In this case
    \code{lambda.min = "AIC"} will be used automatically with
    additional regularization.
    If \code{lambda.min = "loglik"} an unpenalized estimate will be returned.
    If \code{lambda.min} is either AIC or BIC a ridge penalty (L2 penalty)
    is used. A fixed set of penalties will be tested. The one minimising
    either the AIC or BIC criteria will be used for the foehn classification
    model.
}
\value{
    Returns an object of class \code{phoeton}. A set of 
    default and custom methods is available for these objects
    such as \code{print}, \code{summary}, \code{coef}, \code{plot}, \dots
}
