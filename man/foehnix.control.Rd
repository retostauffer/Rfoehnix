\name{foehnix.control}
\alias{foehnix.control}
\title{foehnix Two-Component Mixture-Model Control Object}
\description{
    Used to control the \code{\link{foehnix}} mixture models.
}
\usage{
    # Typically not used directly, can be used to specify the control
    # arguments for the foehnix model estimates.
    foehnix.control(family, left = -Inf, right = Inf, truncated = FALSE,
                    standardize = TRUE, maxit = 100L, tol = 1e-8,
                    alpha = NULL, verbose = FALSE, ...)

    # S3 methods for foehnix.control
    print(x, ...)
}
\arguments{
    \item{family}{character specifying the distribution of the components
        in the mixture model. Allowed: \code{"gaussian"} and \code{"logistic"}.
        For experts: custom \code{foehnix.family} objects can
        be provided as well.}
    \item{left}{default is \code{-Inf}, left censoring or truncation point.
        See also input \code{right} and input \code{truncated}. Can be set
        to any finite numeric value.}
    \item{right}{default is \code{Inf}, right censoring or truncation point.
        See also input \code{left} and input \code{truncated}. Can be set
        to any finite numeric value.}
    \item{truncated}{logical. If set to \code{TRUE} truncation is used instead of
        \code{censored}. This only affects the \code{\link{foehnix}} model estimate
        if input \code{left} and/or input \code{right} are specified.}
    \item{standardize}{logical flag, default is \code{TRUE}. Defines whether or not
        the model matrix for the concomitant model should be standardized for 
        model estimation. Recommended.}
    \item{maxit}{control argument for the iterative solvers. Default is \code{100L},
        the maximum number of iterations for the EM algorithm and the IWLS backfitting
        algorithm for the concomitant model.
        If a vector of length two is provided the first value is used for the
        EM algorithm, the second for the IWLS backfitting.}
    \item{tol}{similar as for \code{maxit}. Used to identify convergence of the
        iterative solvers. Default is \code{1e-8}, if two values are given the
        first will be used for the EM algorithm, the second one for the IWLS backfitting
        procedure. If set to \code{-Inf} \code{maxit} will be used as stopping
        criteria.}
    \item{alpha}{TODO alpha parameter for the penalization of the concomitant model.}
    \item{...}{currently set to hell.}
}
\seealso{
    \code{\link{foehnix}}.
}
