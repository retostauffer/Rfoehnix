\name{foehnix.unreg.fit}
\alias{foehnix.unreg.fit}
\title{Fitting Unregularized foehnix Mixture Model With Concomitant Model.}
\usage{
    foehnix.unreg.fit(y, logitX, family, maxit = 100L,
                      tol = 1e-5, verbose = TRUE, alpha = NULL, ...)
}
\description{
    Fitting method for two-component foehnix mixture model with
    additional concomitants.
    Typically not called directly, interfaced via \code{\link{foehnix}}.
}
\arguments{
    \item{y}{numeric vector, covariate for the components of the
        mixture model, dimension \code{N}.}
    \item{logitX}{numeric matrix of dimension \code{N x P}, covariates
        for the concomitant model (logistic regression model matrix).}
    \item{family}{object of class \code{\link{foehnix.family}}.}
    \item{maxit}{positive integer, or vector of length 2 with positive
        integer values. Maximum number of iterations of the EM algorithm
        and the concomitant model.
        Check manual of \code{\link{foehnix.control}} for more details.} 
    \item{tol}{numeric, or vector of length 2 containing numeric values.
        Tolerance for the optimization (EM algorithm and concomitant model).
        Check manual of \code{\link{foehnix.control}} for more details.} 
    \item{verbose}{logical, default \code{TRUE}. If set to \code{FALSE}
        verbose output will be suppressed.}
    \item{alpha}{#TODO: currently unused.}
    \item{\dots}{additional arguments, unused.}
}
\seealso{
    \code{\link{foehnix}},
    \code{\link{foehnix.control}},
    \code{\link{foehnix.noconcomitant.fit}},
    \code{\link{foehnix.reg.fit}},
    \code{\link{iwls_logit}}.
}
