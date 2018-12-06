\name{standardize_model_matrix}
\alias{standardize_model_matrix}
\title{Standardize Model Matrix}
\usage{
    standardize_model_matrix(X)
}
\arguments{
    \item{X}{matrix to be standardized. See details.}
}
\value{
    Returns a matrix of the same size as input \code{X}
    with standardized data. In addition, two attributes
    will be set, namely \code{scaled_center} and \code{scaled_scale},
    named vectors of length \code{ncol(X)} containing mean (center) and
    standard deviation (scale) used to standardize the data.
    These attributes are important to re-scale the regression coefficients.
}
\details{
    It is recommended to use scaled data when solving the linear system of
    equations of a generalized linear model. Thus, the model matrix
    containing the concomitants (covariates) for the concomitant
    model will be standardized using Gaussian moments.

    \code{X} is a matrix of dimension \code{n x p} where \code{n}
    is the number of observations, \code{p} the number of covariates.
    All columns which are not constant (the model intercept)
    will be standardized using \code{(x - mean(x))/sd(x)}.
}
\seealso{
    \code{\link{destandardize_coefficients}},
    \code{\link{iwls_logit}}
}
