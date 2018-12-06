\name{destandardize_coefficients}
\alias{destandardize_coefficients}
\title{Destandardize Regression Coefficients}
\usage{
    destandardize_coefficients(beta, X)
}
\arguments{
    \item{beta}{matrix of dimension \code{p x 1} containing the
        regression coefficients on the standardized scale.}
    \item{X}{matrix of dimension \code{n x p}, the matrix used
        to train the statistical model (see Details).}
}
\details{
    If the regression model (e.g., logistic regression) is estimated
    on standardized covariates (see \code{\link{standardize_model_matrix}})
    the coefficients will be de-standardized to provide regression
    coefficients on the same scale as the original input data.

    \code{X} is the standardized model matrix as returned by
    \code{\link{standardize_model_matrix}} which, alongside with the
    standardized data, provides the scaling parameters as attributes.
    The attributes \code{scaled:center} and \code{scaled:scale} contain
    mean and standard deviation for all columns \code{p} of matrix \code{X}
    used for standardization.
}
\value{
    Returns a matrix of size \code{p x 1} (modified matrix \code{beta})
    with de-standardized regression coefficients.
}
    
