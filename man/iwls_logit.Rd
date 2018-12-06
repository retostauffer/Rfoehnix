\name{iwls_logit}
\alias{iwls_logit}
\title{IWLS Solver for Binary Logistic Regression Model}
\usage{
    ## Basic usage
    iwls_solver(X, y)

    ## Available options
    iwls_solver(X, y, beta = NULL, lambda = NULL, standardize = TRUE,
                maxit = 100L, tol = 1e-8, ...)
}
\arguments{
    \item{X}{model matrix including intercept (if required).}
    \item{y}{binary response (vector with \code{0}s and \code{1}s).}
    \item{beta}{initial regression coefficients. If not set (\code{beta = NULL}, default),
        all coefficients will be initialized with \code{0}.}
    \item{lambda}{if set to \code{NULL} (default) no penalty is used during optimization.
        A \code{float} can be provided for regularization (ridge/L2 penalty).}
    \item{maxit}{integer, maximum number of iterations, default \code{100}.}
    \item{tol}{float, tolerance for the improvement to check for convergence.}
    \item{standardize}{logical. If set to \code{TRUE} (default) the model matrix
        containing the concomitant variables will be standardized.}
    \item{...}{currently unused.}
}
\value{
    Returns a matrix size \code{p x 1} where \code{p} is the
    number of coefficients (equal to the number of columns in
    the model matrix \code{X}.
}
\details{
    Iterative (re-)weighted least squares solver for logistic
    regression model. The basic call (\code{iwls_solver(X, y)})
    solves the unregularized problem.

    If \code{lambda} is set (\code{float}) a ridge penalty will be added
    to shrink the regression parameters.

    The logical option \code{standardize} controls whether or not the
    model matrix (covariates) should be standardized using Gaussian
    standartization (\code{(x - mean(x)) / sd(x)}) for all columns with
    non-constant data. It is recommended to use standardization
    (\code{standardize = TRUE}) to avoid numerical problems.

    \code{maxit} and \code{tol} allow to control the iterations of the
    IWLS solver. \code{maxit} is the maximum number of iterations allowed.
    \code{tol} is used to check the log-likelihood improvements. If the
    imrovements compared with the previous iteration falls below this
    tolerance the optimizer converged. If \code{maxit} is reached the
    solver will stop, even if not converged.
}
\seealso{
    \code{\link{destandardize_coefficients}},
    \code{\link{standardize_model_matrix}}.
}
\examples{
# Example data set
data("airquality")
airquality <- na.omit(airquality)
airquality$Ozone <- as.numeric(airquality$Ozone > 50)

# glm model
m1 <- glm(Ozone ~ ., data = airquality, family = binomial(link = "logit"))

# iwls solver
mf <- model.frame(Ozone ~ ., data = airquality)
m2 <- iwls_logit(model.matrix(Ozone ~ ., data = airquality), model.response(mf))

# Comparing coefficients
print(cbind(coef(m1), m2$coef))
}
