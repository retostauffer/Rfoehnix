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
\description{
    Iterative weighted least squares solver for a logistic regression
    model. Used by \code{\link{foehnix.unreg.fit}} to estimate the
    concomitant model of the two-component foehnix mixture models.
}
\arguments{
    \item{X}{model matrix including intercept (if required).}
    \item{y}{response, can be binary or probabilities (values in \code{]0,1[}).}
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

    Returns a list with a set of values and information about optimization.
    \itemize{
        \item \code{lambda} value used (or \code{NULL}).
        \item \code{edf} effective degrees of freedom. Equal to \code{P}
            (\code{ncol(X)}) if no regularization is used.
        \item \code{loglik} final log-likelihood sum of the model.
        \item \code{AIC} Akaike information criterium.
        \item \code{BIC} Bayesian information criterium.
        \item \code{converged} logical flag whether or not the algorithm
            converged (see \code{maxit}, \code{tol}).
        \item \code{beta} matrix of dimension \code{P x 1} containing the
            estimated and possibly standardized regression coefficients
            (see input \code{standardize}). If input \code{standardize = FALSE}
            \code{beta == coef}.
        \item \code{coef} matrix of dimension \code{P x 1} containing the
            destandardized regression coefficients. 
    }
}
\details{
    Iterative (re-)weighted least squares solver for logistic regression model.
    The basic call (\code{iwls_solver(X, y)}) solves the unregularized problem.
    Input matrix \code{X} is the design matrix containing the concomitant
    variables for the logistic regression model. Matrix is of dimension
    \code{N x P} where \code{N} is the number of of observations, \code{P} the
    number of concomitant variables (including the intercept, if required). If
    more than one column contains constant values the script will throw an
    error (solution no more identifiable).  \code{y} is the binary response
    vector of length \code{N} containing \code{0}s and \code{1}s.

    \code{beta} can be used to specify the initial regression parameters.  If
    not set (\code{beta = NULL}; default) all parameters will be initialized
    with \code{0}s.

    If \code{lambda} is set (\code{float}) a ridge penalty will be added to
    shrink the regression parameters.

    The logical option \code{standardize} controls whether or not the model
    matrix (covariates) should be standardized using Gaussian standartization
    (\code{(x - mean(x)) / sd(x)}) for all columns with non-constant data. It
    is recommended to use standardization (\code{standardize = TRUE}) to avoid
    numerical problems.

    \code{maxit} and \code{tol} allow to control the iterations of the IWLS
    solver. \code{maxit} is the maximum number of iterations allowed.
    \code{tol} is used to check the log-likelihood improvements. If the
    imrovements compared with the previous iteration falls below this tolerance
    the optimizer converged. If \code{maxit} is reached the solver will stop,
    even if not converged.
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

# Setting up model.frame, response, and model matrix
mf <- model.frame(Ozone ~ ., data = airquality)
X  <- model.matrix(Ozone ~ ., data = airquality)
y  <- model.response(mf)

# Default call
m2 <- iwls_logit(X, y)
# With standardized coefficients
m3 <- iwls_logit(X, y, standardize = TRUE)
# No early stop, stop when maxit = 100 is reached. Will through
m4 <- iwls_logit(X, y, standardize = TRUE, tol = -Inf, maxit = 100)

# Comparing coefficients
print(cbind(coef(m1), m2$coef, m3$coef, m4$coef))
}
