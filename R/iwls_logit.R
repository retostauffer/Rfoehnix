# -------------------------------------------------------------------
# - NAME:        iwls_logit.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-11-28
# -------------------------------------------------------------------
# - DESCRIPTION: Iterative weighted least squares solver for logistic
#                regression models. Used to estimate the concomitant
#                models in 'foehnix'.
# -------------------------------------------------------------------
# - EDITORIAL:   2018-11-28, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-20 17:46 on marvin
# -------------------------------------------------------------------

#' IWLS Solver for Binary Logistic Regression Model
#' 
#' Iterative weighted least squares solver for a logistic regression
#' model. Used by \code{\link{foehnix.unreg.fit}} to estimate the
#' concomitant model of the two-component foehnix mixture models.
#' 
#' @param X model matrix including intercept (if required).
#' @param y response, can be binary or probabilities (values in \code{]0,1[}).
#' @param beta initial regression coefficients. If not set (\code{beta = NULL}, default),
#'        all coefficients will be initialized with \code{0}.
#' @param lambda if set to \code{NULL} (default) no penalty is used during optimization.
#'        A \code{float} can be provided for regularization (ridge/L2 penalty).
#' @param maxit integer, maximum number of iterations, default \code{100}.
#' @param tol float, tolerance for the improvement to check for convergence.
#' @param standardize logical. If set to \code{TRUE} (default) the model matrix
#'        containing the concomitant variables will be standardized.
#' @param verbose logical, if set to \code{FALSE} output is suppressed.
#' @param ... currently unused.
#' @param object,which control the \code{coef} method.
#' @return Returns an object of class \code{ccmodel} (concomitant model).
#' The object contains the following information:
#' 
#' \itemize{
#'     \item \code{lambda} value used (or \code{NULL}).
#'     \item \code{edf} effective degrees of freedom. Equal to \code{P}
#'         (\code{ncol(X)}) if no regularization is used.
#'     \item \code{loglik} final log-likelihood sum of the model.
#'     \item \code{AIC} Akaike information criteria.
#'     \item \code{BIC} Bayesian information criteria.
#'     \item \code{converged} logical flag whether or not the algorithm
#'         converged (see \code{maxit}, \code{tol}).
#'     \item \code{beta} matrix of dimension \code{P x 1} containing the
#'         estimated and possibly standardized regression coefficients
#'         (see input \code{standardize}). If input \code{standardize = FALSE}
#'         \code{beta == coef}.
#'     \item \code{coef} matrix of dimension \code{P x 1} containing the
#'         destandardized regression coefficients. 
#' }
#' 
#' @details
#' Iterative (re-)weighted least squares solver for logistic regression model.
#' The basic call (\code{iwls_solver(X, y)}) solves the unregularized problem.
#' Input matrix \code{X} is the design matrix containing the concomitant
#' variables for the logistic regression model. Matrix is of dimension
#' \code{N x P} where \code{N} is the number of of observations, \code{P} the
#' number of concomitant variables (including the intercept, if required). If
#' more than one column contains constant values the script will throw an
#' error (solution no more identifiable).  \code{y} is the binary response
#' vector of length \code{N} containing \code{0}s and \code{1}s.
#' 
#' \code{beta} can be used to specify the initial regression parameters.  If
#' not set (\code{beta = NULL}; default) all parameters will be initialized
#' with \code{0}s.
#' 
#' If \code{lambda} is set (\code{float}) a ridge penalty will be added to
#' shrink the regression parameters.
#' 
#' The logical option \code{standardize} controls whether or not the model
#' matrix (covariates) should be standardized using Gaussian standardization
#' (\code{(x - mean(x)) / sd(x)}) for all columns with non-constant data. It
#' is recommended to use standardization (\code{standardize = TRUE}) to avoid
#' numerical problems.
#' 
#' \code{maxit} and \code{tol} allow to control the iterations of the IWLS
#' solver. \code{maxit} is the maximum number of iterations allowed.
#' \code{tol} is used to check the log-likelihood improvements. If the
#' improvements compared with the previous iteration falls below this tolerance
#' the optimizer converged. If \code{maxit} is reached the solver will stop,
#' even if not converged.
#' 
#' @seealso
#' \code{\link{destandardize_coefficients}},
#' \code{\link{standardize}}, \code{\link{is.standardized}}
#' 
#' @examples
#' # Example data set
#' data("airquality")
#' airquality <- na.omit(airquality)
#' airquality$Ozone <- as.numeric(airquality$Ozone > 50)
#' 
#' # glm model
#' m1 <- glm(Ozone ~ ., data = airquality, family = binomial(link = "logit"))
#' 
#' # Setting up model.frame, response, and model matrix
#' mf <- model.frame(Ozone ~ ., data = airquality)
#' X  <- model.matrix(Ozone ~ ., data = airquality)
#' y  <- model.response(mf)
#' 
#' # Default call
#' m2 <- iwls_logit(X, y)
#' # With standardized coefficients
#' m3 <- iwls_logit(X, y, standardize = TRUE)
#' # No early stop, stop when maxit = 100 is reached. Will through
#' m4 <- iwls_logit(X, y, standardize = TRUE, tol = -Inf, maxit = 100)
#' 
#' # Comparing coefficients
#' print(cbind(coef(m1), m2$coef, m3$coef, m4$coef))
#'
#' @author Reto Stauffer
#' @export
iwls_logit <- function(X, y, beta = NULL, lambda = NULL, standardize = TRUE,
                       maxit = 100L, tol = 1e-8, verbose = FALSE, ...) {

    # NAns?
    if ( any(is.na(X)) ) stop("Input \"X\" contains NA values. Stop.")
    if ( any(is.na(y)) ) stop("Input \"y\" contains NA values. Stop.")

    # Checking inputs. Constant covariates (concomitant variables)
    # are not allowed except one column (intercept).
    if(sum(apply(X, 2, sd) == 0) > 1) stop("Multiple columns with constant values!")
    if ( min(y) < 0 | max(y) > 1 ) stop("y values out of range. Have to be within ]0,1[.")

    # Standardize design matrix?
    if ( standardize ) X <- standardize(X)

    # initialize regression coefficients if needed
    if ( is.null(beta) ) beta <- rep.int(0, ncol(X)) # FIXME: there is surely a better solution!

    # If all y in {0, 1} this is the binary response and we have to
    # calculate the initial values for the linear predictor and the
    # response (probabilities). If y in [0,1] these are already
    # probabilities.
    # Calculate linear predictor eta
    eta <- drop(X %*% beta)
    # Apply link function on linear predictor to get response mu (probabilities)
    mu  <- plogis(eta) 

    iter   <- 0L
    llpath <- list()
    while( iter < maxit ) {
        # Increase iteration counter
        iter <- iter + 1L

        # New weights
        w <- sqrt(mu * (1 - mu)) + 1e-10
        if( is.null(lambda) ) { reg <- 0 } else { reg <- diag(ncol(X)) * lambda; reg[1,1] <- 0 }
        beta <- solve(t(X*w) %*% (X*w) + reg) %*% t(X*w) %*% (eta * w + (y - mu) / w)
        #beta <- matrix(lm.fit(X * w, eta * w + (y - mu)/w)$coefficients, ncol = 1)

        # Update latent response eta (X^\top \beta)
        eta  <- drop(X %*% beta)

        # Update response (probabilities)
        mu   <- plogis(eta)

        # Update log-likelihood sum
        llpath[[iter]]   <- sum(y * eta - log(1 + exp(eta)))

        # Continue if iter == 1 and/or is.null(tol)
        if ( iter == 1 | is.null(tol) ) next

        if ( verbose) cat(sprintf("Iteration %d, ll = %15.4f, %s\r", iter, ll,
            ifelse(is.null(lambda), "unregularized", sprintf("lambda = %10.4f", lambda))))

        # Check if improvement falls below tolerance
        if ( (llpath[[iter]] - llpath[[iter - 1]]) < tol ) break

    }
    llpath <- structure(do.call(rbind, llpath), dimnames = list(NULL, "loglik"))

    # Not converged? Drop warning.
    if ( ! is.null(tol) & iter == maxit ) warning("IWLS solver for logistic model did not converge.")

    # Just naming the column containing the coefficients.
    colnames(beta) <- c("concomitant")

    # Calculate effective degrees of freedom
    if ( is.null(lambda) ) { reg <- 0 } else { reg <- diag(ncol(X)) * lambda; reg[1,1] <- 0 }
    edf <- sum(diag(t(X*w) %*% (X*w) %*% solve(t(X*w) %*% (X*w) + reg)))

    # Unscale coefficients if needed
    ll   <- tail(llpath, 1)
    rval <- list(lambda = lambda, edf = edf, loglik = ll, AIC = -2 * ll + 2 * edf,
                 BIC = -2 * ll + log(nrow(X)) * edf,
                 converged = ifelse(iter < maxit, TRUE, FALSE))
    rval$beta <- beta
    rval$coef <- if ( standardize ) destandardize_coefficients(beta, X) else beta

    # Return list object containing
    # - edf (numeric): effective degrees of freedom
    # - loglik (numeric): log-likelihood of the model
    # - converged (logical): flag whether or not the iterative solver converged
    # - beta/coef (matrix): standardized and de-standardized coefficients. If
    #       input "standardized = FALSE" beta and coef are identical.
    class(rval) <- "ccmodel"
    return(rval)
}

#' @rdname iwls_logit
#' @export
coef.ccmodel <- function(object, which = "coef", ...) {
    which <- match.arg(which, c("coef", "beta"))
    c <- as.data.frame(t(object[[which]][,1]))
    names(c) <- sprintf("cc.%s", names(c))
    c
}


