# -------------------------------------------------------------------
# - NAME:        foehnix_functions.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-16
# -------------------------------------------------------------------
# - DESCRIPTION: Contains a set of S3 methods for the foehnix model
#                itself and some helper functions used within the
#                foehnix function.
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-16 18:00 on marvin
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Small helper function to check whether or not the matrix is
# standardized or not. Considered to be standardized if the matrix
# contains the two additional attributes "scaled:center" and
# "scaled:scale".
# -------------------------------------------------------------------
is.standardized <- function(x, ...) UseMethod("is.standardized")
is.standardized.matrix <- function(x, ...) inherits(x, "standardized")


# -------------------------------------------------------------------
# Standardize (model) matrix
# -------------------------------------------------------------------
standardize <- function(x, ...) UseMethod("standardize")
standardize.matrix <- function(x, ...) {
    # Scale covariates
    scaled_center <- structure(rep(0, ncol(x)), names = colnames(x))
    scaled_scale  <- structure(rep(1, ncol(x)), names = colnames(x))
    for ( i in 1:ncol(x) ) {
        if ( sd(x[,i], na.rm = TRUE) == 0 ) next
        scaled_center[i] <- mean(x[,i], na.rm = TRUE)
        scaled_scale[i]  <- sd(x[,i], na.rm = TRUE)
        x[,i] <- (x[,i] - scaled_center[i]) / scaled_scale[i]
    }
    attr(x, "scaled:center") <- scaled_center
    attr(x, "scaled:scale")  <- scaled_scale
    class(x) <- c("standardized", class(x))
    return(x)
}
scale.standardized <- function(x) return(attr(x, "scaled:scale"))
center <- function(x, ...) UseMethod("center")
center.standardized <- function(x) return(attr(x, "scaled:center"))


# -------------------------------------------------------------------
# Destandardize coefficients. Brings coefficients back to
# the "real" scale if standardized coefficients are used when
# estimating the logistic regression model (concomitant model).
# -------------------------------------------------------------------
destandardize_coefficients <- function(beta, X) {
    scaled_center = attr(X, "scaled:center")
    scaled_scale  = attr(X, "scaled:scale")
    # Do we have an intercept?
    ic <- grep("^\\(Intercept\\)$", rownames(beta))
    if ( length(ic) == 1 ) {
        # Descaling intercept
        beta[ic,]  <- beta[ic,] - sum(beta[-ic,] * scaled_center[-ic]  / scaled_scale[-ic])
        # Descaling all other regression coefficients
        beta[-ic,] <- beta[-ic,] / scaled_scale[-ic]
    } else {
        beta <- beta / scaled_scale
    }
    return(beta)
}


# TODO: DELETE ME
### -------------------------------------------------------------------
### Trying to estimate a reasonably high upper bound for lambda
### if ridge penalization is needed.
### -------------------------------------------------------------------
##get_lambdas <- function(nlambda, logitX, post, maxit, tol) {
##
##    # If nlambda is not a positive integer: stop.
##    stopifnot(inherits(nlambda, c("integer", "numeric")))
##    stopifnot(nlambda >= 0)
##
##    # Fitting logistic regression models with different lambdas.
##    csum_fun <- function(lambda, logitX, post, maxit, tol) {
##        # As response a first guess y >= median(y) is used.
##        # Force standardize = FALSE as logitX is already standardized if
##        # standardize == TRUE for this function.
##        m <- iwls_logit(logitX, post, standardize = FALSE,
##                        lambda = lambda, maxit = maxit, tol = tol)
##        sum(abs(m$beta[which(!grepl("^\\(Intercept\\)$", rownames(m$beta))),]))
##    }
##
##    # Find large lambda where all parameters are close to 0.
##    # Trying lambdas between exp(6) and exp(12).
##    lambdas <- exp(seq(5, 15, by = 1))
##    x <- sapply(lambdas, csum_fun, logitX = logitX, post = post, maxit = maxit, tol = tol)
##
##    # Pick the lambda where sum of parameters is smaller than a 
##    # certain threshold OR take maximum of lambdas tested.
##    lambdas <- exp(seq(min(which(x < 0.1), length(x)), -8, length = as.numeric(nlambda)))
##    cat(sprintf("Use penalization lambda within %.5f to %.5f\n", max(lambdas), min(lambdas)))
##    return(lambdas)
##}
# TODO: DELETE ME

# -------------------------------------------------------------------
# Information criteria: logLik, AIC, BIC, and effective degrees of
# freedom
# -------------------------------------------------------------------
logLik.foehnix <- function(x, ...)  structure(x$optimizer$loglik, names = "loglik")
AIC.foehnix <- function(x, ...)     structure(x$optimizer$AIC, names = "AIC")
BIC.foehnix <- function(x, ...)     structure(x$optimizer$BIC, names = "BIC")
edf <- function(x, ...) UseMethod("edf")
edf.foehnix <- function(x, ...)     structure(x$optimizer$edf, names = "edf")


# -------------------------------------------------------------------
# Estimated regression coefficients
# TODO: Print method for coef is missing.
# -------------------------------------------------------------------
coef.foehnix <- function(x, type = "parameter", ...) {
    type <- match.arg(type, c("parameter", "coefficient"))

    if ( type == "parameter" ) {
        rval <- rbind(matrix(c(x$coef$mu1, exp(x$coef$logsd1),
                              x$coef$mu2, exp(x$coef$logsd2)), ncol = 1,
                            dimnames = list(c("mu1", "sigma1", "mu2", "sigma2"), NULL)),
                     x$coef$concomitants)
    } else {
        rval <- rbind(matrix(c(x$coef$mu1, x$coef$logsd1,
                              x$coef$mu2, x$coef$logsd2), ncol = 1,
                            dimnames = list(c("mu1", "logsd1", "mu2", "logsd2"), NULL)),
                     x$coef$concomitants)
    }
    rval <- setNames(as.vector(rval), rownames(rval))
    
    # Appending some attributes and a new class
    attr(rval, "concomitants") <- ! is.null(x$coef$concomitants)
    attr(rval, "formula")      <- x$formula
    attr(rval, "family")       <- x$family
    class(rval) <- c("coef.foehnix", class(rval))
    rval
}
print.coef.foehnix <- function(x, ...) {
    cat("Coefficients of foehnix model\n")
    cat(sprintf("Model formula:           %s\n",
                paste(as.character(attr(x, "formula"))[c(2,1,3)], collapse = " ")))
    print(attr(x, "family"))
    if ( ! attr(x, "concomitants") ) {
        cat("No concomitant model in use\n")
    }
    cat("\nCoefficients\n")
    print(structure(as.numeric(x), names = names(x)))
}


# -------------------------------------------------------------------
# Model/classification summary
# -------------------------------------------------------------------
summary.foehnix <- function(x, ...) {

    rval <- list()
    rval$call    <- x$call
    rval$samples <- x$samples
    rval$coef    <- coef(x, type = "parameter")

    # Optimizer statistics
    rval$time      <- x$time
    rval$logLik    <- logLik(x)
    rval$edf       <- edf(x)
    rval$n.iter    <- x$optimizer$n.iter
    rval$maxit     <- x$optimizer$maxit
    rval$converged <- x$optimizer$converged

    # Mean estimated probability of foehn (climatological probability
    # of foehn based on the model estimate)
    rval$meanprob  <- 100 * mean(x$prob, na.rm = TRUE)
    rval$meanfoehn <- 100 * sum(x$prob >= .5, na.rm = TRUE) / sum(!is.na(x$prob))

    class(rval) <- "summary.foehnix"
    return(rval)
}
print.summary.foehnix <- function(x, ...) {

    # Model call
    cat("\nCall: "); print(x$call); cat("\n")

    # Coefficient information
    print(x$coef)

    # Additional information about the data/model
    cat(sprintf("\nNumber of observations (total) %8d\n", x$samples$total)) 
    cat(sprintf("Removed due to missing values  %8d (%3.1f percent)\n",
                x$samples$na,    100 * x$samples$na / x$samples$total)) 
    cat(sprintf("Outside defined wind sector    %8d (%3.1f percent)\n", 
                x$samples$wind,  100 * x$samples$wind / x$samples$total)) 
    cat(sprintf("Used for classification        %8d (%3.1f percent)\n",
                x$samples$taken, 100 * x$samples$taken / x$samples$total)) 
    cat(sprintf("\nClimatological foehn occurance %.2f percent\n", x$meanfoehn))
    cat(sprintf("Mean foehn probability %.2f percent\n", x$meanprob))

    cat(sprintf("\nLog-likelihood: %.1f on %d Df\n",   x$ll, x$df))
    cat(sprintf("Number of EM iterations %d/%d (%s)\n", x$n.iter, x$maxit,
                ifelse(x$converged, "converged", "not converged")))
    if ( x$time < 60 ) {
        cat(sprintf("Time required for model estimation: %.1f seconds\n", x$time))
    } else {
        cat(sprintf("Time required for model estimation: %.1f minutes\n", x$time / 60))
    }
}

