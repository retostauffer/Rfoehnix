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
# - L@ST MODIFIED: 2018-12-21 18:39 on marvin
# -------------------------------------------------------------------


#' Standardize (Model) Matrix
#'
#' Function to standardize the columns of a matrix, used to
#' standardize the model matrix before estimating the regression
#' coefficients of a generalized linear model (\code{\link{iwls_logit}}).
#'
#' @param x matrix of dimension \code{N x p}.
#' @param ... additional arguments, ignored.
#' @return Returns a matrix of the same dimension as input \code{x}
#' but with standardized data. The return object is of class
#' \code{c("standardized", "matrix")} which comes with some handy
#' S3 methods.
#'
#' @examples
#' # Example data set
#' data("airquality")
#' airquality <- na.omit(airquality)
#'
#' # Create model matrix
#' X <- model.matrix(Ozone ~ ., data = airquality)
#' print(head(X))
#'
#' # Standardize
#' S <- standardize(X)
#' print(head(S))
#'
#' is.standardized(X)
#' is.standardized(X)
#'
#' # Get parameters used for standardization
#' center(S)
#' scale(S)
#'
#' @author Reto Stauffer
#' @export
standardize <- function(x, ...) UseMethod("standardize")

#' @rdname standardize
#' @export
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


#' @rdname standardize
#' @export
scale.standardized <- function(x, ...) return(attr(x, "scaled:scale"))


#' @rdname standardize
#' @export
center <- function(x, ...) UseMethod("center")
#' @export
center.standardized <- function(x, ...) return(attr(x, "scaled:center"))

destandardize <- function(x, ...) UseMethod("destandardize")
destandardize.standardized <- function(x, ...) {
    center <- center(x)
    scale  <- scale(x)
    for ( i in seq_along(center) ) x[,i] <- x[,i] * scale[i] + center[i]
    matrix(x, ncol = ncol(x), nrow = nrow(x), dimnames = dimnames(x))
}



#' Check if Matrix is Standardized
#'
#' Helper function for \code{foehnix}. Returns \code{TRUE}
#' if input \code{x} is a standardized matrix, else \code{FALSE}.
#'
#' @param x a matrix or standardized matrix.
#' @param ... ignored.
#'
#' @seealso \code{\link{standardize}}.
#' @export
is.standardized <- function(x, ...) UseMethod("is.standardized")

#' @export
is.standardized.matrix <- function(x, ...) inherits(x, "standardized")



#' Destandardize Regression Coefficients
#'
#' Destandardize coefficients. Brings coefficients back to
#' the "real" scale if standardized coefficients are used when
#' estimating the logistic regression model (concomitant model).
#'
#' @param beta regression coefficients estimated on standardized data.
#' @param X object of class \code{\link{standardize}}.
#' @return Returns destandardized regression coefficients, same object
#' as input \code{beta}.
#'
#' @seealso \code{\link{standardize}}. Used in \code{\link{foehnix}}
#' and \code{\link{iwls_logit}}.
#'
#' @rdname standardize
#' @export
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


#' @rdname foehnix
#' @export
logLik.foehnix <- function(object, ...) structure(object$optimizer$loglik, names = "loglik")


#' @rdname foehnix
#' @export
AIC.foehnix <- function(object, ...)    structure(object$optimizer$AIC, names = "AIC")


#' @rdname foehnix
#' @export
BIC.foehnix <- function(object, ...)    structure(object$optimizer$BIC, names = "BIC")


#' Returns Effective Degrees of Freedom
#'
#' Function which returns the effective degrees of freedom.
#' @param object the object from which the effective degrees
#'        of freedom should be returned.
#' @param ... forwarded to class specific edf methods.
#' @export
edf <- function(object, ...) UseMethod("edf")

#' @rdname foehnix
#' @export
edf.foehnix <- function(object, ...)     structure(object$optimizer$edf, names = "edf")


#' @rdname foehnix
#' @export
print.foehnix <- function(object, ...) print(summary(object, ...))


#' Get Estimated Mixture Model Coefficients
#'
#' Returns the estimated coefficients of a \code{\link{foehnix}} mixture model
#' for both, the components and the concomitant model (if in use).
#'
#' @param object \code{\link{foehnix}} mixture model object.
#' @param type character, either \code{"parameter"} \code{"coefficients"}, see
#'        'Details' section.
#' @param ... additional arguments, ignored.
#' @return Returns a \code{coef.foehnix} object with the estimated
#'         coefficients.
#' @details Returns the coeffficients of the mixture model.
#' If \code{type = "parameter"} the parameters are returned of the
#' components are returned on the 'true' scale, namely:
#' \code{mu1} and \code{sigma1} (location and scale of component 1),
#' \code{mu2} and \code{sigma2} (location and scale of component 2),
#' and a set of coefficients from the concomitant model if one has been
#' specified. These coefficients are the coefficients from a (possibly
#' regularized) logistic regression model (see \code{\link{iwls_logit}}).
#'
#' If \code{type = "coefficients"} the scale parameters are returned
#' on the log-scale (\code{logsd1}, \code{logsd2}) as used during
#' optimization.
#'
#' @seealso \code{\link{foehnix}}, \code{\link{iwls_logit}}.
#'
#' @author Reto Stauffer
#' @export
coef.foehnix <- function(object, type = "parameter", ...) {

    # One of the two types: parameter (destandardized if required),
    # or coefficient (standardized coefficients if standardized was TRUE).
    type <- match.arg(type, c("parameter", "coefficient"))
    if ( type == "parameter" ) {
        rval <- rbind(matrix(c(object$coef$mu1, exp(object$coef$logsd1),
                               object$coef$mu2, exp(object$coef$logsd2)), ncol = 1,
                             dimnames = list(c("mu1", "sigma1", "mu2", "sigma2"), NULL)),
                      object$coef$concomitants)
    } else {
        rval <- rbind(matrix(c(object$coef$mu1, object$coef$logsd1,
                               object$coef$mu2, object$coef$logsd2), ncol = 1,
                             dimnames = list(c("mu1", "logsd1", "mu2", "logsd2"), NULL)),
                      object$coef$concomitants)
    }
    rval <- stats::setNames(as.vector(rval), rownames(rval))
    
    # Appending some attributes and a new class
    attr(rval, "concomitants") <- ! is.null(object$coef$concomitants)
    attr(rval, "formula")      <- object$formula
    attr(rval, "family")       <- object$control$family
    class(rval) <- c("coef.foehnix", class(rval))
    rval
}

#' @export
coefficients.foehnix <- function(object, type = "parameter", ...)
    coef.foehnix(object, type = type, ...)

#' @export
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


#' @rdname foehnix
#' @export
formula.foehnix <- function(object, ...) object$formula


#' @rdname foehnix
#' @export
summary.foehnix <- function(object, ...) {

    rval <- list()
    rval$call    <- object$call
    rval$prob    <- object$prob
    rval$coef    <- coef(object, type = "parameter")

    # Optimizer statistics
    rval$filter_obj <- object$filter_obj
    rval$time       <- object$time
    rval$logLik     <- logLik(object)
    rval$AIC        <- AIC(object)
    rval$BIC        <- BIC(object)
    rval$edf        <- edf(object)
    rval$n.iter     <- object$optimizer$n.iter
    rval$maxit      <- object$optimizer$maxit
    rval$converged  <- object$optimizer$converged

    class(rval) <- "summary.foehnix"
    return(rval)
}

#' @export
print.summary.foehnix <- function(x, ...) {

    # Model call
    cat("\nCall: "); print(x$call)

    sum_na <- sum(is.na(x$prob$flag))
    sum_0  <- sum(x$prob$flag == 0, na.rm = TRUE)
    sum_1  <- sum(x$prob$flag == 1, na.rm = TRUE)

    mean_occ  <- 100 * sum(x$prob$prob >= .5, na.rm = TRUE) / sum(!is.na(x$prob$flag))
    mean_n    <- sum(!is.na(x$prob$flag))
    mean_prob <- 100 * mean(x$prob$prob[!is.na(x$prob$flag)])

    # Additional information about the data/model
    cat(sprintf("\nNumber of observations (total) %8d\n", nrow(x$prob)))
    cat(sprintf("Removed due to missing values  %8d (%3.1f percent)\n",
                sum_na, sum_na / nrow(x$prob) * 100))
    cat(sprintf("Outside defined wind sector    %8d (%3.1f percent)\n", 
                sum_0, sum_0 / nrow(x$prob) * 100))
    cat(sprintf("Used for classification        %8d (%3.1f percent)\n",
                sum_1, sum_1 / nrow(x$prob) * 100))
    cat(sprintf("\nClimatological foehn occurance %.2f percent (on n = %d)\n", mean_occ, mean_n))
    cat(sprintf("Mean foehn probability %.2f percent (on n = %d)\n", mean_prob, mean_n))

    cat(sprintf("\nLog-likelihood: %.1f, %d effective degrees of freedom\n",   x$logLik, x$edf))
    cat(sprintf("Corresponding AIC = %.1f, BIC = %.1f\n", x$AIC, x$BIC))
    cat(sprintf("Number of EM iterations %d/%d (%s)\n", x$n.iter, x$maxit,
                ifelse(x$converged, "converged", "not converged")))
    if ( x$time < 60 ) {
        cat(sprintf("Time required for model estimation: %.1f seconds\n", x$time))
    } else {
        cat(sprintf("Time required for model estimation: %.1f minutes\n", x$time / 60))
    }
}

