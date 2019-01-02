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
# - L@ST MODIFIED: 2019-01-02 15:35 on marvin
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
print.foehnix <- function(x, ...) print(summary(object, ...))


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
formula.foehnix <- function(x, ...) x$formula


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

#' foehnix Image Plot - Hoevmoeller Diagram
#'
#' The default \code{\link{image}} plot of a \code{\link{foehnix}} object
#' is a Hoevmoeller diagram.
#'
#' @param x object of class \code{\link{foehnix}}.
#' @param FUN character string or a custom aggregation function. See 'Details'
#'        section for more information.
#' @param contours logical \code{TRUE} or \code{FALSE}, whether or not the
#'        concours should be added.
#' @param deltat integer, interval in seconds for the time of day axis. Has to be
#'        a fraction of 86400 (24 hours in seconds). It \code{NuLL} (default) the
#'        interval of the time series object will be used.
#' @param ... additional arguments, currently unused.
#'
#' @details Plotting a Hoevmoeller diagram based on the \code{\link{zoo}} time
#' series object of the \code{\link{foehnix}} classification. Different plot
#' types are available. The default functions (see list below) use \code{na.rm = TRUE}.
#'
#' Input \code{FUN} can be one of the following character strings:
#' \itemize{
#'    \item \code{mean}: mean probability.
#'    \item \code{freq}: plotting frequencies.
#'    \item \code{occ}: plotting occurance of foehn (where probability \code{>= 0.5}).
#'    \item \code{noocc}: contrary to \code{occ}: occurance of no foehn (probability \code{< 0.5}).
#' }
#'
#' \code{FUN} can also be a custom function used for time series aggregation
#' (see \code{\link{aggregate.zoo}}).
#' 
#' @export
image.foehnix <- function(x, FUN = "freq", contours = FALSE, deltat = NULL, ...) {

    x <- x$prob
    stopifnot(is.regular(x, strict = TRUE))
    index(x) <- as.POSIXct(index(x))
    if ( is.null(deltat) )
        deltat <- as.numeric(diff(index(x)[1:2]), unit = "secs")
    if ( ! round(86400 / deltat) * deltat == 86400 )
        stop(sprintf("deltat = %d is not a fraction of 86400 (one day in seconds).", deltat))

    # Aggregation function
    if ( is.character(FUN) ) {
        if ( FUN == "mean" ) {
            FUN <- function(x) mean(x, na.rm = TRUE)
        } else if ( FUN == "occ" ) {
            FUN <- function(x) sum(x >= 0.5, na.rm = TRUE)
        } else if ( FUN == "noocc" ) {
            FUN <- function(x) sum(x <  0.5, na.rm = TRUE)
        } else if ( FUN == "freq" ) {
            FUN <- function(x) sum(x >= 0.5, na.rm = TRUE) / sum(!is.na(x))
        }
    }

    # Create a long form of the matrix. A data.frame with three columns
    # containing "time" (HHMM as integer, e.g., 01:00 gets 100, 00:00 is set to 2400),
    # "yday" containing the day of the year (January 1 is 0), and "prob", the
    # foehn probability from the classification.
    longform <- function(x, secs) {
        lt   <- as.POSIXlt(ceiling(as.numeric(index(x)) / secs) * secs, origin = "1970-01-01")
        time <- lt$hour
        yday <- ifelse(lt$hour == 0, lt$yday + 1, lt$yday)
        yday <- ifelse(yday < 365, yday, 0)
        data.frame(time = time, yday = yday, prob = x$prob)
    }
    data <- longform(x, deltat)

    # Aggregate the data given "time" and "yday" from "data"
    by <- sprintf("%d_%03d", data$time, data$yday)
    data <- aggregate(data$prob, by = list(by), FUN = FUN)
    names(data) <- c("hash", "prob")

    # Deparsing the information again
    data$time <- as.integer(regmatches(data$hash, regexpr("^[0-9]+",   data$hash)))
    data$yday <- as.integer(regmatches(data$hash, regexpr("[0-9]{3}$", data$hash)))

    # Create a matrix to take up the aggregated values
    dim_time <- sort(unique(data$time))
    dim_yday <- sort(unique(data$yday))
    mat <- matrix(NA, nrow = length(dim_yday), ncol = length(dim_time), dimnames = list(dim_yday, dim_time))

    # Fill in the data
    mat[cbind(sprintf("%d", data$yday), sprintf("%d", data$time))] <- data$prob

    # Format "time" to "HH:MM"
    tmp <- as.integer(colnames(mat))
    tmp <- list(hour = floor(tmp / 100), min = tmp - 100 * floor(tmp / 100))
    colnames(mat) <- sprintf("%02d:%02d", ifelse(tmp$hour == 24, 0, tmp$hour), tmp$min) 
    # Note: origin _has to be_ a Monday!
    rownames(mat) <- strftime(as.Date(as.integer(rownames(mat)), origin = "2018-01-01"), "%b %d")

    # Format yday as "Mon d". Must be a shift year
    data$date <- strftime(as.Date(data$yday, origin = "2016-01-01"), "%b %d")
    
    # Plotting method
    plotmat <- function(mat, contours = FALSE, ...) {
        # Step one: we have to extend the matrix to be able to plot
        # nice contours. We do it as well even if concour is set to FALSE.
        orig_dim <- dim(mat)
        mat <- rbind(tail(mat,2), mat, head(mat,2))
        mat <- cbind(mat[,c(-1,0)+ncol(mat)], mat, mat[,1:2])

        # Calculate x and y limits
        ylim <- c(1 / (2 * (ncol(mat) - 1))) * 3; ylim <- c(ylim,1 - ylim)
        xlim <- c(1 / (2 * (nrow(mat) - 1))) * 3; xlim <- c(xlim,1 - xlim)
        print(xlim)

        image(mat, ..., xaxt = "n", xaxs = "i", xlim = xlim,
                        yaxt = "n", yaxs = "i", ylim = ylim)
        if ( contours ) contour(mat, add = TRUE)

        xat  <- seq(par()$usr[1L], par()$usr[2L], length = nrow(mat) - 4L)
        xidx <- seq.int(3L, nrow(mat) - 2L, length = 12)
        axis(side = 1, at = xat[xidx], labels = rownames(mat)[xidx])
        browser()

        yat <- seq(par()$usr[3L], par()$usr[4L], length = ncol(mat) - 4 + 1)
        yidx <- c(ncol(mat)-2, seq.int(3L, ncol(mat) - 2L))
        axis(side = 2, at = yat, labels = colnames(mat)[yidx])

    }
    plotmat(mat, contours = contours, col = rev(gray.colors(20))) 

    return(data)

}
