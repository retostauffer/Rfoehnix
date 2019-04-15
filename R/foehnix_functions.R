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
# - L@ST MODIFIED: 2019-04-15 13:57 on marvin
# -------------------------------------------------------------------

utils::globalVariables(c("time_mid", "yday_mid", "value"))

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

#' Destandardize a Standardized Matrix
#'
#' Reverse function of \code{\link[foehnix]{standardize}}.
#'
#' @param x standardized matrix of class \code{standardized}.
#' @param ... forwarded, unused.
#'
#' @seealso \code{\link[foehnix]{standardize}}
#' @author Reto Stauffer
#' @export
destandardize <- function(x, ...) UseMethod("destandardize")

#' @export
#' @rdname standardize
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
print.foehnix <- function(x, ...) print(summary(x, ...))


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
summary.foehnix <- function(object, detailed = FALSE, ...) {

    rval <- list()
    rval$call     <- object$call
    rval$inflated <- object$inflated
    rval$prob     <- object$prob
    rval$coef     <- coef(object, type = "parameter")

    # Optimizer statistics
    rval$filter_obj <- object$filter_obj
    rval$time       <- object$time
    rval$logLik     <- logLik(object)
    rval$AIC        <- AIC(object)
    rval$BIC        <- BIC(object)
    rval$edf        <- edf(object)
    rval$n.iter     <- object$optimizer$n.iter
    rval$maxit      <- object$optimizer$maxit
    rval$detailed   <- detailed
    if ( detailed ) {
        rval$converged  <- object$optimizer$converged
        rval$mu.se      <- object$mu.se
    }

    # Appending concomitant model
    rval$ccmodel    <- object$optimizer$ccmodel

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
    if (x$inflated) {
        cat(sprintf("\nNumber of observations (total) %8d (%d due to inflation)\n",
                    nrow(x$prob), x$inflated))
    } else {
        cat(sprintf("\nNumber of observations (total) %8d (no inflation)\n",
                    nrow(x$prob), x$inflated))
    }
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

    # If "detailed" has been set to "TRUE" we will also print the
    # test statistics of the coefficients of the mixture model.
    if ( x$detailed ) {
        # Summary statistics for the components
        tmp <- matrix(NA, ncol = 4, nrow = 2, dimnames = list(
                      c("(Intercept).1", "(Intercept).2"),
                      c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
        # Store coefficients on "Estimate"
        # Store standard deviation of the coefficients on "Std. Error"
        tmp[,"Estimate"]   <- c(x$coef["mu1"], x$coef["mu2"])
        tmp[,"Std. Error"] <- as.numeric(x$mu.se)
        # Calculate t value and the corresponding p value based on
        # a Gaussian or t-test
        tmp[,"t value"]    <- tmp[,"Estimate"] / tmp[,"Std. Error"]
        tmp[,"Pr(>|t|)"]   <- 2 * pnorm(0, tmp[,"t value"])

        # Show information
        cat("\n---------------------------------\n\n")
        cat("Components: t test of coefficients\n")
        printCoefmat(tmp, P.values = TRUE, has.Pvalues = TRUE)

        # If there is a concomitant model: show estimated coefficients and
        # z statistics.
        if ( ! is.null(x$ccmodel) ) {
            cat("\n---------------------------------\n")
            summary(x$ccmodel)
        }
    } else {
        cat("\nUse summary(object, detailed = TRUE) to get additional test statistics.\n")
    }
}

#' foehnix Image Plot - Hovmoeller Diagram
#'
#' The default \code{\link{image}} plot of a \code{\link{foehnix}} object
#' is a Hovmoeller diagram.
#'
#' @param x object of class \code{\link{foehnix}}.
#' @param FUN character string or a custom aggregation function. See 'Details'
#'        section for more information.
#' @param deltat integer, interval in seconds for the time of day axis. Has to be
#'        a fraction of 86400 (24 hours in seconds). It \code{NuLL} (default) the
#'        interval of the time series object will be used.
#' @param deltad integer, similar to \code{deltat}, the interval in days for the
#'        grid on the x-axis. Default is \code{7L} (aggregate to weekly values).
#' @param col vector of colors forwarded to \code{image.default}. By default
#'        a gray scale color map is used.
#' @param contours logical \code{TRUE} or \code{FALSE}, whether or not the
#'        concours should be added.
#' @param contour.col color for the contour lines, only used if \code{contours = TRUE}.
#'
#' @details Plotting a Hovmoeller diagram based on the \code{\link{zoo}} time
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
#' @importFrom grDevices gray.colors
#' @rdname foehnix
#' @export
image.foehnix <- function(x, FUN = "freq", deltat = NULL, deltad = 7L,
                          col = rev(gray.colors(20)), contours = FALSE,
                          contour.col = "black", ...) {

    stopifnot(inherits(x, "foehnix"))

    # Extend zoo object if needed (inflation)
    x <- x$prob
    stopifnot(is.regular(x, strict = TRUE))
    index(x) <- as.POSIXct(index(x))

    # Checking deltat argument
    if ( is.null(deltat) ) {
        deltat <- as.numeric(diff(index(x)[1:2]), unit = "secs")
    } else {
        stopifnot(is.finite(deltat))
        stopifnot(deltat > 0)
    }
    if ( ! round(86400 / deltat) * deltat == 86400 )
        stop(sprintf("deltat = %d is not a fraction of 86400 (one day in seconds).", deltat))

    # Checking deltad
    stopifnot(inherits(deltad, c("integer", "numeric")))
    stopifnot(deltad <= 365)
    deltad <- as.integer(deltad)
    if ( deltad < 1 ) stop("\"deltad\" has to be a positive integer.")

    # Checking colors
    stopifnot(inherits(col, "character"))
    stopifnot(length(col) > 1)

    # Aggregation function
    FUN_allowed <- c("mean", "occ", "noocc", "freq")
    if ( is.character(FUN) ) {
        FUN <- match.arg(FUN, FUN_allowed)
        if ( FUN == "mean" ) {
            FUN <- function(x) mean(x, na.rm = TRUE)
        } else if ( FUN == "occ" ) {
            FUN <- function(x) sum(x >= 0.5, na.rm = TRUE)
        } else if ( FUN == "noocc" ) {
            FUN <- function(x) sum(x <  0.5, na.rm = TRUE)
        } else if ( FUN == "freq" ) {
            FUN <- function(x) sum(x >= 0.5, na.rm = TRUE) / sum(!is.na(x))
        }
    } else if ( ! is.function(FUN) ) {
        stop("input \"FUN\" has to be a function or a character, one of ",
             paste(FUN_allowed, collapse = ", "))
    }

    # Add information about time and day of the year
    longform <- function(x, breaks.time, breaks.date) {
        # Convert zoo index to POSIXlt once
        lt   <- as.POSIXlt(index(x))

        # Seconds of this day
        time <- as.numeric(index(x)) - as.numeric(as.Date(index(x))) * 86400
        time <- ifelse(time == 0, 86400, time)

        # Zero-based Julian day
        yday <- ifelse(time == 86400, lt$yday - 1, lt$yday)
        yday <- ifelse(yday < 365, yday, 0)
        yday <- ifelse(yday < 0, yday + 365, yday)

        # Labels with full integers (required to extract the data again)
        labels.time <- sprintf("(%d,%d]", breaks.time[-length(breaks.time)], breaks.time[-1])

        res <- data.frame(hash_time = cut(time, breaks.time, labels = labels.time),
                          hash_date = cut(yday, breaks.date, include.lowest = TRUE))
        res$hash <- sprintf("%s_%s", res$hash_date, res$hash_time)
        return(res)
    }


    breaks.time <- seq(0, 86400, by = deltat)
    breaks.date <- unique(pmax(0, c(seq(-1, 364, by = deltad), 364)))
    data <- cbind(as.data.frame(x), longform(x, breaks.time, breaks.date))

    # Aggregate information
    agg <- aggregate(data$prob, by = list(data$hash), FUN = FUN)
    names(agg) <- c("hash", "value")


    # Helper function to extract time/date information. We have reduced
    # the date and time information to a string which looks somehow as follows:
    # (0,5]_(0,3600] where the first pair contain the "day of the year" range
    # (in this example yday 0 to yday 5 (including both, 0 and 5), the second
    # pair contains the time of the day in seconds since 00:00 UTC (in this
    # example from 0 seconds = 00:00 UTC to 3600 = 01:00 UTC. The lower one
    # is not included, the upper one is in this case.
    # This function extracts these values and returns a data.frame with the
    # 'coordinates' (yday_from, yday_to, time_from, time_to) used later to
    # draw the rectangles.
    extract_info <- function(x) {

        # Helper function to remove the brackets
        kill_first <- function(x) as.integer(substr(x, 2, nchar(x)))
        kill_last  <- function(x) as.integer(substr(x, 1, nchar(x) - 1))

        # Extracting date information
        date      <- regmatches(x, regexpr("^\\S[0-9]+,[0-9]+\\S", x))
        yday_from <- sapply(regmatches(date, regexpr("^\\S[0-9]+", date)), kill_first)
        yday_to   <- sapply(regmatches(date, regexpr("[0-9]+\\S$", date)), kill_last)

        # Extracting time information
        time      <- regmatches(x, regexpr("\\S[0-9]+,[0-9]+\\S$", x))
        time_from <- sapply(regmatches(time, regexpr("^\\S[0-9]+", time)), kill_first)
        time_to   <- sapply(regmatches(time, regexpr("[0-9]+\\S$", time)), kill_last)

        data.frame(hash = x,
                   yday_from = yday_from, yday_mid = (1 + yday_from + yday_to) / 2, yday_to = yday_to,
                   time_from = time_from, time_mid = (time_from + time_to) / 2,     time_to = time_to)

    }

    # Aggregate the data for the plot
    agg <- cbind(agg, extract_info(agg[,1]))

    # Some arguments for the plot
    arg <- list(...)
    xlab = if(! "xlab" %in% names(arg)) arg$xlab  = "time of the year"
    ylab = if(! "ylab" %in% names(arg)) arg$ylab  = "time of the day"
    main = if(! "main" %in% names(arg)) arg$main  = "foehnix Hovmoeller Diagram"

    # Convert values to colors for the plot.
    get_color <- function(x, col, zlim = NULL) {
        # Calculate color ID
        if ( is.null(zlim) ) {
            cID <- (x - min(x, na.rm = TRUE)) / max(x - min(x, na.rm = TRUE))
        } else {
            cID <- (x - min(zlim)) / max(zlim)
        }
        cID <- as.integer(round(cID * (length(col) - 1)) + 1)
        cID[which(cID < 1 | cID > length(col))] <- NA
        # Return a vector of colors
        col[cID]
    }
    agg$color <- get_color(agg$value, col, zlim = arg$zlim)

    # Draw plot
    hold <- par(no.readonly = TRUE); on.exit(par(hold))

    # Using layout to draw the diagram plus a legend. First we 
    # need to know the dimension of the image such that we can
    # adjust our layout:
    imgsize = structure(as.list(par()$pin), names = c("width", "height"))

    # Setting the parameters for the plot
    if ( inherits(arg$main, "character") ) {
        oma_top <- length(strsplit(arg$main, "\n")[[1]]) + 2
    } else { oma_top <- 2; }
    par(mar = rep(.3, 4), oma = c(4.1, 4.1, 3, 3))
    layout(matrix(1:2, nrow = 1), widths = c(imgsize$width - 0.5, 0.5))

    # Create an empty plot with the extend we need, namely
    # 0 - 354 (0 based Julian day) along the x-axis, and
    # 0 - 86400 (one full day in seconds) along the y-axis.
    plot(NA, bty = "n",
         xlim = c(-0.5, 364.5), xaxt = "n", xaxs = "i",
         ylim = c(0, 86400),    yaxt = "n", yaxs = "i",
         xlab = NA, ylab = NA, main = NA)
    mtext(side = 1, line = 3.0, arg$xlab)
    mtext(side = 2, line = 3.3, arg$ylab)

    # Adding the data (rectangles), the aggregated data.
    rect(agg$yday_from, agg$time_from, agg$yday_to, agg$time_to,
         border = NA, col = agg$color)

    # Adding y-axis (time)
    yat <- seq(0, 86400, by = 3600)
    ylab <- strftime(as.POSIXct(yat, origin = "1970-01-01"), "%H:%M")
    axis(side = 2, at = yat, labels = ylab, las = 1)

    # Adding x-axis (date)
    xat <- as.POSIXlt(sprintf("2016-%02d-01", 2:12))
    axis(side = 1, at = xat$yday - 0.5, labels = NA)
    xat <- as.POSIXlt(sprintf("2016-%02d-15", 1:12))
    axis(side = 1, at = xat$yday, labels = strftime(xat, "%b"), tick = FALSE,
         las = ifelse(par()$pin[1L] < 6, 2, 1))

    # If the user wants to have contour lines: draw contours.
    if ( contours ) {

        # ------------------------------------------
        # expand_agg is used to expand the aggregated
        # data values, matrix like, as follows:
        #  1  |  2  |  3
        # ---------------
        #  4  |  5  |  6
        # ---------------
        #  7  |  8  |  9 
        # This is necessary to get cyclic data when
        # drawing the contour lines.
        # ------------------------------------------
        expand_agg <- function(x) {

            x <- subset(x, select = c(time_mid, yday_mid, value))

            res <- list(); for ( i in 1:9 ) res[[i]] <- x
            # North West
            res[[1]]$time_mid <- res[[1]]$time_mid + 86400
            res[[1]]$yday_mid <- res[[1]]$yday_mid - 364
            # North
            res[[2]]$time_mid <- res[[2]]$time_mid + 86400
            # North East
            res[[3]]$time_mid <- res[[3]]$time_mid + 86400
            res[[3]]$yday_mid <- res[[3]]$yday_mid + 364
            # West
            res[[4]]$yday_mid <- res[[4]]$yday_mid - 364
            # East
            res[[6]]$yday_mid <- res[[6]]$yday_mid + 364
            # South West
            res[[7]]$time_mid <- res[[7]]$time_mid - 86400
            res[[7]]$yday_mid <- res[[7]]$yday_mid - 364
            # South
            res[[8]]$time_mid <- res[[8]]$time_mid - 86400
            # South East
            res[[9]]$time_mid <- res[[9]]$time_mid - 86400
            res[[9]]$yday_mid <- res[[9]]$yday_mid + 364

            # Combine the data
            res <- return(do.call(rbind, res))

            #TODO This never happens so far ...
            ### And shrink them. We don't need 9 times the data,
            ### cut the parts to far off the plotted area.
            ##res <- subset(res, time_mid > -3600 &
            ##                   time_mid < (86400 + 3600) &
            ##                   yday_mid > -10 &
            ##                   yday_mid < (364 + 10))
            ##return(res)
        }

        # Expand the data set to get  cyclic bounds
        expagg <- expand_agg(agg)

        # Manual grid. Step 1: setting up the dimension names of the new matrix
        cont <- list(sprintf("%.0f", sort(unique(expagg$time_mid))),
                     sprintf("%.0f", sort(unique(expagg$yday_mid))))
        # Step 2: create a matrix with these dimensions
        cont <- matrix(NA, nrow = length(cont[[1]]), ncol = length(cont[[2]]),
                       dimnames = cont)

        # Step 3: mapping data
        tmpcolname <- sprintf("%.0f", expagg$yday_mid)
        tmprowname <- sprintf("%.0f", expagg$time_mid)
        idx <- cbind(match(tmprowname, rownames(cont)), match(tmpcolname, colnames(cont)))
        cont[idx] <- expagg$val

        # Adding contour plot
        contour(x = as.numeric(colnames(cont)), y = as.numeric(rownames(cont)), z = t(cont),
                add = TRUE, col = contour.col, ...)
    }

    # Drawing the outline/box
    box()


    # Drawing the legend
    draw_legend <- function(x, col, zlim = NULL) {

        if ( is.null(zlim) ) {
            at <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length = length(col))
        } else {
            at <- seq(min(zlim, na.rm = TRUE), max(zlim, na.rm = TRUE), length = length(col))
        }
        # Loading colors
        colors <- get_color(at, col, zlim = NULL)
        # Draw the color map
        image(y = at,
              z = matrix(1:length(colors), nrow = 1), col = colors,
              xaxt = "n", xaxs = "i", xlim = c(0,1),
              yaxt = "n", yaxs = "i", ylim = range(at))

        # Adding legend
        axis(side = 4, las = 2, at = pretty(at))
        box()
    }
    draw_legend(agg$value, col, arg$zlim)

    # Adding title
    mtext(side = 3, line = .5, font = 2, cex=  1.2, outer = TRUE, arg$main)

    # That's the end, my friend ...
    # Return some properties (insisibile), mainly for testing.
    invisible(list(agg = agg,
                   xlab = xlab,
                   ylab = ylab,
                   zlim = arg$zlim,
                   breaks.time = breaks.time,
                   breaks.date = breaks.date,
                   control = list(FUN = FUN,
                                  deltat = deltat,
                                  deltad = deltad,
                                  contours = contours,
                                  contour.col = contour.col)))

}



























