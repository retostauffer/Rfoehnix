# -------------------------------------------------------------------
# - NAME:        foehnix_simple.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-11-28
# -------------------------------------------------------------------
# - DESCRIPTION: Scripts to estimate Gaussian and logistic
#                two-component mixture models with IWLS and
#                weighted empirical moments.
#                "simple" here referrs to non-gradient non-optimizer
#                based model estimates.
# -------------------------------------------------------------------
# - EDITORIAL:   2018-11-28, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-12 23:16 on marvin
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

# -------------------------------------------------------------------
# Trying to estimate a reasonably high upper bound for lambda
# if ridge penalization is needed.
# -------------------------------------------------------------------
get_lambdas <- function(nlambda, logitX, post, maxit, tol) {

    # If nlambda is not a positive integer: stop.
    stopifnot(inherits(nlambda, c("integer", "numeric")))
    stopifnot(nlambda >= 0)

    # Fitting logistic regression models with different lambdas.
    csum_fun <- function(lambda, logitX, post, maxit, tol) {
        # As response a first guess y >= median(y) is used.
        # Force standardize = FALSE as logitX is already standardized if
        # standardize == TRUE for this function.
        m <- iwls_logit(logitX, post, standardize = FALSE,
                        lambda = lambda, maxit = maxit, tol = tol)
        sum(abs(m$beta[which(!grepl("^\\(Intercept\\)$", rownames(m$beta))),]))
    }

    # Find large lambda where all parameters are close to 0.
    # Trying lambdas between exp(6) and exp(12).
    lambdas <- exp(seq(5, 15, by = 1))
    x <- sapply(lambdas, csum_fun, logitX = logitX, post = post, maxit = maxit, tol = tol)

    # Pick the lambda where sum of parameters is smaller than a 
    # certain threshold OR take maximum of lambdas tested.
    lambdas <- exp(seq(min(which(x < 0.1), length(x)), -8, length = as.numeric(nlambda)))
    cat(sprintf("Use penalization lambda within %.5f to %.5f\n", max(lambdas), min(lambdas)))
    return(lambdas)
}


# -------------------------------------------------------------------
# Calculates and returns the log-likelihood for the two parts
# of the Gaussian mixture model with two clusters.
# y: response
# post: posterior weights
# theta: list object containing the location/scale parameters (or
#        coefficients for location/scale for the Gaussian distributions)
# -------------------------------------------------------------------
foehnix_gaussian_loglik <- function(y, post, prob, theta) {
    # Calculate/trace loglik
    eps  <- sqrt(.Machine$double.eps)
    ll <- data.frame(
             component = sum(post       * dnorm(y, theta$mu2, exp(theta$logsd2), log = TRUE))
                       + sum((1 - post) * dnorm(y, theta$mu1, exp(theta$logsd1), log = TRUE)),
             concomitant = sum((1 - post) * log(1 - prob) + post * log(prob))
          )
    ll$full <- sum(unlist(ll))
    return(ll)
}

foehnix_gaussian_posterior <- function(y, prob, theta) {
    # Calculate posterior:
    # Ratio between weighted density of component 2 divided by the sum
    # of the weighted density of both components gives the posterior.
    (prob) * dnorm(y, theta$mu2, exp(theta$logsd2)) /
    ( (1 - prob) * dnorm(y, theta$mu1, exp(theta$logsd1)) +
       prob * dnorm(y, theta$mu2, exp(theta$logsd2)) )
}

foehnix_logistic_loglik <- function(y, post, prob, theta) {
    # Calculate/trace loglik
    eps  <- sqrt(.Machine$double.eps)
    prob <- pmax(eps, pmin(1-eps, prob))
    ll <- data.frame(
            component = sum(post       * dlogis(y, theta$mu2, exp(theta$logsd2), log = TRUE))
                      + sum((1 - post) * dlogis(y, theta$mu1, exp(theta$logsd1), log = TRUE)),
            concomitant = sum((1 - post) * log(1 - prob) + post * log(prob))
          )
    ll$full <- sum(unlist(ll))
    return(ll)
}

foehnix_logistic_posterior <- function(y, prob, theta) {
    # Calculate posterior:
    # Ratio between weighted density of component 2 divided by the sum
    # of the weighted density of both components gives the posterior.
    (prob) * dlogis(y, theta$mu2, exp(theta$logsd2)) /
    ((1 - prob) * dlogis(y, theta$mu1, exp(theta$logsd1)) +
    prob * dlogis(y, theta$mu2, exp(theta$logsd2)))
}


# -------------------------------------------------------------------
# Estimated regression coefficients
# -------------------------------------------------------------------
coef.foehnix <- function(x, ...) {
    res <- rbind(matrix(c(x$coef$mu1, x$coef$sd1, x$coef$mu2, x$coef$sd2), ncol = 1,
                        dimnames = list(c("mu1", "sd1", "mu2", "sd2"), NULL)),
                 x$coef$concomitants)
    setNames(as.vector(res), rownames(res))
}


# -------------------------------------------------------------------
# Model/classification summary
# -------------------------------------------------------------------
summary.foehnix <- function(x, ...) {
    rval <- list()
    rval$call    <- x$call
    rval$samples <- x$samples
    rval$coef    <- x$coef;
    colnames(rval$coef$concomitants) <- "coef"

    # Optimizer statistics
    rval$time      <- x$time
    rval$ll        <- x$optimizer$loglik$ll
    rval$df        <- length(coef(x))
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
    cat("\nCall: "); print(x$call)

    # TODO: Additional statistics for the estimated coefficients would be great
    cat("\nCoefficients of Gaussian distributions\n")
    tmp <- matrix(unlist(x$coef[c("mu1", "sd1", "mu2", "sd2")]), ncol = 2, dimnames = list(c("mu","sigma"), c("Comp.1", "Comp.2")))
    print(tmp)
    cat("\nCoefficients of the concomitant model\n")
    print(x$coef$concomitants)

    cat(sprintf("\nNumber of observations (total) %8d\n", x$samples$total)) 
    cat(sprintf("Removed due to missing values  %8d (%3.1f percent)\n", x$samples$na,    100 * x$samples$na / x$samples$total)) 
    cat(sprintf("Outside defined wind sector    %8d (%3.1f percent)\n", x$samples$wind,  100 * x$samples$wind / x$samples$total)) 
    cat(sprintf("Used for classification        %8d (%3.1f percent)\n", x$samples$taken, 100 * x$samples$taken / x$samples$total)) 
    cat(sprintf("\nClimatological foehn occurance %.2f percent\n", x$meanfoehn))
    cat(sprintf("Mean foehn probability %.2f percent\n", x$meanprob))

    cat(sprintf("\nLog-likelihood: %.1f on %d Df\n",   x$ll, x$df))
    cat(sprintf("Number of EM iterations %d/%d (%s)\n", x$n.iter, x$maxit,
                ifelse(x$converged, "converged", "not converged")))
    cat(sprintf("Time for optimization: %.1f minutes\n", x$time))
}



# -------------------------------------------------------------------
# Development plot routine
# TODO: this is very specific to our data and our variable names.
#       either provide something like this and force the users to
#       follow our naming conventions, or make it much more
#       flexible/generig.
# -------------------------------------------------------------------
plot.foehnix <- function(x, start = NULL, end = NULL, ndays = 10, ..., xtra = NULL, ask = TRUE) {

    add_boxes <- function(x, col = "gray94") {
        dx  <- as.numeric(diff(index(x)[1:2]), unit = "secs") / 2
        up   <- which(diff(x > .5) == 1) + 1
        down <- which(diff(x > .5) == -1)
        if ( length(up) > 0 & length(down) > 0 ) {
            down <- down[down >= min(up)]
            y <- par()$usr[3:4]
            for ( i in seq(1, min(length(up), length(down))) )
                rect(index(x)[up[i]] - dx, y[1L], index(x)[down[i]] + dx, y[2L],
                     col = col, border = NA)
        }
    }
    add_midnight_lines <- function(x) {
        ndays <- as.numeric(diff(range(index(x))), unit = "days")
        if ( ndays < 50 ) {
            at <- as.POSIXct(unique(as.Date(index(x))))
            abline(v = at, col = 1)
        }
    }

    # Convert start/end to POSIXct
    if ( ! is.null(start) ) {
        start <- try(as.POSIXct(start))
        if ( inherits(start, "try-error") )
            stop("Invalid input for \"start\". Cannot be converted to POSIXt.")
    }
    if ( ! is.null(end) ) {
        end <- try(as.POSIXct(end))
        if ( inherits(end, "try-error") )
            stop("Invalid input for \"end\". Cannot be converted to POSIXt.")
    }

    # Default plot type/plot interval if start and end are not
    # provided:
    if ( is.null(start) & is.null(end) ) {
        # Extracting zoo index (range of dates)
        dates <- as.POSIXct(as.Date(range(index(x$prob))))
        if ( max(index(x$prob)) > dates[2L] ) dates <- dates + c(0,86400)
        # If less than ndays days: plot all 10 days.
        if ( as.numeric(diff(dates), units = "days") <= ndays ) {
            start <- dates[1L]; end <- dates[2L]
        # Else create a set of sequences to plot
        } else {
            start <- seq(dates[1L], dates[2L], by = 86400 * ndays)
            end   <- start + 86400 * ndays
            start <- start[start < dates[2L]]
            end   <- pmin(end[start < dates[2L]], dates[2L])
        }
    } else {
        if ( is.null(end) & length(start) != 1 )
            stop("If input \"end\" is not provided \"start\" has to be of length 1")
        if ( is.null(start) & length(end) != 1 )
            stop("If input \"start\" is not provided \"end\" has to be of length 1")
        if ( is.null(end) )   end   <- max(x$prob)
        if ( is.null(start) ) start <- min(x$prob) 
    }
    # Check whether both (start and end) are of same length
    if ( ! length(start) == length(end) )
        stop("Input \"start\" and \"end\" have to be of same length!")

    # Check if time range is valid
    if ( all(start > max(index(x$prob))) | all(end < min(index(x$prob))) )
        stop("All time periods defined by start/end outside specified data set.")

    # Keep user settings (will be reset when this function ends)
    hold <- par(no.readonly = TRUE); on.exit(par(hold))

    # If multiple periods have to be plotted: set ask = TRUE
    if ( length(start) > 1 ) par(ask = ask) else par(ask = FALSE)

    # Combine foehn probabilities and observations
    data <- merge(x$prob, x$data)
    names(data)[1L] <- "prob"

    # Looping over the different periods we have to plot
    par(mfrow = c(4,1), mar = rep(0.1, 4), xaxs = "i", oma = c(4.1, 4.1, 2, 4.1))
    for ( k in seq_along(start) ) {

        tmp <- window(data, start = start[k], end = end[k])
        # No data, or only missing data?
        if ( nrow(tmp) == 0 | sum(!is.na(tmp)) == 0 ) {
            tmp <- paste("No data (or only missing values) for the time period",
                         strftime(start[k], "%Y-%m-%d %H:%M"), "to",
                         strftime(end[k], "%Y-%m-%d %H:%M"))
            warning(sprintf("%s. Skip plotting.", str))
            next
        }

        # Air temperature
        plot(tmp$t, type = "n", ylab = NA, xaxt = "n", bty = "n")
        add_boxes(tmp$prob); add_midnight_lines(tmp)
        lines(tmp$t, col ="red", lwd = 2)
        mtext(side = 2, line = 3, "dry air temperature")
        box()
    
        # Relative humidity
        par(new = TRUE)
        plot(tmp$rh, type = "n", lwd = 2, yaxt = "n", ylim = c(0,150), yaxs = "i", xaxt = "n", bty = "n")
        add_polygon(tmp$rh, col = "#009900")
        abline(h = seq(20, 100, by = 20), lty = 3, col = "#00990060")
        axis(side = 4, at = seq(20, 100, by = 20))
        mtext(side = 4, line = 3, "relative humidity")
        box()
    
        # Temperature difference
        plot(tmp$diff_t, type = "n", xaxt = "n", bty = "n")
        add_boxes(tmp$prob); add_midnight_lines(tmp)
        lines(tmp$diff_t, col = "orange", lwd = 2)
        abline(h = seq(-20,20, by = 1), col = "gray80", lty = 3)
        abline(h = 0, col = 1)
        mtext(side = 2, line = 3, "temperature difference")
        box()
    
        # Wind speed and direction
        if ( ! is.null(x$windsector) ) {
            stopifnot("dd" %in% names(tmp))
            if ( x$windsector[1L] < x$windsector[2L] ) {
                ddflag <- ifelse(tmp$dd < x$windsector[1L] | tmp$dd > x$windsector[2L], 1, 2) 
            } else {
                ddflag <- ifelse(tmp$dd > x$windsector[1L] | tmp$dd < x$windsector[2L], 1, 2) 
            }
        } else { ddflag <- rep(2, nrow(tmp)) }
        plot(NA, type = "n", xaxt = "n", ylab = "", xlim = range(index(tmp)),
                 ylim = c(0, 360), yaxt = "n", bty = "n")
        add_boxes(tmp$prob); add_midnight_lines(tmp)
        if ( "dd" %in% names(tmp) ) {
            points(tmp$dd, col = c("gray50","black")[ddflag], pch = c(1, 19)[ddflag], cex = c(.3, .5)[ddflag])
        }
        axis(side = 2, at = seq(90, 360 - 90, by = 90))
        mtext(side = 2, line = 3, "wind direction")
        if ( ! is.null(x$windsector) ) abline(h = x$windsector, col = "gray", lty = 3)
        box()
    
        # Adding wind speed
        par(new = TRUE)
        plot(tmp$ff, type = "n", ylim = c(0, max(tmp$ff, na.rm = TRUE)) * 1.05,
             yaxs = "i", yaxt = "n", xaxt = "n")
        add_polygon(tmp$ff, col = "#005ce6")
        axis(side = 4, at = pretty(tmp$ff))
        mtext(side = 4, line = 3, "wind speed")
        box()
    
        # Foehn prob
        plot(tmp$prob * 100, type = "n", ylab = NA, ylim = c(-4,104), yaxs = "i") 
        add_boxes(tmp$prob); add_midnight_lines(tmp)
        if ( ! is.null(xtra) ) lines(xtra * 100, col = "gray50", lty = 5)
        abline(h = seq(0, 100, by = 20), col = "gray", lty = 3)
        mtext(side = 2, line = 3, "foehn probability")
        add_polygon(tmp$prob * 100, col = "#FF6666", lower.limit = -4)
        # Adding RUG
        at <- index(tmp$prob)[which(tmp$prob >= .5)]
        if ( length(at) > 0 ) axis(side = 1, at = at, labels = NA, col = 2)
        box()
        if ( ! is.null(xtra) )
            legend("left", bg = "white", col = c("#FF6666", "gray50"), lty = c(1,5),
                   legend = c("foehnix", "xtra"))
    
        # Adding a title to the plot
        title <- sprintf("Foehn Diagnosis %s to %s", start[k], end[k])
        mtext(side = 3, outer = TRUE, title, font = 2, cex = 1.2, line = 0.5)
    } # End of loop over start/end (loop index k)


}

# --------------------------------------------------------------------
# Helper function to draw nice filled polygons with NA's.
# --------------------------------------------------------------------
add_polygon <- function( x, col = "#ff0000", lower.limit = 0, lwd = 1 ) {
    # Need hex color
    if ( ! grepl("^#[A-Za-z0-9]{6}$",col) ) stop("Sorry, need hex color definition for polygon plots.")
    # All elements NA?
    if ( all( is.na(x) ) ) return(invisible(NULL))
    # Else find valid blocks and plot them. Start with 1
    i <- 1
    while ( i <= length(x) ) {
        if ( all(is.na(x)) ) break
        i1 <- min( which( !is.na(x) ) )
        if ( i1 > 1 ) { x <- x[-c(seq(1,i1-1))]; i1 <- 1 }
        # Else check first NA
        if ( ! any(is.na(x)) ) { i2 <- length(x) } else { i2 <- min( which( is.na(x) ) ) - 1 }
        p_x <- as.numeric(index(x[i1:i2])); p_x <- c(p_x,max(p_x),min(p_x))
        p_y <- c(as.numeric(x[i1:i2]),lower.limit, lower.limit )
        polygon( p_x, p_y, col = sprintf("%s20",col), border = NA )
        lines( x[i1:i2],   col = col, lwd = lwd )
        # Remove plotted data from time series and continue
        x <- x[-c(i1:i2)]
    }

}

