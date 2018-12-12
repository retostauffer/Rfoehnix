# -------------------------------------------------------------------
# - NAME:        phoeton_simple.R
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
# - L@ST MODIFIED: 2018-12-12 08:11 on marvin
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Prepare data for classification. Used for both, phoeton and
# advanced phoeton as the first part of both functions is the
# same for both methods.
# Returns a list which will be attached in the parent function.
# -------------------------------------------------------------------
phoeton_prepare <- function(formula, data, windsector, maxit, tol, lambda.min,
                            standardize, family, nlambda) {

    # Regularization for the logit model (concomitant model) can be either
    # loglik (no regularization), AIC, or BIC. In case of AIC and BIC
    # the optimal penalization is based on AIC/BIC criteria using
    # a ridge penalization. Requires to estimate
    # the logit model multiple times for different lambdas.
    lambda.min <- match.arg(lambda.min, c("auto","loglik", "AIC", "BIC"))
    family     <- match.arg(family, c("gaussian", "logistic"))
    if ( ! inherits(standardize, "logical") )
        stop("Input \"standardize\" has to be logical (TRUE or FALSE).")
    if ( ! inherits(data, "zoo") )
        stop("Input \"data\" to foehndiag needs to be a time series object of class \"zoo\".")

    # Deconstruct the formula
    left  <- as.character(formula)[2]
    right <- as.character(formula)[3]

    # Stop if the main covariate for the flexible Gaussian mixture model
    # is not a valid variable name.
    stopifnot(grepl("^\\S+$", left) & ! grepl("[+~]", left))

    # Maxit and tol are the maximum number of iterations for the
    # optimization. Need to be numeric. If one value is given it will
    # be used for both, the EM algorithm and the IWLS optimization for
    # the concomitants. If two values are given the first one is used
    # for the EM algorithm, the second for the IWLS solver.
    stopifnot(is.numeric(maxit) | length(maxit) > 2)
    stopifnot(is.numeric(tol)   | length(tol) > 2)

    # Create strictly regular time series object with POSIXct
    # time stamp.
    index(data) <- as.POSIXct(index(data))
    if ( is.regular(data) & ! is.regular(data, strict = TRUE) ) {
        interval <- min(diff(index(data)))
        tmp <- seq(min(index(data)), max(index(data)), by = interval)
        data <- merge(data, zoo(,tmp))
    }

    # Extracting model.frame used for the concomitant model,
    # and the vector y used for the clustering (main covariate).
    # Keep missing values.
    mf <- model.frame(formula, data, na.action = na.pass)
    y  <- model.response(mf)

    # Identify rows with missing values
    idx_na   <- which(is.na(y) | apply(mf, 1, function(x) sum(is.na(x))) != 0)
    # If a wind sector is given: identify observations with a wind direction
    # outside the user defined sector. These will not be considered in the
    # statistical models.
    if ( is.null(windsector) ) {
        idx_wind <- NULL # No wind sector filter
    } else {
        # FIXME: is it possible to use custom names?
        if ( ! "dd" %in% names(data) )
            stop("If wind sector is given the data object requires to have a column \"dd\".")
        # Filtering
        if ( windsector[1L] < windsector[2L] ) {
            idx_wind <- which(data$dd < windsector[1L] | data$dd > windsector[2L])
        } else {
            idx_wind <- which(data$dd > windsector[1L] & data$dd < windsector[2L])
        }
    }
    # Indes of all values which should be considered in the model
    idx_take <- which(! 1:nrow(data) %in% c(idx_na, idx_wind))
    if ( length(idx_take) == 0 ) stop("No data left after applying the required filters.")

    # Subset the model.frame (mf) and the response (y) and pick
    # all valid rows (without missing values on the mandatory columns
    # and, if a wind sector is given, with valid wind direction observations).
    mf <- matrix(unlist(mf[idx,]), ncol = ncol(mf), dimnames = list(NULL, names(mf)))
    y  <- y[idx_take]

    # Check whether regularization is preferred over unpenalized
    # regression estimation (only if lambda.min is "auto")
    if ( lambda.min == "auto" & ncol(mf) > 2 ) {
        tmp <- cor(na.omit(mf[,-1])); diag(tmp) <- 0
        if ( max(abs(tmp)) > .75 ) lambda.min <- "AIC"
    }

    # Setting up the model matrix for the concomitant model (logit model).
    logitX <- model.matrix(formula, data = data[idx_take,])

    # Initialize coefficients for the two components of the mixture
    # model (location and log-scale; mean and log-standard deviation).
    # If family == "logistic": empirical scale for a logistic random
    # variable is sd(y) * sqrt(3) / pi
    logsd <- function(y) log(sqrt(sum((y-mean(y))^2) / length(y)))
    theta  <- list(mu1    = as.numeric(quantile(y, 0.25)),
                   logsd1 = logsd(y[y <= median(y)]),
                   mu2    = as.numeric(quantile(y, 0.75)),
                   logsd2 = logsd(y[y >= median(y)]))
    if ( family == "logistic" ) {
        theta$logsd1 <- log(exp(theta$logsd1) * sqrt(3) / pi)
        theta$logsd2 <- log(exp(theta$logsd2) * sqrt(3) / pi)
    }

    # Initial parameters for the concomitant model (ccmodel)
    # as.numeric(y > median(y)) is the initial best guess component assignment.
    # Force standardize = FALSE as logitX is already standardized if input
    # standardize = TRUE for the main function phoeton and advanced_phoeton.
    ccmodel <- iwls_logit(logitX, as.numeric(y > median(y)), standardize = FALSE,
                          maxit = tail(maxit, 1), tol = tail(tol, 1), nlambda = nlambda)
    cat("\nInitial parameters:\n")
    print(matrix(c(theta$mu1, exp(theta$logsd1), theta$mu2, exp(theta$logsd2)), ncol = 2,
                 dimnames = list(c("mu", "sd"), c("Comp.1", "Comp.2"))))

    # calculate current probabilities (probability to be in second cluster)
    # ccmodel$coef are the non-standardized coefficients (alpha)
    prob <- plogis(drop(logitX %*% ccmodel$coef))

    # If standardize = TRUE: standardize model matrix for
    # the concomitant model (logitX)
    if ( standardize ) logitX <- standardize_model_matrix(logitX)


    # Returning a set of objects as named list used in the parent
    # function (phoeton and advanced_phoeton):
    return(list(theta  = theta,      ccmodel  = ccmodel,
                prob   = prob,
                y      = y,          logitX   = logitX,
                idx_take = idx_take, idx_na   = idx_na,
                idx_wind = idx_wind))
}

# -------------------------------------------------------------------
# Small helper function to check whether or not the matrix is
# standardized or not. Considered to be standardized if the matrix
# contains the two additional attributes "scaled:center" and
# "scaled:scale".
# -------------------------------------------------------------------
is.standardized <- function(x, ...) UseMethod("is.standardized")
is.standardized.matrix <- function(x, ...) {
    all(c("scaled:center", "scaled:scale") %in% names(attributes(x)))
}

# -------------------------------------------------------------------
# Standardize coefficients
# -------------------------------------------------------------------
standardize_model_matrix <- function(X) {
    if(sum(apply(X, 2, sd) == 0) > 1)
        stop("Multiple columns with constant values!")
    # Scale covariates
    scaled_center <- structure(rep(0, ncol(X)), names = colnames(X))
    scaled_scale  <- structure(rep(1, ncol(X)), names = colnames(X))
    for ( i in ncol(X) ) {
        if ( sd(X[,i]) == 0 ) next
        scaled_center[i] <- mean(X[,i])
        scaled_scale[i]  <- sd(X[,i])
        X[,i] <- (X[,i] - scaled_center[i]) / scaled_scale[i]
    }
    attr(X, "scaled:center") <- scaled_center
    attr(X, "scaled:scale")  <- scaled_scale
    return(X)
}


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
# Calculates and returns the log-likelihood for the two parts
# of the Gaussian mixture model with two clusters.
# y: response
# post: posterior weights
# theta: list object containing the location/scale parameters (or
#        coefficients for location/scale for the Gaussian distributions)
# -------------------------------------------------------------------
foehndiag_gaussian_loglik <- function(y, post, prob, theta) {
        # Calculate/trace loglik
        eps  <- sqrt(.Machine$double.eps)
        ll <- list(component = sum(post       * dnorm(y, theta$mu2, exp(theta$logsd2), log = TRUE))
                             + sum((1 - post) * dnorm(y, theta$mu1, exp(theta$logsd1), log = TRUE)),
                   concomitant = sum((1 - post) * log(1 - prob) + post * log(prob)))
        ll$full <- sum(unlist(ll))
        return(ll)
}
foehndiag_gaussian_posterior <- function(y, prob, theta) {
    (prob) * dnorm(y, theta$mu2, exp(theta$logsd2)) /
    ((1 - prob) * dnorm(y, theta$mu1, exp(theta$logsd1)) +
    prob * dnorm(y, theta$mu2, exp(theta$logsd2)))
}

foehndiag_logistic_loglik <- function(y, post, prob, theta) {
        # Calculate/trace loglik
        eps  <- sqrt(.Machine$double.eps)
        prob <- pmax(eps, pmin(1-eps, prob))
        ll <- list(component = sum(post       * dlogis(y, theta$mu2, exp(theta$logsd2), log = TRUE))
                             + sum((1 - post) * dlogis(y, theta$mu1, exp(theta$logsd1), log = TRUE)),
                   concomitant = sum((1 - post) * log(1 - prob) + post * log(prob)))
        ll$full <- sum(unlist(ll))
        return(ll)
}
foehndiag_logistic_posterior <- function(y, prob, theta) {
    (prob) * dlogis(y, theta$mu2, exp(theta$logsd2)) /
    ((1 - prob) * dlogis(y, theta$mu1, exp(theta$logsd1)) +
    prob * dlogis(y, theta$mu2, exp(theta$logsd2)))
}


# -------------------------------------------------------------------
# Estimated regression coefficients
# -------------------------------------------------------------------
coef.phoeton <- function(x, ...) {
    res <- rbind(matrix(c(x$coef$mu1, x$coef$sd1, x$coef$mu2, x$coef$sd2), ncol = 1,
                        dimnames = list(c("mu1", "sd1", "mu2", "sd2"), NULL)),
                 x$coef$concomitants)
    setNames(as.vector(res), rownames(res))
}


# -------------------------------------------------------------------
# Model/classification summary
# -------------------------------------------------------------------
summary.phoeton <- function(x, ...) {
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

    class(rval) <- "summary.phoeton"
    return(rval)
}
print.summary.phoeton <- function(x, ...) {
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
plot.phoeton <- function(x, start = NULL, end = NULL, ndays = 10, ..., xtra = NULL, ask = TRUE) {

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
        at <- index(x$prob)[which(x$prob >= .5)]
        if ( length(at) > 0 ) axis(side = 1, at = at, labels = NA, col = 2)
        box()
        if ( ! is.null(xtra) )
            legend("left", bg = "white", col = c("#FF6666", "gray50"), lty = c(1,5),
                   legend = c("phoeton", "xtra"))
    
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

