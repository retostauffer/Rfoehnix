# -------------------------------------------------------------------
# - NAME:        simple.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-11-28
# -------------------------------------------------------------------
# - DESCRIPTION: Development test script for foehton, simple method.
# -------------------------------------------------------------------
# - EDITORIAL:   2018-11-28, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-06 13:49 on marvin
# -------------------------------------------------------------------



# -------------------------------------------------------------------
# IWLS optimizer for logistic regression model (concomitant model)
# -------------------------------------------------------------------
iwls_logit <- function(X, y, beta = NULL, lambda = NULL, maxit = 100L, tol = 1e-8,
                       standardize = TRUE, ...) {

    #data <- cbind(data.frame(obs = y, a = X[,2]))
    #m <- glm(obs ~ a, data = data, family = binomial)
    # Unscale coefficients if needed
    #rval <- list(lambda = 99, edf = 2, loglik = as.numeric(logLik(m)), AIC = AIC(m), BIC = BIC(m),
    #             converged = TRUE)
    #rval$beta <- as.matrix(coef(m))
    #rval$coef <- as.matrix(coef(m))

    #warning("using glm")
    #return(rval)

    if ( standardize ) X <- standardize_model_matrix(X)

    # initialize regression coefficients if needed
    if ( is.null(beta) ) beta <- rep.int(0, ncol(X)) # FIXME: there is surely a better solution!

    # If all y in {0, 1} this is the binary response and we have to
    # calculate the initial values for the linear predictor and the
    # response (probabilities). If y in [0,1] these are already
    # probabilities.
    if ( all(y %in% 0:1) ) {
        # Calculate linear predictor eta
        eta <- drop(X %*% beta)
        # Apply link function on linear predictor to get response mu (probabilities)
        mu  <- plogis(eta) 
    } else {
        eps <- sqrt(.Machine$double.eps)
        mu  <- pmax(eps, pmin(1-eps, y))
        eta <- log(mu / (1 - mu))
        y   <- as.numeric(y >= 0.5)
    }
    # Calculate log-likelihood given initial parameters beta.
    # Initialize ll0 as ll - 1000 such that we don't stop in iteration 0.
    ll  <- sum(ifelse(y > 0, log(mu), log(1 - mu))) # FIXME: possibly avoid ifelse()
    ll0 <- ll - 1000
    # Maximum number of iterations
    iter <- 0

    # IWLS
    while( (iter < maxit) & ( (ll - ll0) > tol ) ) {
        ll0 <- ll
        # Initialize new iteration
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
        ll   <- sum(ifelse(y > 0, log(mu), log(1 - mu)))
        cat(sprintf("Iteration %d, ll = %15.4f, %s\r", iter, ll,
                    ifelse(is.null(lambda), "unregularized", sprintf("lambda = %10.4f", lambda))))
    }


    # Just naming the column containing the coefficients.
    colnames(beta) <- c("concomitant")

    # Calculate effective degrees of freedom
    if ( is.null(lambda) ) { reg <- 0 } else { reg <- diag(ncol(X)) * lambda; reg[1,1] <- 0 }
    edf <- sum(diag(t(X*w) %*% (X*w) %*% solve(t(X*w) %*% (X*w) + reg)))

    # Unscale coefficients if needed
    rval <- list(lambda = lambda, edf = edf, loglik = ll, AIC = -2 * ll + 2 * edf,
                 BIC = -2 * ll + log(nrow(X)) * edf,
                 converged = ifelse(iter < maxit, TRUE, FALSE))
    rval$beta <- beta
    rval$coef <- if ( ! standardize ) beta else destandardize_coefficients(beta, X)

    # Return list object containing
    # - edf (numeric): effective degrees of freedom
    # - loglik (numeric): log-likelihood of the model
    # - converged (logical): flag whether or not the iterative solver converged
    # - beta/coef (matrix): standardized and de-standardized coefficients. If
    #       input "standardized = FALSE" beta and coef are identical.
    return(rval)
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
# The simple version for the foehn diagnosis using empirical weighted
# moments for the parameters of the two Gaussian clusters, and a
# logistic regression model for the concomitant part.
# -------------------------------------------------------------------
foehndiag <- function(formula, data, windsector = NULL, maxit = 100L, tol = 1e-8, lambda.min = "AIC",
                      standardize = TRUE, family = "gaussian", ...) {

    # Regularization for the logit model (concomitant model) can be either
    # loglik (no regularization), AIC, or BIC. In case of AIC and BIC
    # the optimal penalization is based on AIC/BIC criteria using
    # a ridge penalization. Requires to estimate
    # the logit model multiple times for different lambdas.
    lambda.min <- match.arg(lambda.min, c("loglik", "AIC", "BIC"))
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

    # Create strictly regular time series object
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
    mf <- mf[idx_take,]
    y  <- y[idx_take]

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
    ccmodel <- iwls_logit(logitX, as.numeric(y > median(y)), maxit = tail(maxit, 1), tol = tail(tol, 1))
    cat("\nInitial parameters:\n")
    print(matrix(c(theta$mu1, exp(theta$logsd1), theta$mu2, exp(theta$logsd2)), ncol = 2,
                 dimnames = list(c("mu", "sd"), c("Comp.1", "Comp.2"))))

    # calculate current probabilities (probability to be in second cluster)
    # ccmodel$coef are the non-standardized coefficients (alpha)
    prob <- plogis(drop(logitX %*% ccmodel$coef))

    # Start optimization using weighted empirical moments for
    # location and scale of the two Gaussian distributions plus
    # an IWLS solver for the logistic regression model given the
    # concomitant variables.
    # Optimization
    ll0 <- NULL      # Initial value for the log-likelihood sum
    llpath <- list() # List element to store log-likelihood path
    t <- Sys.time()  # Measuring execution time
    iter <- 0        # Iteration index for the EM algorithm

    # If lambda.min is not loglik: ridge penalization
    lambdas <- if ( ! lambda.min == "loglik" ) exp(seq(8, -8, length = 100)) else NULL

    # Perform EM algorithm
    while ( iter < maxit[1L] ) { 

        # Increase iteration counter
        iter <- iter + 1

        ## E-Step ##
        # Calculate the posterior weights
        post <- do.call(sprintf("foehndiag_%s_posterior", family),
                        list(y = y, prob = prob, theta = theta))

        ## M-Step ##
        # Empirical moments for the two clusters (mu1/sd1 and mu2/sd2)
        theta$mu1    <- 1 / sum(1 - post) * sum( y * (1 - post))
        theta$mu2    <- 1 / sum(post) * sum( y * post)
        theta$logsd1 <- log(sqrt( 1 / sum(1 - post) * sum( (y - theta$mu1)^2 * (1 - post))))
        theta$logsd2 <- log(sqrt( 1 / sum(post)     * sum( (y - theta$mu2)^2 *     (post))))
        if ( family == "logistic" ) {
            theta$logsd1 <- log(exp(theta$logsd1) * sqrt(3) / pi)
            theta$logsd2 <- log(exp(theta$logsd2) * sqrt(3) / pi)
        }

        # Update the concomitant model.
        # Using the (possibly) standardized coefficients from the previous iteration
        # as initial parameters (alpha).
        if ( is.null(lambdas) ) {
            ccmodel <- iwls_logit(logitX, as.numeric(post >= .5), ccmodel$beta,
                                  maxit = tail(maxit, 1), tol = tail(tol, 1))
        } else {
            tmp <- list()
            for ( la in lambdas ) {
                m <- iwls_logit(logitX, as.numeric(post >= .5), ccmodel$beta,
                                maxit = tail(maxit, 1), tol = tail(tol, 1), lambda = la)
                # Break early if increase AIC/BIC (as specified for lambda.min)
                # is smaller than the tolerance.
                if ( length(tmp) > 1 ) {
                    if ( (tmp[[length(tmp)]][[lambda.min]] - m[[lambda.min]]) < tol )
                        break
                }
                tmp[[length(tmp)+1]] <- m
            }
            ccmodel = tmp[[which.min(sapply(tmp, function(x, score) x[[score]], score = lambda.min))]]
        }
        cat("\n")


        # Update the probabilities using the non-standardized coefficients (alpha)
        # from the concomitant model (ccmodel)
        prob <- plogis(drop(logitX %*% ccmodel$coef)) # Update probabilities

        # Calculate/trace loglik
        ll <- do.call(sprintf("foehndiag_%s_loglik", family),
                      list(y = y, post = post, prob = prob, theta = theta))
        if ( !is.finite(ll$full) ) browser()
        llpath[[iter]] <- ll

        # Initial log-likelihood given the initial guess
        if ( is.null(ll0) ) ll0 <- ll$full - 1000

        #cat(sprintf("  EM step  %2d: logLik = %10.3f\n", iter, ll$ll))
        #cat(sprintf("       Gauss 1: %10.5f %10.5f\n", theta$mu1, exp(theta$logsd1)))
        #cat(sprintf("       Gauss 2: %10.5f %10.5f\n", theta$mu2, exp(theta$logsd2)))
        cat(sprintf("EM step %3d/%3d, log-likelihood sum: %10.5f\n", iter, maxit[1L], ll$full))
    
        # Check log-likelihood improvement in the current iteration.
        # If the improvement is smaller than the tolerance the algorithm
        # converged: stop optimization.
        if ( (ll$full - ll0) < tol[1L] ) break
        ll0 <- ll$full
    }; cat("\n")

    # Create the return list object (phoeton object)
    rval <- list()
    rval$call <- match.call()
    rval$coef <- list(mu1 = theta$mu1, sd1 = exp(theta$logsd1), mu2 = theta$mu2, sd2 = exp(theta$logsd2),
                      concomitants = ccmodel$coef)

    rval$optimizer <- list(loglik = ll, loglikpath = do.call(rbind, llpath), n.iter = iter,
                           maxit = maxit[1L], tol = tol[1L], converged = ifelse(iter < maxit, TRUE, FALSE))
    rval$data <- data
    rval$windsector <- windsector

    rval$samples <- list(total = nrow(data), na = length(idx_na), wind = length(idx_wind), taken = length(idx_take))

    # Calculate final probabilities using the non-standardized coefficients (alpha)
    # from the final concomitant model (ccmodel).
    # d1: density of cluster 1 given the parameters for the two Gaussian
    #     distributions/clusters and the posterior information
    # d2: density of cluster 2 (see above)
    pfun <- ifelse(family == "gaussian", pnorm, plogis)
    d1   <- pfun(y, theta$mu1, exp(theta$logsd1)) * (1 - post)
    d2   <- pfun(y, theta$mu2, exp(theta$logsd2)) * post

    # The fohen probability vector: create an object of the
    # same length and class as input "data" and:
    # - fill all with NA (default)
    # - observations outside the requested wind sector get a 0 (no foehn)
    # - those observations which entered the models get their modelled
    #   foehn probability.
    rval$prob <- zoo(data.frame(prob = rep(NA, nrow(data))), index(data))
    rval$prob[idx_take] <- d2 / (d2 + d1)
    rval$prob[idx_wind] <- 0

    d1   <- pnorm(y, theta$mu1, exp(theta$logsd1)) * (1 - prob)
    d2   <- pnorm(y, theta$mu2, exp(theta$logsd2)) * prob
    rval$probX <- zoo(data.frame(prob = rep(NA, nrow(data))), index(data))
    rval$probX[idx_take] <- d2 / (d2 + d1)
    rval$probX[idx_wind] <- 0

    # Return new object
    class(rval) <- "phoeton"
    return(rval)
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
}



# -------------------------------------------------------------------
# Development plot routine
# TODO: this is very specific to our data and our variable names.
#       either provide something like this and force the users to
#       follow our naming conventions, or make it much more
#       flexible/generig.
# -------------------------------------------------------------------
plot.phoeton <- function(x, start = NULL, end = NULL, ..., xtra = NULL) {

    add_boxes <- function(x, col = "gray94") {
        dx  <- as.numeric(diff(index(x)[1:2]), unit = "secs") / 2
        up   <- which(diff(x > .5) == 1) + 1
        down <- which(diff(x > .5) == -1)
        if ( length(up) > 0 & length(down) > 0 ) {
            down <- down[down > min(up)]
            y <- par()$usr[3:4]
            for ( i in seq(1, min(length(up), length(down))) )
                rect(index(x)[up[i]] - dx, y[1L], index(x)[down[i]] + dx, y[2L],
                     col = col, border = NA)
        }
    }

    hold <- par(no.readonly = TRUE); on.exit(par(hold))

    # Combine foehn probabilities and observations
    # FIXME: remove probX, only for testing
    tmp <- merge(x$prob, x$probX, x$data)
    tmp <- window(tmp, start = start, end = end)
    names(tmp)[1:2] <- c("prob", "probX")
    print(head(tmp))

    par(mfrow = c(4,1), mar = rep(0.1, 4), xaxs = "i", oma = c(4.1, 4.1, 2, 4.1))

    # Air temperature
    plot(tmp$t, type = "n", ylab = NA, xaxt = "n")
    add_boxes(tmp$prob)
    lines(tmp$t, col ="red", lwd = 2)
    mtext(side = 2, line = 3, "dry air temperature")

    # Relative humidity
    par(new = TRUE)
    plot(tmp$rh, col = "green", lwd = 2, yaxt = "n", ylim = c(0,150), yaxs = "i", xaxt = "n")
    abline(h = seq(20, 100, by = 20), lty = 3, col = "gray")
    axis(side = 4, at = seq(20, 100, by = 20))
    mtext(side = 4, line = 3, "relative humidity")

    # Temperature difference
    plot(tmp$diff_t, type = "n", xaxt = "n")
    add_boxes(tmp$prob)
    lines(tmp$diff_t, col = "magenta", lwd = 2)
    abline(h = seq(-20,20, by = 1), col = "gray80", lty = 3)
    abline(h = 0, col = 1)
    mtext(side = 2, line = 3, "temperature difference")

    # Wind speed and direction
    if ( ! is.null(x$windsector) ) {
        stopifnot("dd" %in% names(tmp))
        if ( x$windsector[1L] < x$windsector[2L] ) {
            ddflag <- ifelse(tmp$dd < x$windsector[1L] | tmp$dd > x$windsector[2L], 1, 2) 
        } else {
            ddflag <- ifelse(tmp$dd > x$windsector[1L] | tmp$dd < x$windsector[2L], 1, 2) 
        }
    } else { ddflag <- rep(2, nrow(tmp)) }
    plot(tmp$dd, type = "n", xaxt = "n", ylab = "", ylim = c(0, 360), yaxt = "n")
    add_boxes(tmp$prob)
    points(tmp$dd, col = c("gray50","black")[ddflag], pch = c(1, 19)[ddflag], cex = c(.3, .5)[ddflag])
    axis(side = 2, at = seq(45, 360 - 45, by = 45))
    mtext(side = 2, line = 3, "wind direction")
    if ( ! is.null(x$windsector) ) abline(h = x$windsector, col = "gray", lty = 3)

    # Adding wind speed
    par(new = TRUE)
    plot(tmp$ff, lwd = 2, col = "orange", ylim = c(0, max(tmp$ff, na.rm = TRUE)) * 1.05, yaxt = "n", xaxt = "n")
    axis(side = 4, at = pretty(tmp$ff))
    mtext(side = 4, line = 3, "wind speed")

    # Foehn prob
    plot(tmp$prob * 100, type = "n", ylab = NA, ylim = c(-4,104), yaxs = "i") 
    add_boxes(tmp$prob)
    lines(tmp$prob * 100, col = "blue")
    abline(h = seq(20, 80, by = 20), col = "gray", lty = 3)
    mtext(side = 2, line = 3, "foehn probability")
    # FIXME: remove probX, only for testing
    lines(tmp$probX * 100, col = 3, lwd = 1, lty = 3)
    if ( ! is.null(xtra) ) lines(xtra * 100, col = 2, lty = 5)
    if ( is.null(xtra) ) {
        legend("left", bg = "white", col = c("blue"), lty = c(1), legend = c("phoeton"))
    } else {
        legend("left", bg = "white", col = c("blue", "red"), lty = c(1,5), legend = c("phoeton", "xtra (flexmix)"))
    }

    # Adding a title to the plot
    title <- sprintf("Foehn Diagnosis %s to %s", min(index(tmp)), max(index(tmp)))
    mtext(side = 3, outer = TRUE, title, font = 2, cex = 1.2, line = 0.5)


}

standardize_model_matrix <- function(X) {
    # Scale covariates
    tmp <- scale(X[,-1])
    scaled_center = attr(tmp, "scaled:center")
    scaled_scale  = attr(tmp, "scaled:scale")
    X[,-1] <- tmp; rm(tmp)
    attr(X, "scaled:center") <- scaled_center
    attr(X, "scaled:scale")  <- scaled_scale
    return(X)
}

destandardize_coefficients <- function(beta, X) {
    scaled_center = attr(X, "scaled:center")
    scaled_scale  = attr(X, "scaled:scale")
    # Descaling intercept
    beta[1L,]  <- beta[1L,] - sum(beta[-1L,] * scaled_center / scaled_scale)
    # Descaling all other regression coefficients
    beta[-1L,] <- beta[-1L,] / scaled_scale
    return(beta)
}

