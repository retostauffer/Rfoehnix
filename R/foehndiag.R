# -------------------------------------------------------------------
# - NAME:        simple.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-11-28
# -------------------------------------------------------------------
# - DESCRIPTION: Development test script for foehton, simple method.
# -------------------------------------------------------------------
# - EDITORIAL:   2018-11-28, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-11-28 17:09 on marvin
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# IWLS optimizer for logistic regression model (concomitant model)
# -------------------------------------------------------------------
iwls_logit <- function(X, y, beta = NULL, maxit = 20L, tol = 1e-8, ...) {

    ## initialize regression coefficients if needed
    if ( is.null(beta) ) beta <- rep.int(0, ncol(X)) ## FIXME: there is surely a better solution!
    ## Calculate linear predictor eta
    eta <- drop(X %*% beta)
    ## Apply link function on linear predictor to get response mu
    ## (probabilities)
    mu  <- plogis(eta) 
    ## Calculate log-likelihood given initial parameters beta
    ll  <- sum(ifelse(y > 0, log(mu), log(1 - mu))) ## FIXME: possibly avoid ifelse()
    ## Initial value
    ll0 <- ll - 1000
    ## Maximum number of iterations
    iter <- 0

    ## IWLS
    while( (iter < maxit) & ( (ll - ll0) > tol ) ) {
        ll0 <- ll
        iter <- iter + 1L
        w <- sqrt(mu * (1 - mu))

        #beta <- lm.fit(X * w, eta * w + (y - mu)/w)$coefficients
        beta <- solve(t(X*w) %*% (X*w)) %*% t(X*w) %*% (eta * w + (y - mu) / w)
        eta  <- drop(X %*% beta)
        mu   <- plogis(eta)
        ll   <- sum(ifelse(y > 0, log(mu), log(1 - mu)))
    }
    # Return updated/estimated parameters
    return(beta)
}


# -------------------------------------------------------------------
# The simple version for the foehn diagnosis using empirical weighted
# moments for the parameters of the two Gaussian clusters, and a
# logistic regression model for the concomitant part.
# -------------------------------------------------------------------
foehndiag <- function(formula, data, ...) {
    left  <- as.character(formula)[2]
    right <- as.character(formula)[3]
    stopifnot(grepl("^\\S+$", left))

    # Initial guess of the two clusters
    y  <- data[,left]

    # model frame/model matrix for the logistic regression model
    logitX <- model.matrix(model.frame(formula, data = data), data = data) 

    # Initialize regression coefficients
    logsd  <- function(y) log(sqrt(sum((y-mean(y))^2) / length(y)))
    theta  <- list(mu1 = as.numeric(quantile(y, 0.25)), logsd1 = logsd(y),
                   mu2 = as.numeric(quantile(y, 0.75)), logsd2 = logsd(y))

    # Initial parameters for the concomitant model
    # as.numeric(y > median(y)) is the initial best guess cluster
    # assignment.
    alpha <- iwls_logit(logitX, as.numeric(y > median(y)))

    # Start optimization using weighted empirical moments for
    # location and scale of the two Gaussian distributions plus
    # an IWLS solver for the logistic regression model given the
    # concomitant variables.
    # Optimization
    llpath <- list()
    t <- Sys.time()
    for ( i in 1:10 ) {

        ##### E-Step ######
        # calculate current probabilities (probability to be in second cluster)
        prob <- plogis(drop(logitX %*% alpha))

        # Calculate the posterior weights
        post <- (prob) * dnorm(y, theta$mu2, exp(theta$logsd2)) /
                ((1 - prob) * dnorm(y, theta$mu1, exp(theta$logsd1)) +
                 prob * dnorm(y, theta$mu2, exp(theta$logsd2)))
    
        ##### M-Step ######
        # Empirical moments for the two clusters (mu1/sd1 and mu2/sd2)
        theta$mu1    <- 1 / sum(1 - post) * sum( y * (1 - post))
        theta$mu2    <- 1 / sum(post) * sum( y * post)
        theta$logsd1 <- log(sqrt( 1 / sum(1 - post) * sum( (y - theta$mu1)^2 * (1 - post))))
        theta$logsd2 <- log(sqrt( 1 / sum(post)     * sum( (y - theta$mu2)^2 *     (post))))

        # Re-create the vector theta with c(mu1, log(sigma1), mu2, log(sigma2))
        alpha <- iwls_logit(logitX, prob, alpha)

        # Apend likelihood
        ##ll[i] <- lli
        prob <- plogis(drop(logitX %*% alpha)) # Update probabilities


        # Calculate/trace loglik
        ll <- list(Q1 =   sum(post       * dnorm(y, theta$mu2, exp(theta$logsd2), log = TRUE))
                        + sum((1 - post) * dnorm(y, theta$mu1, exp(theta$logsd1), log = TRUE)),
                   Q2 = sum((1 - post) * log(1 - prob) + post * log(prob)))
        ll$ll <- ll$Q1 + ll$Q2
        llpath[[i]] <- ll

        cat(sprintf("  EM step  %2d: logLik = %10.3f\n", i, ll$ll))
        cat(sprintf("       Gauss 1: %10.5f %10.5f\n", theta$mu1, exp(theta$logsd1)))
        cat(sprintf("       Gauss 2: %10.5f %10.5f\n", theta$mu2, exp(theta$logsd2)))
    
    }
    print(theta)
    print(alpha)
    #return(llpath)
    # Calculate final probabilities and classes
    prob <- plogis(drop(logitX %*% alpha))
    d1 <- pnorm(y, theta$mu1, exp(theta$logsd1)) * (1 - prob)
    d2 <- pnorm(y, theta$mu2, exp(theta$logsd2)) * prob
    return(d2 / (d2 + d1))
}





