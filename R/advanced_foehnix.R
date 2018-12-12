# -------------------------------------------------------------------
# - NAME:        advanced_foehnix.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-11-28
# -------------------------------------------------------------------
# - DESCRIPTION: Code to estimate Gaussian and logistic two-component
#                mixture models with optional truncation and
#                censoring. Gradient based optimization based on
#                BFGS optimizer.
# -------------------------------------------------------------------
# - EDITORIAL:   2018-11-28, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-12 10:19 on marvin
# -------------------------------------------------------------------



# -------------------------------------------------------------------
# IWLS optimizer for logistic regression model (concomitant model)
# -------------------------------------------------------------------
bfgs_logit <- function(X, y, beta = NULL, lambda = NULL, standardize = TRUE,
                       maxit = 100L, tol = 1e-8, ...) {

    # Checking inputs. Constant covariates (concomitant variables)
    # are not allowed except one column (intercept).
    if(sum(apply(X, 2, sd) == 0) > 1) stop("Multiple columns with constant values!")
    if ( min(y) < 0 | max(y) > 1 ) stop("y values out of range. Have to be within ]0,1[.")

    # Standardize design matrix?
    if ( standardize ) X <- standardize_model_matrix(X)

    # initialize regression coefficients if needed
    if ( is.null(beta) ) beta <- rep.int(0, ncol(X)) # FIXME: there is surely a better solution!

    # Calculate logistic regression likelihood
    loglik <- function(beta, post, X, lambda) {
        eta <- drop(X %*% beta) 
        # Penalize all non-intercept coefficients
        idx <- which(!grepl("^\\(Intercept\\)$", colnames(X)))
        if ( is.null(lambda) | length(idx) == 0 ) return(sum(y * eta - log(1 + exp(eta))))
        # Penalized log-likelihood sum
        sum(y * eta - log(1 + exp(eta))) - lambda / 2 * sum(beta[idx]^2)
    }
    # Gradient for the 
    gradfun <- function(beta, post, X, lambda) {
        eta  <- drop(X %*% beta)
        prob <- plogis(eta)
        idx  <- which(!grepl("^\\(Intercept\\)$", colnames(X)))
        grad <- (post / prob - (1-post) / (1-prob)) * exp(eta) / (1 + exp(eta))^2

        res <- colSums(X * grad)
        for ( i in which(!grepl("^\\(Intercept\\)$", colnames(X))) )
            res[i] = res[i] - lambda * as.numeric(beta[i])
        return(res)
        return(colSums(X * grad))
    }


    # Start optimizer. fnscale = -1 makes the minimization problem
    # a maximization problem (maximum likelihood)
    post <- y
    if ( is.null(lambda) ) lambda <- 0
    opt <- optim(beta, fn = loglik, gr = gradfun,
                 X = X, post = post, lambda = lambda, method = "BFGS",
                 control = list(maxit = maxit, abstol = tol, fnscale = -1, ...))

    # Create coefficient matrix of estimated parameters "beta"
    beta <- matrix(opt$par, ncol = 1, dimnames = list(colnames(X), "concomitant"))

    # Just naming the column containing the coefficients.
    colnames(beta) <- c("concomitant")

    # Calculate effective degrees of freedom
    # TODO: regularization not yet implemented
    ##if ( is.null(lambda) ) { reg <- 0 } else { reg <- diag(ncol(X)) * lambda; reg[1,1] <- 0 }
    ##edf <- sum(diag(t(X*w) %*% (X*w) %*% solve(t(X*w) %*% (X*w) + reg)))

    ll <- loglik(beta, post, X, lambda)
    edf <- ncol(X)
    cat(sprintf("Concomitant model, ll = %15.4f, %s\r", ll,
                ifelse(is.null(lambda), "unregularized", sprintf("lambda = %10.4f", lambda))))

    # Unscale coefficients if needed
    rval <- list(lambda = lambda, edf = edf, loglik = ll, AIC = -2 * ll + 2 * edf,
                 BIC = -2 * ll + log(nrow(X)) * edf,
                 converged = ifelse(opt$convergence == 0, TRUE, FALSE))
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
# The simple version for the foehn diagnosis using empirical weighted
# moments for the parameters of the two Gaussian clusters, and a
# logistic regression model for the concomitant part.
# TODO: check what's going on if no intercept is requested by the user
#       for the concomitant model (e.g., ff ~ -1 + rh). The standardize/
#       destandardize function should technically be ready to support
#       this.
# -------------------------------------------------------------------
advanced_foehnix <- function(formula, data, windsector = NULL, maxit = 100L, tol = 1e-8, lambda.min = "auto",
                      standardize = TRUE, family = "gaussian", nlambda = 100L, ...) {

    timing <- Sys.time()

    # Prepare inputs. foehnix_prepare returns a list of required objects.
    x <- foehnix_prepare(formula, data, windsector, maxit, tol, lambda.min,
                         standardize, family, nlambda)
    # Attach to current environment to be used in the EM algorithm.
    for ( n in names(x) ) eval(parse(text = sprintf("%1$s <- x$%1$s", n))); rm(x)


     #TODO: Should be moved into the prepare function.
    # If lambda.min is not loglik: ridge penalization
    if ( lambda.min %in% c("AIC", "BIC") ) {
        # If nlambda is not a positive integer: stop.
        stopifnot(inherits(nlambda, c("integer", "numeric")))
        stopifnot(nlambda >= 0)
        # Find large lambda where all parameters are close to 0.
        # Trying lambdas between exp(6) and exp(12).
        lambdas <- exp(seq(6, 12, by = 2))
        # Fitting logistic regression models with different lambdas.
        coef_sum_fun <- function(lambda, logitX, post, maxit, tol) {
            # As response a first guess y >= median(y) is used.
            # Force standardize = FALSE as logitX is already standardized if
            # standardize == TRUE for this function.
            m <- bfgs_logit(logitX, as.numeric(y >= median(y)), standardize = FALSE,
                            lambda = lambda, maxit = maxit, tol = tol)
            sum(abs(m$beta[which(!grepl("^\\(Intercept\\)$", rownames(m$beta))),]))
        }
        x <- sapply(lambdas, coef_sum_fun, logitX = logitX, post = post, maxit = maxit, tol = tol)
        # Pick the lambda where sum of parameters is smaller than a 
        # certain threshold OR take maximum of lambdas tested.
        lambdas <- exp(seq(min(which(x < 0.1), length(x)), -8, length = as.numeric(nlambda)))
    } else { lambdas <- NULL }


    # Helper function to compute the log-likelihoood sum contribution
    # of the two components.
    component_loglik <- function(par, post, y, theta, family) {
        if ( family == "gaussian" ) { dfun <- dnorm }
        else if ( family == "logistic" ) { dfun <- dlogis }
        else { stop(sprintf("Whoops, family = \"%s\" unknown in component_loglik.", family)) }

        # Sum of the posterior weighted log-likelihood of the two components.
        sum((1 - post) * dfun(y, par[1L], exp(par[2L]), log = TRUE)) +
        sum(   post    * dfun(y, par[3L], exp(par[4L]), log = TRUE))
    }

    # Helper functions: convert named list "theta" into unnamed numeric vector
    # "par" used for optimization (I/O for optim(...)).
    theta2par <- function(x) c(theta$mu1, theta$logsd1, theta$mu2, theta$logsd2)
    par2theta <- function(x) list(mu1 = x[1L], logsd1 = x[2L], mu2 = x[3L], logsd2 = x[4L])

    # Start optimization using weighted empirical moments for
    # location and scale of the two Gaussian distributions plus
    # an IWLS solver for the logistic regression model given the
    # concomitant variables.
    # Optimization
    ll0      <- NULL   # Initial value for the log-likelihood sum
    llpath   <- list() # Listto store log-likelihood path
    coefpath <- list() # List to store coefficient path
    iter     <- 0      # Iteration index for the EM algorithm

    # Initial likelihoods
    post <- do.call(sprintf("foehndiag_%s_posterior", family),
                    list(y = y, prob = prob, theta = theta))
    llpath[[1]] <- do.call(sprintf("foehndiag_%s_loglik", family),
                      list(y = y, post = post, prob = prob, theta = theta))
    coefpath <- list()
    coefpath[[1]] <- cbind(as.data.frame(theta), as.data.frame(t(ccmodel$beta)))

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
        opt <- optim(theta2par(theta), fn = component_loglik, #gr = component_gradfun,
                     post = post, y = y, family = family, method = "BFGS",
                     control = list(maxit = maxit, abstol = tol, fnscale = -1, ...))
        theta <- par2theta(opt$par)

        # Update the concomitant model.
        # Using the (possibly) standardized coefficients from the previous iteration
        # as initial parameters (alpha).
        if ( is.null(lambdas) ) {
            # Force standardize = FALSE as logitX is already standardized if input
            # standardize == TRUE for this function.
            ccmodel <- bfgs_logit(logitX, post, ccmodel$beta, standardize = FALSE,
                                  maxit = tail(maxit, 1), tol = tail(tol, 1))
        } else {
            tmp <- list()
            for ( la in lambdas ) {
                # Force standardize = FALSE as logitX is already standardized if input
                # standardize == TRUE for this function.
                m <- bfgs_logit(logitX, post, ccmodel$beta, standardize = FALSE,
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
        llpath[[iter + 1]] <- ll

        coefpath[[iter + 1]] <- cbind(as.data.frame(theta), as.data.frame(t(ccmodel$beta)))

        # Initial log-likelihood given the initial guess
        if ( is.null(ll0) ) { ll0 <- ll$full - 1000 }

        cat(sprintf("EM step %3d/%3d, log-likelihood sum: %10.5f\n", iter, maxit[1L], ll$full))

        # At least do maxit * 0.05 iterations
        if ( iter < (maxit * .05) ) next

    
    
        # Check log-likelihood improvement in the current iteration.
        # If the improvement is smaller than the tolerance the algorithm
        # converged: stop optimization.
        if ( (ll$full - ll0) < tol[1L] ) break
        ll0 <- ll$full
    }; cat("\n")

    # Final coefficients of the concomitant model have to be destandardized
    # if standardize == TRUE.
    if ( ! is.standardized(logitX) ) { coef <- ccmodel$coef }
    else { coef <- destandardize_coefficients(ccmodel$coef, logitX) }

    # Create the return list object (foehnix object)
    rval <- list()
    rval$call <- match.call()
    rval$coef <- list(mu1 = theta$mu1, sd1 = exp(theta$logsd1),
                      mu2 = theta$mu2, sd2 = exp(theta$logsd2),
                      concomitants = coef)

    rval$optimizer <- list(loglik = ll, loglikpath = do.call(rbind, llpath), n.iter = iter,
                           coefpath = do.call(rbind, coefpath),
                           maxit = maxit[1L], tol = tol[1L], converged = ifelse(iter < maxit, TRUE, FALSE))
    rval$data <- data
    rval$windsector <- windsector

    rval$samples <- list(total = nrow(data), na = length(idx_na), wind = length(idx_wind), taken = length(idx_take))

    # Calculate final probabilities using the non-standardized coefficients (alpha)
    # from the final concomitant model (ccmodel).
    # d1: density of cluster 1 given the parameters for the two Gaussian
    #     distributions/clusters and the posterior information
    # d2: density of cluster 2 (see above)
    #pfun <- ifelse(family == "gaussian", pnorm, plogis)
    #d1   <- pfun(y, theta$mu1, exp(theta$logsd1)) * (1 - prob)
    #d2   <- pfun(y, theta$mu2, exp(theta$logsd2)) * prob
    post <- do.call(sprintf("foehndiag_%s_posterior", family),
                        list(y = y, prob = prob, theta = theta))

    # The fohen probability vector: create an object of the
    # same length and class as input "data" and:
    # - fill all with NA (default)
    # - observations outside the requested wind sector get a 0 (no foehn)
    # - those observations which entered the models get their modelled
    #   foehn probability.
    rval$prob <- zoo(data.frame(prob = rep(NA, nrow(data))), index(data))
    rval$prob[idx_take] <- post#d2 / (d2 + d1)
    rval$prob[idx_wind] <- 0

    d1   <- pnorm(y, theta$mu1, exp(theta$logsd1)) * (1 - post)
    d2   <- pnorm(y, theta$mu2, exp(theta$logsd2)) * post
    rval$probX <- zoo(data.frame(prob = rep(NA, nrow(data))), index(data))
    rval$probX[idx_take] <- d2 / (d2 + d1)
    rval$probX[idx_wind] <- 0

    rval$time <- as.numeric(Sys.time() - timing, units = "mins")

    # Return new object
    class(rval) <- "foehnix"
    return(rval)
}


