# -------------------------------------------------------------------
# - NAME:        phoeton.R
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
# - L@ST MODIFIED: 2018-12-12 09:32 on marvin
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# IWLS optimizer for logistic regression model (concomitant model)
# -------------------------------------------------------------------
iwls_logit <- function(X, y, beta = NULL, lambda = NULL, standardize = TRUE,
                       maxit = 100L, tol = 1e-8, ...) {

    # Checking inputs. Constant covariates (concomitant variables)
    # are not allowed except one column (intercept).
    if(sum(apply(X, 2, sd) == 0) > 1) stop("Multiple columns with constant values!")
    if ( min(y) < 0 | max(y) > 1 ) stop("y values out of range. Have to be within ]0,1[.")

    # Standardize design matrix?
    if ( standardize ) X <- standardize_model_matrix(X)

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

    # Calculate log-likelihood given initial parameters beta.
    # Initialize ll0 as ll - 1000 such that we don't stop in iteration 0.
    ll  <- sum(y * eta - log(1 + exp(eta)))
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
        ll   <- sum(y * eta - log(1 + exp(eta)))
        cat(sprintf("Iteration %d, ll = %15.4f, %s\r", iter, ll,
                    ifelse(is.null(lambda), "unregularized", sprintf("lambda = %10.4f", lambda))))
    }

    # Not converged? Drop warning.
    if ( iter == maxit ) warning("IWLS solver for logistic model did not converge.")

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
# The simple version for the foehn diagnosis using empirical weighted
# moments for the parameters of the two Gaussian clusters, and a
# logistic regression model for the concomitant part.
# TODO: check what's going on if no intercept is requested by the user
#       for the concomitant model (e.g., ff ~ -1 + rh). The standardize/
#       destandardize function should technically be ready to support
#       this.
# -------------------------------------------------------------------
phoeton <- function(formula, data, windsector = NULL, maxit = 100L, tol = 1e-8, lambda.min = "auto",
                      standardize = TRUE, family = "gaussian", nlambda = 100L, ...) {

    timing <- Sys.time()

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
    if ( lambda.min == "auto" && ncol(mf) > 2 ) {
        tmp <- cor(na.omit(mf)[,-1]); diag(tmp) <- 0
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
    ccmodel <- iwls_logit(logitX, as.numeric(y > median(y)), maxit = tail(maxit, 1), tol = tail(tol, 1), nlambda = nlambda)
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
            m <- bfgs_logit(logitX, as.numeric(y >= median(y)), lambda = lambda, maxit = maxit, tol = tol)
            sum(abs(m$beta[which(!grepl("^\\(Intercept\\)$", rownames(m$beta))),]))
        }
        x <- sapply(lambdas, coef_sum_fun, logitX = logitX, post = post, maxit = maxit, tol = tol)
        # Pick the lambda where sum of parameters is smaller than a 
        # certain threshold OR take maximum of lambdas tested.
        lambdas <- exp(seq(min(which(x < 0.1), length(x)), -8, length = as.numeric(nlambda)))
    } else { lambdas <- NULL }

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
            ccmodel <- iwls_logit(logitX, post, ccmodel$beta,
                                  maxit = tail(maxit, 1), tol = tail(tol, 1))
        } else {
            tmp <- list()
            for ( la in lambdas ) {
                m <- iwls_logit(logitX, post, ccmodel$beta,
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
    class(rval) <- "phoeton"
    return(rval)
}


