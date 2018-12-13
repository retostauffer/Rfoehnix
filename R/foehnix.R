# -------------------------------------------------------------------
# - NAME:        foehnix.R
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
# - L@ST MODIFIED: 2018-12-13 16:38 on marvin
# -------------------------------------------------------------------




# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
foehnix.noconcomitant.fit <- function(y, logitX, family,
                    maxit = 100L, tol = 1e-8, verbose = FALSE, ...) {

    # Lists to trace log-likelihood path and the development of
    # the coefficients during EM optimization.
    llpath   <- list()
    coefpath <- list()

    
    # Given the initial probabilities: calculate parameters
    # for the two components (mu1, logsd1, mu2, logsd2) given
    # the selected family and calculate the a-posteriori probabilities.
    z     <- as.numeric(y >= mean(y))
    theta <- family$theta(y, z, init = TRUE) # M-step

    # Initial probability: fifty/fifty!
    prob <- rep(.5, length(y))

    # EM algorithm: estimate probabilities (prob; E-step), update the model
    # given the new probabilities (M-step). Always with respect to the
    # selected family.
    iter <- 0
    while ( iter <= maxit ) {
        iter <- iter + 1;

        # E-step: calculate a-posteriori probability
        post  <- family$posterior(y, prob, theta)

        # M-step: update probabilites and theta
        prob <- mean(post)
        theta <- family$theta(y, post)

        # Store log-likelihood and coefficients of the current
        # iteration.
        llpath[[iter+1]] <- family$loglik(y, post, prob, theta)
        coefpath[[iter+1]] <- as.data.frame(theta)
        cat(sprintf("EM iteration %d/%d, ll = %10.2f\r", iter, maxit, llpath[[iter+1]]))

        # If the log-likelihood decreases: proceed!
        if ( iter == 1 ) next

        # If the log-likelihood improvement falls below the
        # specified tolerance we assume that the algorithm
        # converged: stop iteration.
        if ( (llpath[[iter+1]]$full - llpath[[iter]]$full) < tol ) break
    }; cat("\n")

    # Check if algorithm converged before maxit was reached
    converged <- ifelse(iter < maxit, TRUE, FALSE)

    # Combine to data.frame (number of rows corresponds to iter + 1)
    llpath    <- do.call(rbind, llpath)
    coefpath  <- do.call(rbind, coefpath)

    # In this case we have no concomitants, however, the log-likelihood
    # contribution of the concomitants is not == 0 (as log(0) is set to
    # log(sqrt(.Machine$double.eps)) to avoid -Inf). Fix this here.
    llpath$concomitant <- 0; llpath$full <- llpath$component

    # Return a list with results
if ( inherits(y, "binned") ) stop("Stop, requires changes on computation of BIC!")
    ll   <- tail(llpath$full, 1)
    rval <- list(loglik     = ll,
                 edf        = ncol(coefpath),
                 AIC        = - 2 * ll + 2 * ncol(coefpath),
                 BIC        = - 2 * ll + log(length(y)) * ncol(coefpath),
                 loglikpath = llpath,
                 coefpath   = coefpath)
    class(rval) <- c("foehnix.noconcomitant.fit", "foehnix.fit")
    # Return
    return(rval)
}


# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
foehnix.unreg.fit <- function(formula, data, windsector = NULL, family = "gaussian",
                    maxit = 100L, tol = 1e-8, standardize = TRUE,
                    verbose = FALSE, ...) {
    timing <- Sys.time() # Measure execution time
    print("hallo from foehix.unreg")
    #rval$time <- as.numeric(Sys.time() - timing, units = "mins")
}


# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------
foehnix.reg.fit <- function(formula, data, windsector = NULL, family = "gaussian",
                    maxit = 100L, tol = 1e-8, standardize = TRUE,
                    alpha = NULL, nlambda = 100L, verbose = FALSE, ...) {
    timing <- Sys.time() # Measure execution time
    print("hallo from foehix.reg")
    #rval$time <- as.numeric(Sys.time() - timing, units = "mins")
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
foehnix <- function(formula, data, windsector = NULL, family = "gaussian",
                    maxit = 100L, tol = 1e-8,
                    standardize = TRUE, alpha = NULL, nlambda = 100L,
                    verbose = FALSE, ...) {

    timing <- Sys.time() # Measure execution time

    # Loading family object
    family <- match.arg(family, c("gaussian", "cgaussian", "tgaussian", "logistic", "clogistic", "tlogistic"))
    family <- get(sprintf("foehnix_%s", family))()

    # Stop if the main covariate for the flexible Gaussian mixture model
    # is not a valid variable name.
    left  <- as.character(formula)[2]
    if ( ! length(as.character(as.list(formula)[[2]])) == 1 )
        stop("Unsuitable formula given. Exactly one term allowed on the left hand side.")
    ##stopifnot(grepl("^\\S+$", left) & ! grepl("[+~]", left))

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

    # Check if we have multiple columns with constant values.
    # This would lead to a non-identifiable problem.
    if( sum(apply(mf, 2, function(x) length(unique(na.omit(x)))) <= 1) > 1 )
        stop("Multiple columns with constant values in model.matrix. Stop!")


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
#  regression estimation (only if lambda.min is "auto")
#   if ( lambda.min == "auto" & ncol(mf) > 2 ) {
#       tmp <- cor(na.omit(mf[,-1])); diag(tmp) <- 0
#       if ( max(abs(tmp)) > .75 ) lambda.min <- "AIC"
#   }

    # Setting up the model matrix for the concomitant model (logit model).
    logitX <- model.matrix(formula, data = data[idx_take,])
    if ( standardize ) logitX <- standardize(logitX)

    # Non-concomitant model
    if ( length(labels(terms(formula))) == 0 ) {
        rval <- do.call("foehnix.noconcomitant.fit", list(
                 y = y, logitX = logitX, family = family, maxit = maxit, tol = tol))
    } else if ( is.null(alpha) ) {
        rval <- do.call("foehnix.unreg.fit", arg)
    } else {
        rval <- do.call("foehnix.reg.fit", arg)
    }
print(rval)

stop(' ---------- stop in foehnix -------------- ')

    # Regularization for the logit model (concomitant model) can be either
    # loglik (no regularization), AIC, or BIC. In case of AIC and BIC
    # the optimal penalization is based on AIC/BIC criteria using
    # a ridge penalization. Requires to estimate
    # the logit model multiple times for different lambdas.
##    lambda.min <- match.arg(lambda.min, c("auto","loglik", "AIC", "BIC"))
##    family     <- match.arg(family, c("gaussian", "logistic"))
##    if ( ! inherits(standardize, "logical") )
##        stop("Input \"standardize\" has to be logical (TRUE or FALSE).")
##    if ( ! inherits(data, "zoo") )
##        stop("Input \"data\" to foehndiag needs to be a time series object of class \"zoo\".")


    # Initial parameters for the concomitant model (ccmodel)
    # as.numeric(y > median(y)) is the initial best guess component assignment.
    # Force standardize = FALSE as logitX is already standardized if input
    # standardize = TRUE for the main function foehnix and advanced_foehnix.
    prob <- rep(0.5, length(y))
    post <- do.call(sprintf("foehnix_%s_posterior", family), list(y = y, prob = prob, theta = theta))
    ccmodel <- iwls_logit(logitX, post, standardize = FALSE,
                          maxit = tail(maxit, 1), tol = tail(tol, 1),
                          nlambda = nlambda, verbose = verbose)

    # Initial parameters
    if ( verbose) {
        cat("\nInitial parameters:\n")
        print(matrix(c(theta$mu1, exp(theta$logsd1), theta$mu2, exp(theta$logsd2)), ncol = 2,
                     dimnames = list(c("mu", "sd"), c("Comp.1", "Comp.2"))))
    }


    # calculate current probabilities (probability to be in second cluster)
    # ccmodel$coef are the non-standardized coefficients (alpha)
    prob <- plogis(drop(logitX %*% ccmodel$coef))

    # If standardize = TRUE: standardize model matrix for
    # the concomitant model (logitX)
    if ( standardize ) logitX <- standardize_model_matrix(logitX)
    stop('xx')


    # Start optimization using weighted empirical moments for
    # location and scale of the two Gaussian distributions plus
    # an IWLS solver for the logistic regression model given the
    # concomitant variables.
    # Optimization
    ll0      <- NULL   # Initial value for the log-likelihood sum
    llpath   <- list() # List to store log-likelihood path
    coefpath <- list() # List to store coefficient path
    regpath  <- list() # Store regularization path (if needed)
    iter     <- 0      # Iteration index for the EM algorithm

    # Initial likelihoods
    post <- do.call(sprintf("foehnix_%s_posterior", family),
                    list(y = y, prob = prob, theta = theta))
    llpath[[1]] <- do.call(sprintf("foehnix_%s_loglik", family),
                      list(y = y, post = post, prob = prob, theta = theta))
    coefpath[[1]] <- cbind(as.data.frame(theta), as.data.frame(t(ccmodel$beta)))

    # Helper function to extract regularization path
    regpath_fun <- function(x, selected)
        data.frame(lambda = x$lambda, edf = x$edf, AIC = x$AIC, BIC = x$BIC,
                   selected = ifelse(selected == x$lambda, TRUE, FALSE))


###    # Getting lambdas for ridge penalty
###    if ( lambda.min %in% c("AIC", "BIC") ) {
###        lambdas <- get_lambdas(nlambda, logitX, post, maxit, tol)
###    } else { lambdas <- NULL }
    lambdas <- NULL

    # Perform EM algorithm
    while ( iter < maxit[1L] ) { 

        # Increase iteration counter
        iter <- iter + 1

        ## E-Step ##
        # Calculate the posterior weights
        post <- do.call(sprintf("foehnix_%s_posterior", family),
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
        ####if ( is.null(lambdas) ) {
        ccmodel <- iwls_logit(logitX, post, ccmodel$coef, standardize = FALSE,
                              maxit = tail(maxit, 1), tol = tail(tol, 1),
                              verbose = verbose)
        ####} else {
        ####    tmp <- list()
        ####    for ( lambda in lambdas ) {
        ####        m <- iwls_logit(logitX, post, ccmodel$coef, standardize = FALSE,
        ####                        maxit = tail(maxit, 1), tol = tail(tol, 1),
        ####                        lambda = lambda, verbose = verbose)
        ####        # Break early if increase AIC/BIC (as specified for lambda.min)
        ####        # is smaller than the tolerance.
        ####        if ( length(tmp) > 1 ) {
        ####            #TODO: manual, state that algorithm stops early if
        ####            # no changes can be see anymore with decreased lambda.
        ####            if ( (tmp[[length(tmp)]][[lambda.min]] - m[[lambda.min]]) < tol ) break
        ####        }
        ####        tmp[[length(tmp)+1]] <- m
        ####    }
        ####    # Search for iteration with lowest criterium ("score")
        ####    tmp_idx <- which.min(sapply(tmp, function(x, score) x[[score]], score = lambda.min))
        ####    # Append information to regpath (regularization path)
        ####    regpath[[iter]] <- do.call(rbind, lapply(tmp, regpath_fun, tmp[[tmp_idx]]$lambda))
        ####    # Pick the one model we need, drop tmp
        ####    ccmodel <- tmp[[tmp_idx]]; rm(tmp)
        ####}
        cat("\n")


        # Update the probabilities using the non-standardized coefficients (alpha)
        # from the concomitant model (ccmodel)
        prob <- plogis(drop(logitX %*% ccmodel$coef)) # Update probabilities

        # Calculate/trace loglik
        ll <- do.call(sprintf("foehnix_%s_loglik", family),
                      list(y = y, post = post, prob = prob, theta = theta))
        if ( !is.finite(ll$full) ) browser()
        llpath[[iter + 1]] <- ll
        coefpath[[iter + 1]] <- cbind(as.data.frame(theta), as.data.frame(t(ccmodel$beta)))

        # Initial log-likelihood given the initial guess
        if ( is.null(ll0) ) { ll0 <- ll$full - 1000 }

        if ( verbose ) cat(sprintf("EM step %3d/%3d, log-likelihood sum: %10.5f\n", iter, maxit[1L], ll$full))

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
                           coefpath = do.call(rbind, coefpath), ccmodel = ccmodel,
                           maxit = maxit[1L], tol = tol[1L], converged = ifelse(iter < maxit, TRUE, FALSE))
    rval$data <- data
    rval$windsector <- windsector

    rval$samples <- list(total = nrow(data), na = length(idx_na), wind = length(idx_wind), taken = length(idx_take))

    # Calculate final probabilities using the non-standardized coefficients (alpha)
    # from the final concomitant model (ccmodel).
    # d1: density of cluster 1 given the parameters for the two Gaussian
    #     distributions/clusters and the posterior information
    # d2: density of cluster 2 (see above)
    post <- do.call(sprintf("foehnix_%s_posterior", family),
                        list(y = y, prob = prob, theta = theta))

    # The fohen probability vector: create an object of the
    # same length and class as input "data" and:
    # - fill all with NA (default)
    # - observations outside the requested wind sector get a 0 (no foehn)
    # - those observations which entered the models get their modelled
    #   foehn probability.
    tmp <- rep(NA, ncol(data))
    tmp[idx_take] <- post
    tmp[idx_wind] <- 0
    rval$prob <- zoo(tmp, index(data))

    # Store execution time
    rval$time <- as.numeric(Sys.time() - timing, units = "mins")

    # Return new object
    class(rval) <- "foehnix"
    return(rval)
}


