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
# - L@ST MODIFIED: 2018-12-16 16:57 on marvin
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# IWLS optimizer for logistic regression model (concomitant model)
# -------------------------------------------------------------------
iwls_logit <- function(X, y, beta = NULL, lambda = NULL, standardize = TRUE,
                       maxit = 100L, tol = 1e-8, verbose = FALSE, ...) {

    # Checking inputs. Constant covariates (concomitant variables)
    # are not allowed except one column (intercept).
    if(sum(apply(X, 2, sd) == 0) > 1) stop("Multiple columns with constant values!")
    if ( min(y) < 0 | max(y) > 1 ) stop("y values out of range. Have to be within ]0,1[.")

    # Standardize design matrix?
    if ( standardize ) X <- standardize(X)

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

    iter   <- 0L
    llpath <- list()
    while( iter < maxit ) {
        # Increase iteration counter
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
        llpath[[iter]]   <- sum(y * eta - log(1 + exp(eta)))

        # Continue if iter == 1 and/or is.null(tol)
        if ( iter == 1 | is.null(tol) ) next

        if ( verbose) cat(sprintf("Iteration %d, ll = %15.4f, %s\r", iter, ll,
            ifelse(is.null(lambda), "unregularized", sprintf("lambda = %10.4f", lambda))))

        # Check if improvement falls below tolerance
        if ( (llpath[[iter]] - llpath[[iter - 1]]) < tol ) break

    }
    llpath <- structure(do.call(rbind, llpath), dimnames = list(NULL, "loglik"))

    # Not converged? Drop warning.
    if ( ! is.null(tol) & iter == maxit ) warning("IWLS solver for logistic model did not converge.")

    # Just naming the column containing the coefficients.
    colnames(beta) <- c("concomitant")

    # Calculate effective degrees of freedom
    if ( is.null(lambda) ) { reg <- 0 } else { reg <- diag(ncol(X)) * lambda; reg[1,1] <- 0 }
    edf <- sum(diag(t(X*w) %*% (X*w) %*% solve(t(X*w) %*% (X*w) + reg)))

    # Unscale coefficients if needed
    ll   <- tail(llpath, 1)
    rval <- list(lambda = lambda, edf = edf, loglik = ll, AIC = -2 * ll + 2 * edf,
                 BIC = -2 * ll + log(nrow(X)) * edf,
                 converged = ifelse(iter < maxit, TRUE, FALSE))
    rval$beta <- beta
    rval$coef <- if ( standardize ) destandardize_coefficients(beta, X) else beta

    # Return list object containing
    # - edf (numeric): effective degrees of freedom
    # - loglik (numeric): log-likelihood of the model
    # - converged (logical): flag whether or not the iterative solver converged
    # - beta/coef (matrix): standardized and de-standardized coefficients. If
    #       input "standardized = FALSE" beta and coef are identical.
    class(rval) <- "ccmodel"
    return(rval)
}

coef.ccmodel <- function(x, which = "coef", ...) {
    which <- match.arg(which, c("coef", "beta"))
    c <- as.data.frame(t(x[[which]][,1]))
    names(c) <- sprintf("cc.%s", names(c))
    c
}





