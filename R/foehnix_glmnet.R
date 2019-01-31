

foehnix_glmnet <- function(y, x, arg) {

    # Likelihood function
    llfun <- function(par, y, x) {
        eta <- as.numeric(x %*% par)
        return(sum(y * eta - log(1 + exp(eta))))
    }

    # Index of non-intercept columns in the design matrix
    idx_noIC <- grep("[^\\(Intercept\\)]$", colnames(x))

    # Extract the 'min' argument, minimization argument for the
    # glmnet model.
    min <- arg$min; arg$min <- NULL

    # If lambda.min or lambda.1se have been choosen: perform
    # cross-validation.
    arg <- append(list(x = x[,idx_noIC], y = y, family = "binomial",
                       intercept = TRUE), unclass(arg))

    # Estimate the model
    mod  <- do.call(glmnet::glmnet, arg)

    # Calculate log-likelihood sum and degrees of freedom
    loglik <- apply(coef(mod), 2, llfun, y = y, x = x)
    edf    <- apply(coef(mod), 2, function(x) sum(!x==0))

    if(min == "AIC") {
        AIC <- drop(-2 * loglik + 2 * edf) 
        idx <- min(which(AIC == min(AIC)))
    } else if(min == "BIC") {
        BIC <- drop(-2 * loglik + log(nrow(x)) * edf) 
        idx <- min(which(BIC == min(BIC)))
    } else {
        idx <- min(which(loglik == max(loglik)))
    }

    # Extracting beta, calculate hessian
    beta    <- matrix(coef(mod)[,idx], ncol = 1,
                      dimnames = list(colnames(x), "concomitant"))

    # Empirical hessian
    hess    <- optimHess(as.numeric(beta), llfun, x = x, y = y)
    beta.se <- matrix(sqrt(diag(-solve(hess))), ncol = 1,
                      dimnames = list(colnames(x), NULL))

    # Calculate log-likelihood once for the final model
    ll  <- llfun(beta, y, x)
    edf <- sum(!beta == 0)

    # Create the return object (ccmodel class)
    ccmodel <- list(call       = match.call(),
                    lambda     = mod$lambda.min,
                    edf        = edf,
                    loglik     = ll,
                    iterations = NULL, 
                    AIC        = setNames(drop(-2 * ll + 2 * edf), "AIC"),
                    BIC        = setNames(drop(-2 * ll + log(nrow(x)) * edf), "BIC"),
                    converged  = TRUE,
                    coef       = beta,
                    beta       = beta,
                    beta.se    = beta.se)

    class(ccmodel) <- "ccmodel"
    return(ccmodel)

}
