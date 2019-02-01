
#' Control Object to Allow for Regularized Concomitant Models
#'
#' \code{foehnix} allows to estimate regularized concomitant models
#' based on the R package glmnet. The \code{\link[foehnix]{glmnet.control}}
#' object controls, if specified, the glmnet input arguments.
#'
#' @param min a character, either \code{"AIC"}, \code{"BIC"}, or \code{"loglik"}.
#'        Default is \code{"AIC"}.
#' @param ... additional arguments forwarded to \code{glmnet::glmnet}.
#' @param x an object of class \code{glmnet.control} (print method).
#' @details Note that the two input arguments \code{intercept} and
#' \code{family} (to function \code{glmnet}) cannot be overruled by the
#' foehnix user. In any case a binomial logit model with intercept will
#' be estimated.
#'
#' @export
#' @author Reto Stauffer
glmnet.control <- function(min = "AIC", ...) {
    input <- list(...)
    # Family is fixed, will be ignored in all cases!
    if("family" %in% names(input))
        stop("\"family\" specification for glmnet.control not allowed (forced to binomial logit)")
    if("intercept" %in% names(input))
        stop("\"intercept\" specification for glmnet.control not allowed")


    # Force binomial logit model
    # Force intercept = FALSE as our input model matrix will already
    # contain the intercept.
    args <- list(...)
    # Regualization
    args$min <- match.arg(min, c("AIC", "BIC", "loglik"))
    class(args) <- "glmnet.control"
    return(args)
}

#' @export
#' @author Reto Stauffer
#' @rdname glmnet.control
print.glmnet.control <- function(x, ...) {
    cat("foehnix glmnet.control settings\n")
    for(n in names(x))
        cat(sprintf(" - %s = %s\n", n, as.character(x[[n]])))
}


#' Wrapper Function Around glmnet Logistic Regression
#'
#' The \code{\link[foehnix]{foehnix}} function allows to estimate
#' a regularized logistic regression model for the concomitants.
#' In case \code{\link[foehnix]{glmnet.control}} is provided the
#' EM algorithm uses this function to estimate the regression coefficients
#' of the concomitant model. \code{\link[foehnix]{glmnet.control}} allows
#' to specify options forwarded to \code{glmnet::glmnet} except 
#' family (\code{"binomial"}) and intercept (\code{TRUE}).
#' Depending on \code{\link[foehnix]{glmnet.control}} the AIC, BIC, or
#' log-likelihood criterion will be used.
#'
#' @param y response vector (binary)
#' @param x design matrix. If a column \code{"(Intercept)"} exists it will be
#' dropped (as we force glmnet to estimate an intercept).
#' @param arg list of arguments forwarded to \code{glmnet::glmnet}.
#' 
#' @return Returns an object of class \code{ccmodel} for \code{\link[foehnix]{foehnix}}
#' models.
#'
#' @author Reto Stauffer
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
