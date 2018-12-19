# -------------------------------------------------------------------
# - NAME:        families.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-13
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-13, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-19 08:30 on marvin
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Small print method for foehnix.family objects.
# -------------------------------------------------------------------
print.foehnix.family <- function(x, ...) {

    cat(sprintf("foehnix family of class: %s\n", x$name))

    if ( all(c("left", "right") %in% names(x)) ) {
        if ( "censored" %in% names(x)  ) cat("Censoring thresholds:    ")
        if ( "truncated" %in% names(x) ) cat("Truncation thresholds:   ")
        # Load thresholds
        tmp <- x[c("left", "right")]
        cat(paste(sprintf("%s = %s", names(tmp), tmp), collapse = ", "), "\n")
    }

}
is.truncated <- function(x, ...) UseMethod("is.truncated")
is.truncated.foehnix.family <- function(x, ...) "truncated" %in% names(x)

has.left <- function(x, ...) UseMethod("has.left")
has.left.foehnix.family <- function(x, ...) {
    if ( "left" %in% names(x) ) {
        if ( is.finite(x$left) ) return(TRUE) else return(FALSE)
    }
    return(FALSE)
}
has.right <- function(x, ...) UseMethod("has.right")
has.right.foehnix.family <- function(x, ...) {
    if ( "right" %in% names(x) ) {
        if ( is.finite(x$right) ) return(TRUE) else return(FALSE)
    }
    return(FALSE)
}
    

# -------------------------------------------------------------------
# Logistic distribution family
# -------------------------------------------------------------------
foehnix_logistic <- function() {
    rval <- list(
        name = "logistic",
        # Density function
        d = function(y, mu, sigma, log = FALSE)
            dlogis(y, mu, sigma, log = log),
        # Distribution function 
        p = function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE)
            plogis(q, mu, sigma, lower.tail, log.p),
        # Random sample function for two-component logistic distribution
        r = function(n, mu, sigma, lower.tail = TRUE) {
            if ( length(n) == 1 ) n <- c(floor(n/2), ceiling(n/2))
            c(rlogis(n[1L], mu[1L], sigma[1L]), rlogis(n[2L], mu[2L], sigma[2L]))
        },
        # Calculate log-likelihood sum of the two-component mixture model
        loglik = function(y, post, prob, theta) {
            # Calculate/trace loglik
            eps  <- sqrt(.Machine$double.eps)
            prob <- pmax(eps, pmin(1-eps, prob))
            # plog function and eps are used to void log(0)
            plog <- function(x, eps) ifelse(x < eps, log(eps), log(x))
            eps <- sqrt(.Machine$double.eps)
            # Calculate log-likelihood
            ll <- data.frame(
                    component = sum(post       * dlogis(y, theta$mu2, exp(theta$logsd2), log = TRUE))
                              + sum((1 - post) * dlogis(y, theta$mu1, exp(theta$logsd1), log = TRUE)),
                    concomitant = sum((1 - post) * plog(1 - prob, eps) + post * plog(prob, eps))
                  )
            ll$full <- sum(unlist(ll))
            return(ll)
        },
        # Calculate posterior:
        # Ratio between weighted density of component 2 divided by the sum
        # of the weighted density of both components gives the posterior.
        posterior = function(y, prob, theta) {
            # Calculate posterior:
            # Ratio between weighted density of component 2 divided by the sum
            # of the weighted density of both components gives the posterior.
            (prob) * dlogis(y, theta$mu2, exp(theta$logsd2)) /
            ((1 - prob) * dlogis(y, theta$mu1, exp(theta$logsd1)) +
            prob * dlogis(y, theta$mu2, exp(theta$logsd2)))
        },
        # Update the parameters of the two components.
        # Returns a list containing mu1, logsd1, mu2, and logsd2.
        theta = function(y, post, init = FALSE, ...) {
            mu1 <- sum(y * (1 - post)) / sum(1 - post)
            mu2 <- sum(y * post) / sum(post)
            if ( init ) {
                sigma1 <- sigma2 <- sd(y) * sqrt(3) / pi
            } else {
                sigma1 <- sqrt(sum((y-mu1)^2 * (1-post)) / sum(1-post)) * sqrt(3) / pi
                sigma2 <- sqrt(sum((y-mu2)^2 * (post)) / sum(post)) * sqrt(3) / pi
            }
            list(mu1    = mu1, 
                 logsd1 = ifelse(sigma1 < exp(-6), -6, log(sigma1)),
                 mu2    = mu2,
                 logsd2 = ifelse(sigma2 < exp(-6), -6, log(sigma2)))
        }
    )
    class(rval) <- c("foehnix.family")
    return(rval)
}


# -------------------------------------------------------------------
# Censored logistic distribution family
# -------------------------------------------------------------------
foehnix_clogistic <- function(left = -Inf, right = Inf) {
    if ( ! length(left) == 1 | ! length(right) == 1 )
        stop("Input left/right have to be numeric values of length 1!")
    rval <- list(
        name = "censored logistic", left = left, right = right, censored = TRUE,
        # Density function
        d = function(y, mu, sigma, log = FALSE) {
            .Call("cdclogis", as.numeric(y), as.numeric(mu),
                  as.numeric(sigma), as.numeric(left), as.numeric(right),
                  log, package = "foehnix")
        },
        # Distribution function 
        p = function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE) {
            .Call("cpclogis", as.numeric(q), as.numeric(mu), as.numeric(sigma), 
                  as.numeric(left), as.numeric(right), lower.tail, log.p,
                  package = "foehnix")
        },
        # Random sample function for two-component censored logistic distribution
        r = function(n, mu, sigma, lower.tail = TRUE) {
            stopifnot(length(mu) == 2)
            stopifnot(length(sigma) == 2)
            if ( length(n) == 1 ) n <- c(floor(n/2), ceiling(n/2))
            lower <- if ( lower.tail ) left else right
            upper <- if ( lower.tail ) right else left
            p1 <- rlogis(n[1L], mu[1L], sigma[1L])
            p2 <- rlogis(n[2L], mu[2L], sigma[2L])
            pmax(left, pmin(right, c(p1, p2)))
        },
        # Calculate log-likelihood sum of the two-component mixture model
        loglik = function(y, post, prob, theta) {
            # Avoid 0/1 due to computational reasons
            eps  <- sqrt(.Machine$double.eps)
            prob <- pmax(eps, pmin(1-eps, prob))
            post <- pmax(eps, pmin(1-eps, post))
            # Prepare inputs
            logd1 <- .Call("cdclogis", as.numeric(y), as.numeric(theta$mu1), as.numeric(exp(theta$logsd1)),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
            logd2 <- .Call("cdclogis", as.numeric(y), as.numeric(theta$mu2), as.numeric(exp(theta$logsd2)),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
            # Calculate log-likelihood
            ll <- data.frame(
                    component   = sum((1 - post) * logd1) + sum(post * logd2),
                    concomitant = sum((1 - post) * log(1 - prob) + post * log(prob))
                  )
            ll$full <- sum(unlist(ll))
            return(ll)
        },
        # Calculate posterior:
        # Ratio between weighted density of component 2 divided by the sum
        # of the weighted density of both components gives the posterior.
        posterior = function(y, prob, theta) {
            # Calculate (possibly censored) densities
            d1 <- .Call("cdclogis", as.numeric(y), as.numeric(theta$mu1), as.numeric(exp(theta$logsd1)),
                        as.numeric(left), as.numeric(right), FALSE, package = "foehnix")
            d2 <- .Call("cdclogis", as.numeric(y), as.numeric(theta$mu2), as.numeric(exp(theta$logsd2)),
                        as.numeric(left), as.numeric(right), FALSE, package = "foehnix") 
            # Calculate a-posteriori probabilities
            prob * d2 / ((1 - prob) * d1 + prob * d2)
        },
        # Update the parameters of the two components.
        # Returns a list containing mu1, logsd1, mu2, and logsd2.
        theta = function(y, post, init = FALSE, theta = NULL) {
            # non-censored estimate
            if ( is.null(theta) ) {
                theta <- list()
                theta$mu1 <- sum(y * (1 - post)) / sum(1 - post)
                theta$mu2 <- sum(y * post) / sum(post)
                if ( init ) {
                    theta$logsd1 <- theta$logsd2 <- log(sd(y))
                } else {
                    theta$logsd1 <- log(sqrt(sum((y-mu1)^2 * (1-post)) / sum(1-post)))
                    theta$logsd2 <- log(sqrt(sum((y-mu2)^2 * (post)) / sum(post)))
                }
            }

            fun <- function(par, y, post, left, right) {
                d1 <- .Call("cdclogis", as.numeric(y), as.numeric(par[1L]), as.numeric(exp(par[2L])),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
                mu2 <- exp(par[3L]) + par[1L]
                d2 <- .Call("cdclogis", as.numeric(y), as.numeric(mu2), as.numeric(exp(par[4L])),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
                return(sum((1-post) * d1 + d2 * post))
            }
            par <- c(theta$mu1, theta$logsd1, log(theta$mu2 - theta$mu1), theta$logsd2)
            opt <- optim(par, fun, y = y, post = post,
                        left = left, right = right, method = "BFGS",
                        control = list(fnscale = -1))

            rval <- list(mu1 = opt$par[1L], logsd1 = opt$par[2L],
                 mu2 = exp(opt$par[3L]) + opt$par[1L], logsd2 = opt$par[4L])
            #cat(sprintf("Current theta            %.2f %.2f %.2f %.2f\n", rval$mu1,
            #            exp(rval$logsd1), rval$mu2, exp(rval$logsd2)))
            return(rval)
        }
    )
    class(rval) <- c("foehnix.family")
    return(rval)
}


# -------------------------------------------------------------------
# Truncated Gaussian distribution family
# -------------------------------------------------------------------
foehnix_tlogistic <- function(left = -Inf, right = Inf) {
    if ( ! length(left) == 1 | ! length(right) == 1 )
        stop("Input left/right have to be numeric values of length 1!")
    rval <- list(
        name = "truncated logistic", left = left, right = right, truncated = TRUE,
        # Density function
        d = function(y, mu, sigma, log = FALSE) {
            .Call("cdtlogis", as.numeric(y), as.numeric(mu),
                  as.numeric(sigma), as.numeric(left), as.numeric(right),
                  log, package = "foehnix")
        },
        # Distribution function 
        p = function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE) {
            .Call("cptlogis", as.numeric(q), as.numeric(mu), as.numeric(sigma), 
                  as.numeric(left), as.numeric(right), lower.tail, log.p,
                  package = "foehnix")
        },
        # Random sample function for two-component truncated logistic distribution
        r = function(n, mu, sigma, lower.tail = TRUE) {
            stopifnot(length(mu) == 2)
            stopifnot(length(sigma) == 2)
            if ( length(n) == 1 ) n <- c(floor(n/2), ceiling(n/2))
            lower <- if ( lower.tail ) left else right
            upper <- if ( lower.tail ) right else left
            p1 <- runif(n[1L]); p2 <- runif(n[2L])
            p1 <- (1 - p1) * plogis((lower - mu[1L]) / sigma[1L], lower.tail = lower.tail) +
                  p1       * plogis((upper - mu[1L]) / sigma[1L], lower.tail = lower.tail)
            p2 <- (1 - p2) * plogis((lower - mu[2L]) / sigma[2L], lower.tail = lower.tail) +
                  p2       * plogis((upper - mu[2L]) / sigma[2L], lower.tail = lower.tail)
            c(qlogis(p1, lower.tail = lower.tail) * sigma[1L] + mu[1L],
              qlogis(p2, lower.tail = lower.tail) * sigma[2L] + mu[2L])
        },
        # Calculate log-likelihood sum of the two-component mixture model
        loglik = function(y, post, prob, theta) {
            # Avoid 0/1 due to computational reasons
            eps  <- sqrt(.Machine$double.eps)
            prob <- pmax(eps, pmin(1-eps, prob))
            post <- pmax(eps, pmin(1-eps, post))
            # Prepare inputs
            logd1 <- .Call("cdtlogis", as.numeric(y), as.numeric(theta$mu1), as.numeric(exp(theta$logsd1)),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
            logd2 <- .Call("cdtlogis", as.numeric(y), as.numeric(theta$mu2), as.numeric(exp(theta$logsd2)),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
            # Calculate log-likelihood
            ll <- data.frame(
                    component   = sum((1 - post) * logd1) + sum(post * logd2),
                    concomitant = sum((1 - post) * log(1 - prob) + post * log(prob))
                  )
            ll$full <- sum(unlist(ll))
            return(ll)
        },
        # Calculate posterior:
        # Ratio between weighted density of component 2 divided by the sum
        # of the weighted density of both components gives the posterior.
        posterior = function(y, prob, theta) {
            # Calculate (possibly censored) densities
            d1 <- .Call("cdtlogis", as.numeric(y), as.numeric(theta$mu1), as.numeric(exp(theta$logsd1)),
                        as.numeric(left), as.numeric(right), FALSE, package = "foehnix")
            d2 <- .Call("cdtlogis", as.numeric(y), as.numeric(theta$mu2), as.numeric(exp(theta$logsd2)),
                        as.numeric(left), as.numeric(right), FALSE, package = "foehnix") 
            # Calculate a-posteriori probabilities
            prob * d2 / ((1 - prob) * d1 + prob * d2)
        },
        # Update the parameters of the two components.
        # Returns a list containing mu1, logsd1, mu2, and logsd2.
        theta = function(y, post, init = FALSE, theta = NULL) {
            # non-censored estimate
            if ( is.null(theta) ) {
                theta <- list()
                theta$mu1 <- sum(y * (1 - post)) / sum(1 - post)
                theta$mu2 <- sum(y * post) / sum(post)
                if ( init ) {
                    theta$logsd1 <- theta$logsd2 <- log(sd(y))
                } else {
                    theta$logsd1 <- log(sqrt(sum((y-mu1)^2 * (1-post)) / sum(1-post)))
                    theta$logsd2 <- log(sqrt(sum((y-mu2)^2 * (post)) / sum(post)))
                }
            }

            fun <- function(par, y, post, left, right) {
                d1 <- .Call("cdtlogis", as.numeric(y), as.numeric(par[1L]), as.numeric(exp(par[2L])),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
                mu2 <- exp(par[3L]) + par[1L]
                d2 <- .Call("cdtlogis", as.numeric(y), as.numeric(mu2), as.numeric(exp(par[4L])),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
                return(sum((1-post) * d1 + d2 * post))
            }
            par <- c(theta$mu1, theta$logsd1, log(theta$mu2 - theta$mu1), theta$logsd2)
            opt <- optim(par, fun, y = y, post = post,
                        left = left, right = right, method = "BFGS",
                        control = list(fnscale = -1))

            rval <- list(mu1 = opt$par[1L], logsd1 = opt$par[2L],
                 mu2 = exp(opt$par[3L]) + opt$par[1L], logsd2 = opt$par[4L])
            #cat(sprintf("Current theta            %.2f %.2f %.2f %.2f\n", rval$mu1,
            #            exp(rval$logsd1), rval$mu2, exp(rval$logsd2)))
            return(rval)
        }
    )
    class(rval) <- c("foehnix.family")
    return(rval)
}


# -------------------------------------------------------------------
# Gaussian distribution family
# -------------------------------------------------------------------
foehnix_gaussian <- function() {
    rval <- list(
        name = "Gaussian",
        # Density function
        d = function(y, mu, sigma, log = FALSE)
            dnorm(y, mu, sigma, log = log),
        # Distribution function 
        p = function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE)
            pnorm(q, mu, sigma, lower.tail, log.p),
        # Calculate log-likelihood sum of the two-component mixture model
        loglik = function(y, post, prob, theta) {
            # Calculate/trace loglik
            eps  <- sqrt(.Machine$double.eps)
            prob <- pmax(eps, pmin(1-eps, prob))
            # plog function and eps are used to void log(0)
            plog <- function(x, eps) ifelse(x < eps, log(eps), log(x))
            eps <- sqrt(.Machine$double.eps)
            # Calculate log-likelihood
            ll <- data.frame(
                    component = sum(post       * dnorm(y, theta$mu2, exp(theta$logsd2), log = TRUE))
                              + sum((1 - post) * dnorm(y, theta$mu1, exp(theta$logsd1), log = TRUE)),
                    concomitant = sum((1 - post) * plog(1 - prob, eps) + post * plog(prob, eps))
                  )
            ll$full <- sum(unlist(ll))
            return(ll)
        },
        # Random sample function for two-component Gaussian distribution
        r = function(n, mu, sigma, lower.tail = TRUE) {
            stopifnot(length(mu) == 2)
            stopifnot(length(sigma) == 2)
            if ( length(n) == 1 ) n <- c(floor(n/2), ceiling(n/2))
            c(rnorm(n[1L], mu[1L], sigma[1L]), rnorm(n[2L], mu[2L], sigma[2L]))
        },
        # Calculate posterior:
        # Ratio between weighted density of component 2 divided by the sum
        # of the weighted density of both components gives the posterior.
        posterior = function(y, prob, theta) {
            # Calculate posterior:
            # Ratio between weighted density of component 2 divided by the sum
            # of the weighted density of both components gives the posterior.
            (prob) * dnorm(y, theta$mu2, exp(theta$logsd2)) /
            ( (1 - prob) * dnorm(y, theta$mu1, exp(theta$logsd1)) +
               prob * dnorm(y, theta$mu2, exp(theta$logsd2)) )
        },
        # Update the parameters of the two components.
        # Returns a list containing mu1, logsd1, mu2, and logsd2.
        theta = function(y, post, init = FALSE, ...) {
            mu1 <- sum(y * (1 - post)) / sum(1 - post)
            mu2 <- sum(y * post) / sum(post)
            if ( init ) {
                sigma1 <- sigma2 <- sd(y)
            } else {
                sigma1 <- sqrt(sum((y-mu1)^2 * (1-post)) / sum(1-post))
                sigma2 <- sqrt(sum((y-mu2)^2 * (post)) / sum(post))
            }
            list(mu1    = mu1, 
                 logsd1 = ifelse(sigma1 < exp(-6), -6, log(sigma1)),
                 mu2    = mu2,
                 logsd2 = ifelse(sigma2 < exp(-6), -6, log(sigma2)))
        }
    )
    class(rval) <- c("foehnix.family")
    return(rval)
}


# -------------------------------------------------------------------
# Censored Gaussian distribution family
# -------------------------------------------------------------------
foehnix_cgaussian <- function(left = -Inf, right = Inf) {
    if ( ! length(left) == 1 | ! length(right) == 1 )
        stop("Input left/right have to be numeric values of length 1!")
    rval <- list(
        name = "censored Gaussian", left = left, right = right, censored = TRUE,
        # Density function
        d = function(y, mu, sigma, log = FALSE) {
            .Call("cdcnorm", as.numeric(y), as.numeric(mu),
                  as.numeric(sigma), as.numeric(left), as.numeric(right),
                  log, package = "foehnix")
        },
        # Distribution function 
        p = function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE) {
            .Call("cpcnorm", as.numeric(q), as.numeric(mu), as.numeric(sigma), 
                  as.numeric(left), as.numeric(right), lower.tail, log.p,
                  package = "foehnix")
        },
        # Random sample function for two-component censored Gaussian distribution
        r = function(n, mu, sigma, lower.tail = TRUE) {
            stopifnot(length(mu) == 2)
            stopifnot(length(sigma) == 2)
            if ( length(n) == 1 ) n <- c(floor(n/2), ceiling(n/2))
            lower <- if ( lower.tail ) left else right
            upper <- if ( lower.tail ) right else left
            p1 <- rnorm(n[1L], mu[1L], sigma[1L])
            p2 <- rnorm(n[2L], mu[2L], sigma[2L])
            pmax(left, pmin(right, c(p1, p2)))
        },
        # Calculate log-likelihood sum of the two-component mixture model
        loglik = function(y, post, prob, theta) {
            # Avoid 0/1 due to computational reasons
            eps  <- sqrt(.Machine$double.eps)
            prob <- pmax(eps, pmin(1-eps, prob))
            post <- pmax(eps, pmin(1-eps, post))
            # Prepare inputs
            logd1 <- .Call("cdcnorm", as.numeric(y), as.numeric(theta$mu1), as.numeric(exp(theta$logsd1)),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
            logd2 <- .Call("cdcnorm", as.numeric(y), as.numeric(theta$mu2), as.numeric(exp(theta$logsd2)),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
            # Calculate log-likelihood
            ll <- data.frame(
                    component   = sum((1 - post) * logd1) + sum(post * logd2),
                    concomitant = sum((1 - post) * log(1 - prob) + post * log(prob))
                  )
            ll$full <- sum(unlist(ll))
            return(ll)
        },
        # Calculate posterior:
        # Ratio between weighted density of component 2 divided by the sum
        # of the weighted density of both components gives the posterior.
        posterior = function(y, prob, theta) {
            # Calculate (possibly censored) densities
            d1 <- .Call("cdcnorm", as.numeric(y), as.numeric(theta$mu1), as.numeric(exp(theta$logsd1)),
                        as.numeric(left), as.numeric(right), FALSE, package = "foehnix")
            d2 <- .Call("cdcnorm", as.numeric(y), as.numeric(theta$mu2), as.numeric(exp(theta$logsd2)),
                        as.numeric(left), as.numeric(right), FALSE, package = "foehnix") 
            # Calculate a-posteriori probabilities
            prob * d2 / ((1 - prob) * d1 + prob * d2)
        },
        # Update the parameters of the two components.
        # Returns a list containing mu1, logsd1, mu2, and logsd2.
        theta = function(y, post, init = FALSE, theta = NULL) {
            # non-censored estimate
            if ( is.null(theta) ) {
                theta <- list()
                theta$mu1 <- sum(y * (1 - post)) / sum(1 - post)
                theta$mu2 <- sum(y * post) / sum(post)
                if ( init ) {
                    theta$logsd1 <- theta$logsd2 <- log(sd(y))
                } else {
                    theta$logsd1 <- log(sqrt(sum((y-mu1)^2 * (1-post)) / sum(1-post)))
                    theta$logsd2 <- log(sqrt(sum((y-mu2)^2 * (post)) / sum(post)))
                }
            }

            fun <- function(par, y, post, left, right) {
                d1 <- .Call("cdcnorm", as.numeric(y), as.numeric(par[1L]), as.numeric(exp(par[2L])),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
                mu2 <- exp(par[3L]) + par[1L]
                d2 <- .Call("cdcnorm", as.numeric(y), as.numeric(mu2), as.numeric(exp(par[4L])),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
                return(sum((1-post) * d1 + d2 * post))
            }
            par <- c(theta$mu1, theta$logsd1, log(theta$mu2 - theta$mu1), theta$logsd2)
            opt <- optim(par, fun, y = y, post = post,
                        left = left, right = right, method = "BFGS",
                        control = list(fnscale = -1))

            rval <- list(mu1 = opt$par[1L], logsd1 = opt$par[2L],
                 mu2 = exp(opt$par[3L]) + opt$par[1L], logsd2 = opt$par[4L])
            #cat(sprintf("Current theta            %.2f %.2f %.2f %.2f\n", rval$mu1,
            #            exp(rval$logsd1), rval$mu2, exp(rval$logsd2)))
            return(rval)
        }
    )
    class(rval) <- c("foehnix.family")
    return(rval)
}


# -------------------------------------------------------------------
# Truncated Gaussian distribution family
# -------------------------------------------------------------------
foehnix_tgaussian <- function(left = -Inf, right = Inf) {
    if ( ! length(left) == 1 | ! length(right) == 1 )
        stop("Input left/right have to be numeric values of length 1!")
    rval <- list(
        name = "truncated Gaussian", left = left, right = right, truncated = TRUE,
        # Density function
        d = function(y, mu, sigma, log = FALSE) {
            .Call("cdtnorm", as.numeric(y), as.numeric(mu),
                  as.numeric(sigma), as.numeric(left), as.numeric(right),
                  log, package = "foehnix")
        },
        # Distribution function 
        p = function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE) {
            .Call("cptnorm", as.numeric(q), as.numeric(mu), as.numeric(sigma), 
                  as.numeric(left), as.numeric(right), lower.tail, log.p,
                  package = "foehnix")
        },
        # Random sample function for two-component truncated Gaussian distribution
        r = function(n, mu, sigma, lower.tail = TRUE) {
            stopifnot(length(mu) == 2)
            stopifnot(length(sigma) == 2)
            if ( length(n) == 1 ) n <- c(floor(n/2), ceiling(n/2))
            lower <- if ( lower.tail ) left else right
            upper <- if ( lower.tail ) right else left
            p1 <- runif(n[1L]); p2 <- runif(n[2L])
            p1 <- (1 - p1) * pnorm((lower - mu[1L]) / sigma[1L], lower.tail = lower.tail) +
                  p1       * pnorm((upper - mu[1L]) / sigma[1L], lower.tail = lower.tail)
            p2 <- (1 - p2) * pnorm((lower - mu[2L]) / sigma[2L], lower.tail = lower.tail) +
                  p2       * pnorm((upper - mu[2L]) / sigma[2L], lower.tail = lower.tail)
            c(qnorm(p1, lower.tail = lower.tail) * sigma[1L] + mu[1L],
              qnorm(p2, lower.tail = lower.tail) * sigma[2L] + mu[2L])
        },
        # Calculate log-likelihood sum of the two-component mixture model
        loglik = function(y, post, prob, theta) {
            # Avoid 0/1 due to computational reasons
            eps  <- sqrt(.Machine$double.eps)
            prob <- pmax(eps, pmin(1-eps, prob))
            post <- pmax(eps, pmin(1-eps, post))
            # Prepare inputs
            logd1 <- .Call("cdtnorm", as.numeric(y), as.numeric(theta$mu1), as.numeric(exp(theta$logsd1)),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
            logd2 <- .Call("cdtnorm", as.numeric(y), as.numeric(theta$mu2), as.numeric(exp(theta$logsd2)),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
            # Calculate log-likelihood
            ll <- data.frame(
                    component   = sum((1 - post) * logd1) + sum(post * logd2),
                    concomitant = sum((1 - post) * log(1 - prob) + post * log(prob))
                  )
            ll$full <- sum(unlist(ll))
            return(ll)
        },
        # Calculate posterior:
        # Ratio between weighted density of component 2 divided by the sum
        # of the weighted density of both components gives the posterior.
        posterior = function(y, prob, theta) {
            # Calculate (possibly censored) densities
            d1 <- .Call("cdtnorm", as.numeric(y), as.numeric(theta$mu1), as.numeric(exp(theta$logsd1)),
                        as.numeric(left), as.numeric(right), FALSE, package = "foehnix")
            d2 <- .Call("cdtnorm", as.numeric(y), as.numeric(theta$mu2), as.numeric(exp(theta$logsd2)),
                        as.numeric(left), as.numeric(right), FALSE, package = "foehnix") 
            # Calculate a-posteriori probabilities
            prob * d2 / ((1 - prob) * d1 + prob * d2)
        },
        # Update the parameters of the two components.
        # Returns a list containing mu1, logsd1, mu2, and logsd2.
        theta = function(y, post, init = FALSE, theta = NULL) {
            # non-censored estimate
            if ( is.null(theta) ) {
                theta <- list()
                theta$mu1 <- sum(y * (1 - post)) / sum(1 - post)
                theta$mu2 <- sum(y * post) / sum(post)
                if ( init ) {
                    theta$logsd1 <- theta$logsd2 <- log(sd(y))
                } else {
                    theta$logsd1 <- log(sqrt(sum((y-mu1)^2 * (1-post)) / sum(1-post)))
                    theta$logsd2 <- log(sqrt(sum((y-mu2)^2 * (post)) / sum(post)))
                }
            }

            fun <- function(par, y, post, left, right) {
                d1 <- .Call("cdtnorm", as.numeric(y), as.numeric(par[1L]), as.numeric(exp(par[2L])),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
                mu2 <- exp(par[3L]) + par[1L]
                d2 <- .Call("cdtnorm", as.numeric(y), as.numeric(mu2), as.numeric(exp(par[4L])),
                           as.numeric(left), as.numeric(right), TRUE, package = "foehnix")
                return(sum((1-post) * d1 + d2 * post))
            }
            par <- c(theta$mu1, theta$logsd1, log(theta$mu2 - theta$mu1), theta$logsd2)
            opt <- optim(par, fun, y = y, post = post,
                        left = left, right = right, method = "BFGS",
                        control = list(fnscale = -1))

            rval <- list(mu1 = opt$par[1L], logsd1 = opt$par[2L],
                 mu2 = exp(opt$par[3L]) + opt$par[1L], logsd2 = opt$par[4L])
            #cat(sprintf("Current theta            %.2f %.2f %.2f %.2f\n", rval$mu1,
            #            exp(rval$logsd1), rval$mu2, exp(rval$logsd2)))
            return(rval)
        }
    )
    class(rval) <- c("foehnix.family")
    return(rval)
}