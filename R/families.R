# -------------------------------------------------------------------
# - NAME:        families.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-13
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-13, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-13 17:11 on marvin
# -------------------------------------------------------------------



# -------------------------------------------------------------------
# Logistic distribution family
# -------------------------------------------------------------------
foehnix_logistic <- function() {
    list(
        # Density function
        d = function(y, mu, sigma, log = FALSE)
            dlogis(y, mu, sigma, log = log),
        # Distribution function 
        p = function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE)
            plogis(q, mu, sigma, lower.tail, log.p),
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
        theta = function(y, prob, init = FALSE) {
            mu1 <- sum(y * (1 - prob)) / sum(1 - prob)
            mu2 <- sum(y * prob) / sum(prob)
            if ( init ) {
                sigma1 <- sigma2 <- sd(y) * sqrt(3) / pi
            } else {
                sigma1 <- sqrt(sum((y-mu1)^2 * (1-prob)) / sum(1-prob)) * sqrt(3) / pi
                sigma2 <- sqrt(sum((y-mu2)^2 * (prob)) / sum(prob)) * sqrt(3) / pi
            }
            list(mu1    = mu1, 
                 logsd1 = ifelse(sigma1 < exp(-6), -6, log(sigma1)),
                 mu2    = mu2,
                 logsd2 = ifelse(sigma2 < exp(-6), -6, log(sigma2)))
        }
    )
}


# -------------------------------------------------------------------
# Censored logistic distribution family
# -------------------------------------------------------------------
foehnix_clogistic <- function(left = -Inf, right = Inf) {
    if ( ! length(left) == 1 | ! length(right) == 1 )
        stop("Input left/right have to be numeric values of length 1!")
    list(
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
        theta = function(y, prob, init = FALSE) {
            mu1 <- sum(y * (1 - prob)) / sum(1 - prob)
            mu2 <- sum(y * prob) / sum(prob)
            if ( init ) {
                sigma1 <- sigma2 <- sd(y)
            } else {
                sigma1 <- sqrt(sum((y-mu1)^2 * (1-prob)) / sum(1-prob))
                sigma2 <- sqrt(sum((y-mu2)^2 * (prob)) / sum(prob))
            }
            list(mu1    = mu1, 
                 logsd1 = ifelse(sigma1 < exp(-6), -6, log(sigma1)),
                 mu2    = mu2,
                 logsd2 = ifelse(sigma2 < exp(-6), -6, log(sigma2)))
        }
    )
}


# -------------------------------------------------------------------
# Truncated Gaussian distribution family
# -------------------------------------------------------------------
foehnix_tlogistic <- function(left = -Inf, right = Inf) {
    if ( ! length(left) == 1 | ! length(right) == 1 )
        stop("Input left/right have to be numeric values of length 1!")
    list(
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
        theta = function(y, prob, init = FALSE) {
            mu1 <- sum(y * (1 - prob)) / sum(1 - prob)
            mu2 <- sum(y * prob) / sum(prob)
            if ( init ) {
                sigma1 <- sigma2 <- sd(y)
            } else {
                sigma1 <- sqrt(sum((y-mu1)^2 * (1-prob)) / sum(1-prob))
                sigma2 <- sqrt(sum((y-mu2)^2 * (prob)) / sum(prob))
            }
            list(mu1    = mu1, 
                 logsd1 = ifelse(sigma1 < exp(-6), -6, log(sigma1)),
                 mu2    = mu2,
                 logsd2 = ifelse(sigma2 < exp(-6), -6, log(sigma2)))
        }
    )
}


# -------------------------------------------------------------------
# Gaussian distribution family
# -------------------------------------------------------------------
foehnix_gaussian <- function() {
    list(
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
        theta = function(y, prob, init = FALSE) {
            mu1 <- sum(y * (1 - prob)) / sum(1 - prob)
            mu2 <- sum(y * prob) / sum(prob)
            if ( init ) {
                sigma1 <- sigma2 <- sd(y)
            } else {
                sigma1 <- sqrt(sum((y-mu1)^2 * (1-prob)) / sum(1-prob))
                sigma2 <- sqrt(sum((y-mu2)^2 * (prob)) / sum(prob))
            }
            list(mu1    = mu1, 
                 logsd1 = ifelse(sigma1 < exp(-6), -6, log(sigma1)),
                 mu2    = mu2,
                 logsd2 = ifelse(sigma2 < exp(-6), -6, log(sigma2)))
        }
    )
}


# -------------------------------------------------------------------
# Censored Gaussian distribution family
# -------------------------------------------------------------------
foehnix_cgaussian <- function(left = -Inf, right = Inf) {
    if ( ! length(left) == 1 | ! length(right) == 1 )
        stop("Input left/right have to be numeric values of length 1!")
    list(
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
        theta = function(y, prob, init = FALSE) {
            mu1 <- sum(y * (1 - prob)) / sum(1 - prob)
            mu2 <- sum(y * prob) / sum(prob)
            if ( init ) {
                sigma1 <- sigma2 <- sd(y)
            } else {
                sigma1 <- sqrt(sum((y-mu1)^2 * (1-prob)) / sum(1-prob))
                sigma2 <- sqrt(sum((y-mu2)^2 * (prob)) / sum(prob))
            }
            list(mu1    = mu1, 
                 logsd1 = ifelse(sigma1 < exp(-6), -6, log(sigma1)),
                 mu2    = mu2,
                 logsd2 = ifelse(sigma2 < exp(-6), -6, log(sigma2)))
        }
    )
}


# -------------------------------------------------------------------
# Truncated Gaussian distribution family
# -------------------------------------------------------------------
foehnix_tgaussian <- function(left = -Inf, right = Inf) {
    if ( ! length(left) == 1 | ! length(right) == 1 )
        stop("Input left/right have to be numeric values of length 1!")
    list(
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
        theta = function(y, prob, init = FALSE) {
            mu1 <- sum(y * (1 - prob)) / sum(1 - prob)
            mu2 <- sum(y * prob) / sum(prob)
            if ( init ) {
                sigma1 <- sigma2 <- sd(y)
            } else {
                sigma1 <- sqrt(sum((y-mu1)^2 * (1-prob)) / sum(1-prob))
                sigma2 <- sqrt(sum((y-mu2)^2 * (prob)) / sum(prob))
            }
            list(mu1    = mu1, 
                 logsd1 = ifelse(sigma1 < exp(-6), -6, log(sigma1)),
                 mu2    = mu2,
                 logsd2 = ifelse(sigma2 < exp(-6), -6, log(sigma2)))
        }
    )
}
