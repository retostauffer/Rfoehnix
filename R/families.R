


foehnix_gaussian <- function() {
    list(
        d = function(y, mu, sigma, log = FALSE, ...) dnorm(y, mu, sigma, log = FALSE, ...),
        p = function(y, mu, sigma, ...)              pnorm(y, mu, sigma, ...),
        ##mu = function(x, ...) mean(x, ...),
        ##sigma   = function(x, log = FALSE, ...) {
        ##    sd <- sd(x, ...)
        ##    if ( log ) ifelse(sd == 0, 0, log(sd))
        ##    return(max(sd, sqrt(.Machine$double.eps)))
        ##},
        loglik = function(y, post, prob, theta) {
            # Calculate/trace loglik
            eps  <- sqrt(.Machine$double.eps)
            prob <- pmax(eps, pmin(1-eps, prob))
            ll <- data.frame(
                     component = sum(post       * dnorm(y, theta$mu2, exp(theta$logsd2), log = TRUE))
                               + sum((1 - post) * dnorm(y, theta$mu1, exp(theta$logsd1), log = TRUE)),
                     concomitant = sum((1 - post) * log(1 - prob) + post * log(prob))
                  )
            sum(unlist(ll))
        },
        posterior = function(y, prob, theta) {
            # Calculate posterior:
            # Ratio between weighted density of component 2 divided by the sum
            # of the weighted density of both components gives the posterior.
            (prob) * dnorm(y, theta$mu2, exp(theta$logsd2)) /
            ( (1 - prob) * dnorm(y, theta$mu1, exp(theta$logsd1)) +
               prob * dnorm(y, theta$mu2, exp(theta$logsd2)) )
        },
        theta = function(y, prob) {
            mu1 <- sum(y * (1 - prob)) / sum(1 - prob)
            mu2 <- sum(y * prob) / sum(prob)
            sigma1 <- sqrt(sum((y-mu1)^2 * (1-prob)) / sum(1-prob))
            sigma2 <- sqrt(sum((y-mu2)^2 * (prob)) / sum(prob))
            list(mu1    = mu1, 
                 logsd1 = ifelse(sigma1 < exp(-6), -6, log(sigma1)),
                 mu2    = mu2,
                 logsd2 = ifelse(sigma2 < exp(-6), -6, log(sigma2)))
        }
    )
}

foehnix_logistic <- function() {
    list(
        d = function(y, mu, sigma, log = FALSE, ...) dlogis(y, mu, sigma, log = FALSE, ...),
        p = function(y, mu, sigma, ...)              plogis(y, mu, sigma, ...),
        ##mu = function(x, ...) mean(x, ...),
        ##sigma   = function(x, log = FALSE, ...) {
        ##    sd <- sd(x, ...) * sqrt(3) / pi
        ##    if ( log ) ifelse(sd == 0, 0, log(sd))
        ##    return(max(sd, sqrt(.Machine$double.eps)))
        ##},
        loglik = function(y, post, prob, theta) {
            # Calculate/trace loglik
            eps  <- sqrt(.Machine$double.eps)
            prob <- pmax(eps, pmin(1-eps, prob))
            ll <- data.frame(
                    component = sum(post       * dlogis(y, theta$mu2, exp(theta$logsd2), log = TRUE))
                              + sum((1 - post) * dlogis(y, theta$mu1, exp(theta$logsd1), log = TRUE)),
                    concomitant = sum((1 - post) * log(1 - prob) + post * log(prob))
                  )
            sum(unlist(ll))
        },
        posterior = function(y, prob, theta) {
            # Calculate posterior:
            # Ratio between weighted density of component 2 divided by the sum
            # of the weighted density of both components gives the posterior.
            (prob) * dlogis(y, theta$mu2, exp(theta$logsd2)) /
            ((1 - prob) * dlogis(y, theta$mu1, exp(theta$logsd1)) +
            prob * dlogis(y, theta$mu2, exp(theta$logsd2)))
        },
        theta = function(y, prob) {
            mu1 <- sum(y * (1 - prob)) / sum(1 - prob)
            mu2 <- sum(y * prob) / sum(prob)
            sigma1 <- sqrt(sum((y-mu1)^2 * (1-prob)) / sum(1-prob)) * sqrt(3) / pi
            sigma2 <- sqrt(sum((y-mu2)^2 * (prob)) / sum(prob)) * sqrt(3) / pi
            list(mu1    = mu1, 
                 logsd1 = ifelse(sigma1 < exp(-6), -6, log(sigma1)),
                 mu2    = mu2,
                 logsd2 = ifelse(sigma2 < exp(-6), -6, log(sigma2)))
        }
    )
}

