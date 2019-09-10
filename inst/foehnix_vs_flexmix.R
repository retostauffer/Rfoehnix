

rm(list = ls())

library("foehnix")

# Load Tyrolean demo data set
data <- demodata()
filter <- list(dd = c(43, 223), crest_dd = c(90, 270))
formula <- "diff_t ~ rh + ff"
switch = TRUE

estimate_flexmix <- function(formula, data, switch = FALSE, filter = NULL) {

    variables  <- rownames(attr(terms(formula(formula)), "factors"))
    filter_obj <- foehnix_filter(data, filter, variables)
    # Model frame
    mf <- data[filter_obj$good, ]

    # Initial cluster
    initial.cluster <- as.numeric(mf[, variables[1L]] > 
                                  mean(mf[, variables[1L]])) + 1

    # Formula for flexmix model call
    f_main <- formula(paste(variables[1L], "1", sep = "~"))
    if (length(variables) > 1) {
        f_conc <- formula(paste("~", paste(variables[-1L], collapse = "+")))
    } else {
        f_conc <- NULL
    }

    require("flexmix")
    mycont <- new("FLXcontrol"); mycont@minprior = 0.01
    M  = flexmix(f_main, data = mf,
                 control = mycont, 
                 concomitant = FLXPmultinom(f_conc),
                 k = 2, cluster = initial.cluster,
                 model = FLXglm(family = "gaussian"))

    # Parameters of the two clusters
    param   <- parameters(M)
    if (!switch) {
        foehn.idx <- which.max(param[1L, ])
        param <- structure(param[, order(param[1L, ])], col.names = c("nofoehn", "foehn"))
    } else {
        foehn.idx <- which.min(param[1L, ])
        param <- structure(param[, order(param[1L, ], decreasing = TRUE)], col.names = c("nofoehn", "foehn"))
    }

    # Pick estimated posterior probabilities
    post <- posterior(M)[, foehn.idx]

    # Create final zoo object
    res <- zoo(NA, index(data))
    res[filter_obj$bad] <- 0
    res[filter_obj$good] <- post

    # Inflate
    index_infl <- seq(min(index(res)), max(index(res)), by = deltat(res))
    res <- merge(res, zoo(, index_infl))
    return(list(model = M, result = res, param = param))
}


# Estimate both models
mod_flexmix <- estimate_flexmix(formula, data, switch = switch, filter = filter)
mod_foehnix <- foehnix(formula, data, switch = switch, filter = filter)

# Coefficients
parameters(mod_flexmix$model)
mod_flexmix$model@concomitant@coef
coef(mod_foehnix)


# Log-likelihood
print(c(foehnix = logLik(mod_foehnix), flexmix = mod_flexmix$model@logLik))

# Time series plot
tsplot(list(foehnix = mod_foehnix, flexmix = mod_flexmix$result),
       start = "2017-01-02", end = "2017-03-01")

probs <- merge(fitted(mod_foehnix), mod_flexmix$result)


