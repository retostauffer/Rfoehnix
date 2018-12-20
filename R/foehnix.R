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
# - L@ST MODIFIED: 2018-12-20 11:32 on marvin
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
foehnix.noconcomitant.fit <- function(y, family,
                    maxit = 100L, tol = 1e-5, verbose = TRUE, ...) {

    # Lists to trace log-likelihood path and the development of
    # the coefficients during EM optimization.
    llpath   <- list()
    coefpath <- list()

    # Given the initial probabilities: calculate parameters
    # for the two components (mu1, logsd1, mu2, logsd2) given
    # the selected family and calculate the a-posteriori probabilities.
    z     <- as.numeric(y >= mean(y))
    theta <- family$theta(y, z, init = TRUE) # M-step

    # Initial probability (fifty fifty) and inital prior
    # probabilites for the component membership.
    prob  <- rep(.5, length(y))
    post  <- family$posterior(y, mean(prob), theta)

    # EM algorithm: estimate probabilities (prob; E-step), update the model
    # given the new probabilities (M-step). Always with respect to the
    # selected family.
    iter <- 0
    while ( iter < maxit[1L] ) {
        iter <- iter + 1;

        # M-step: update probabilites and theta
        #prob  <- as.numeric(post >= .5)
        ##TODO: prob <- post
        #prob <- post
        prob <- mean(post)
        theta <- family$theta(y, post, theta = theta)

        # E-step: calculate a-posteriori probability
        post  <- family$posterior(y, mean(prob), theta)

        # Store log-likelihood and coefficients of the current
        # iteration.
        llpath[[iter]] <- family$loglik(y, post, prob, theta)
        coefpath[[iter]] <- as.data.frame(theta)
        if ( verbose )
            cat(sprintf("EM iteration %d/%d, ll = %10.2f\r", iter, maxit[1L], llpath[[iter]]))

        # If the log-likelihood decreases: proceed!
        if ( iter == 1 ) next

        # Improvement < 0 (model got worse): continue
        ##if ( (llpath[[iter]]$full - llpath[[iter - 1]]$full) < 0 ) next

        # If the log-likelihood improvement falls below the
        # specified tolerance we assume that the algorithm
        # converged: stop iteration.
        if( is.na(llpath[[iter]]$full)) {
            print("Likelihood got NA! Start environment browser for debugging ...")
            browser()
        }

        if ( (llpath[[iter]]$full - llpath[[iter - 1]]$full) < tol[1L] ) break
    }; if ( verbose ) cat("\n")

    # Check if algorithm converged before maxit was reached
    converged <- ifelse(iter < maxit[1L], TRUE, FALSE)

    # Combine to data.frame (number of rows corresponds to iter + 1)
    llpath    <- do.call(rbind, llpath)
    coefpath  <- do.call(rbind, coefpath)

    # In this case we have no concomitants, however, the log-likelihood
    # contribution of the concomitants is not == 0 (as log(0) is set to
    # log(sqrt(.Machine$double.eps)) to avoid -Inf). Fix this here.
    #llpath$concomitant <- 0; llpath$full <- llpath$component

    # Return a list with results
if ( inherits(y, "binned") ) stop("Stop, requires changes on computation of BIC!")
    ll   <- tail(llpath$full, 1)
    rval <- list(prob       = prob,
                 post       = post,
                 theta      = theta,
                 loglik     = ll,
                 edf        = ncol(coefpath),
                 AIC        = - 2 * ll + 2 * ncol(coefpath),
                 BIC        = - 2 * ll + log(length(y)) * ncol(coefpath),
                 ccmodel    = NULL,
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
foehnix.unreg.fit <- function(y, logitX, family,
                    maxit = 100L, tol = 1e-5, verbose = TRUE,
                    alpha = NULL, ...) {

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
    # Force standardize = FALSE. If required logitX has alreday been
    # standardized in the parent function (foehnix).
    ccmodel <- iwls_logit(logitX, z, standardize = FALSE,
                          maxit = tail(maxit, 1L), tol = tail(tol, 1L))
    # Initial probabilites and prior probabilities
    prob    <- plogis(drop(logitX %*% ccmodel$beta))
    post    <- family$posterior(y, prob, theta)

    # EM algorithm: estimate probabilities (prob; E-step), update the model
    # given the new probabilities (M-step). Always with respect to the
    # selected family.
    iter <- 0
    while ( iter < maxit[1L] ) {
        iter <- iter + 1;


        # M-step: update probabilites and theta
        ccmodel <- iwls_logit(logitX, post, beta = ccmodel$beta, standardize = FALSE,
                              maxit = tail(maxit, 1L), tol = tail(tol, 1L))
        prob    <- plogis(drop(logitX %*% ccmodel$beta))
        theta   <- family$theta(y, post, theta = theta)

        # E-step: update expected a-posteriori
        post    <- family$posterior(y, prob, theta)

        # Store log-likelihood and coefficients of the current
        # iteration.
        llpath[[iter]]   <- family$loglik(y, post, prob, theta)
        coefpath[[iter]] <- cbind(as.data.frame(theta), coef(ccmodel, which = "beta"))
        if ( verbose )
            cat(sprintf("EM iteration %d/%d, ll = %10.2f\r", iter, maxit[1L], llpath[[iter]]))

        # If the log-likelihood decreases: proceed!
        if ( iter == 1 ) next

        # Improvement < 0 (model got worse): continue
        ##if ( (llpath[[iter]]$full - llpath[[iter - 1]]$full) < 0 ) next

        # If the log-likelihood improvement falls below the
        # specified tolerance we assume that the algorithm
        # converged: stop iteration.
        if ( (llpath[[iter]]$full - llpath[[iter - 1]]$full) < tol[1L] ) break

    }; if ( verbose ) cat("\n")

    # Check if algorithm converged before maxit was reached
    converged <- ifelse(iter < maxit[1L], TRUE, FALSE)

    # Combine to data.frame (number of rows corresponds to iter + 1)
    llpath    <- do.call(rbind, llpath)
    coefpath  <- do.call(rbind, coefpath)

    # In this case we have no concomitants, however, the log-likelihood
    # contribution of the concomitants is not == 0 (as log(0) is set to
    # log(sqrt(.Machine$double.eps)) to avoid -Inf). Fix this here.
    #llpath$concomitant <- 0; llpath$full <- llpath$component

    # Return a list with results
    # TODO: Should we implement the binning?
if ( inherits(y, "binned") ) stop("Stop, requires changes on computation of BIC!")
    ll   <- tail(llpath$full, 1)
    rval <- list(prob       = prob,
                 post       = post,
                 theta      = theta,
                 loglik     = ll,
                 edf        = ncol(coefpath),
                 AIC        = - 2 * ll + 2 * ncol(coefpath),
                 BIC        = - 2 * ll + log(length(y)) * ncol(coefpath),
                 ccmodel    = ccmodel,
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
foehnix.reg.fit <- function(formula, data, windfilter = NULL, family = "gaussian",
                    maxit = 100L, tol = 1e-5, standardize = TRUE,
                    alpha = NULL, nlambda = 100L, verbose = TRUE, ...) {
    print("hallo from foehix.reg")
}


# -------------------------------------------------------------------
# Handling control arguments
# -------------------------------------------------------------------
foehnix.control <- function(family, left = -Inf, right = Inf, truncated = FALSE, 
                            standardize = TRUE, maxit = 100L, tol = 1e-8,
                            alpha = NULL, verbose = TRUE, ...) {

    # "truncated" has to be logical
    stopifnot(inherits(truncated, "logical"))

    # Checking limits for censoring/truncation.
    if ( any(!is.null(c(left, right))) ) {
        left  <- max(-Inf, left); right <- min(Inf, right)
        # Left has to be smaller than right.
        if ( left >= right )
            stop("For censoring and truncation: \"left\" has to be smaller than \"right\"!")
        # If left has been set to -Inf and right to Inf: set
        # both to NULL, the default value using a non-censored
        # and non-truncated Gaussian or logistic distribution.
        if ( all(is.infinite(c(left, right))) ) left <- right <- NULL
    }

    # After checking left/right truncation/censoring threshold:
    # check family argument and initialize foehnix family object.
    if ( inherits(family, "foehnix.family") ) {
        if ( verbose ) cat("foehnix.family object probided: use custom family object.\n")
    } else if ( inherits(family, "character") ) {
        family <- match.arg(family, c("gaussian", "logistic"))
        if ( ! all(is.infinite(c(left, right))) ) {
            # Take censored version of "family" using the censoring
            # thresholds left and right.
            if ( ! truncated ) {
                family <- get(sprintf("foehnix_c%s", family))(left = left, right = right)
            # Else take the truncated version of the "family".
            } else {
                family <- get(sprintf("foehnix_t%s", family))(left = left, right = right)
            }
        # Else (left = -Inf, right = Inf): use non-truncated/non-censored version.
        } else {
            family <- get(sprintf("foehnix_%s", family))()
        }
    } else {
        stop("Input \"family\" has to be of class \"character\" or \"foehnix.family\".")
    }

    # Maxit and tol are the maximum number of iterations for the
    # optimization. Need to be numeric. If one value is given it will
    # be used for both, the EM algorithm and the IWLS optimization for
    # the concomitants. If two values are given the first one is used
    # for the EM algorithm, the second for the IWLS solver.
    stopifnot(is.numeric(maxit) | length(maxit) > 2)
    stopifnot(is.numeric(tol)   | length(tol) > 2)

    rval <- list(family = family, left = left, right = right, truncated = truncated,
                 standardize = standardize, maxit = maxit, tol = tol,
                 alpha = alpha, verbose = verbose)
    class(rval) <- c("foehnix.control")
    rval
}
print.foehnix.control <- function(x, ...) str(x)


# -------------------------------------------------------------------
# The simple version for the foehn diagnosis using empirical weighted
# moments for the parameters of the two Gaussian clusters, and a
# logistic regression model for the concomitant part.
# TODO: check what's going on if no intercept is requested by the user
#       for the concomitant model (e.g., ff ~ -1 + rh). The standardize/
#       destandardize function should technically be ready to support
#       this.
# -------------------------------------------------------------------
foehnix <- function(formula, data, switch = FALSE, windfilter = NULL,
                    family = "gaussian",
                    control = foehnix.control(family, ...), ...) { 

    # Start timing (execution time of foehnix)
    timing <- Sys.time() # Measure execution time

    # Stop if input control is not of class foehnix.control
    stopifnot(inherits(control, "foehnix.control"))

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
    y  <- as.numeric(model.response(mf))

    # If a truncated family is used: y has to lie within the
    # truncation points. Density is not defined outside the
    # range ]left, right[.
    if ( is.truncated(control$family) ) {
        if ( min(y, na.rm = TRUE) < control$family$left |
             max(y, na.rm = TRUE) > control$family$right )
            stop(paste(sprintf("Data \"%s\"", as.character(as.list(formula)[[2]])),
                      "outside the specified range for truncation",
                       sprintf("(left = %d, right = %d)", control$family$left, control$family$right)))
    }

    # Check if we have multiple columns with constant values.
    # This would lead to a non-identifiable problem.
    if( sum(apply(mf, 2, function(x) length(unique(na.omit(x)))) <= 1) > 1 )
        stop("Multiple columns with constant values in model.matrix. Stop!")


    # Identify rows with missing values
    idx_notna <- which(!is.na(y) & apply(mf, 1, function(x) sum(is.na(x))) == 0)

    # If a wind sector is given: identify observations with a wind direction
    # outside the user defined sector. These will not be considered in the
    # statistical models.
    idx_wind <- foehnix_filter(data, windfilter)

    # Take all elements which are not NA and are within the
    # defined wind sectors (if wind filter specified).
    idx_take <- if ( is.null(idx_wind) ) idx_notna else idx_notna[idx_notna %in% idx_wind]
    if ( length(idx_take) == 0 ) stop("No data left after applying the required filters.")

    # Subset the model.frame (mf) and the response (y) and pick
    # all valid rows (without missing values on the mandatory columns
    # and, if a wind sector is given, with valid wind direction observations).
    mf <- matrix(unlist(mf[idx_take,]), ncol = ncol(mf), dimnames = list(NULL, names(mf)))
    y  <- y[idx_take]

    # Check whether regularization is preferred over unpenalized
#  regression estimation (only if lambda.min is "auto")
#   if ( lambda.min == "auto" & ncol(mf) > 2 ) {
#       tmp <- cor(na.omit(mf[,-1])); diag(tmp) <- 0
#       if ( max(abs(tmp)) > .75 ) lambda.min <- "AIC"
#   }

    # Helper function subsetting data. Avoids that the matrix
    # is reduced to a single vector if x is a single-column matrix
    # or zoo object.
    subset_data <- function(x, idx)
        as.data.frame(matrix(x[idx,], ncol = ncol(x), dimnames = list(NULL, names(x))))
    # Setting up the model matrix for the concomitant model (logit model).
    logitX <- model.matrix(formula, data = subset_data(data, idx_take))
    if ( control$standardize ) logitX <- standardize(logitX)

    # Non-concomitant model
    if ( length(labels(terms(formula))) == 0 ) {
        if ( control$verbose ) cat("Calling foehnix.noconcomitant.fit\n")
        rval <- do.call("foehnix.noconcomitant.fit",
                        append(list(y = y), control))
    } else if ( is.null(control$alpha) ) {
        if ( control$verbose ) cat("Calling foehnix.unreg.fit\n")
        rval <- do.call("foehnix.unreg.fit",
                        append(list(y = y, logitX = logitX), control))
    } else {
        if ( control$verbose ) cat("Calling foehnix.reg.fit\n")
        rval <- do.call("foehnix.reg.fit",
                        append(list(y = y, logitX = logitX), control))
    }
    if ( control$verbose ) cat("Estimation finished, create final object\n")


    # Final coefficients of the concomitant model have to be destandardized
    # if standardize == TRUE.
    if ( ! is.null(rval$ccmodel) ) {
        if ( ! is.standardized(logitX) ) { coef <- rval$ccmodel$coef }
        else { coef <- destandardize_coefficients(rval$ccmodel$coef, logitX) }
    } else {
        coef <- NULL
    }

    # If inversion has been requested: switch coefficients
    if ( switch ) {
        print(rval$theta)
        rval$ccmodel$coef <- -rval$ccmodel$coef
        rval$ccmodel$beta <- -rval$ccmodel$beta
        rval$coefpath     <- -rval$coefpath
        rval$theta <- list(mu1    = rval$theta$mu2,
                           logsd1 = rval$theta$logsd2,
                           mu2    = rval$theta$mu1,
                           logsd2 = rval$theta$logsd1)
        rval$post <- 1 - rval$post
        # Invert coefficients if switch = TRUE
        coef <- -coef
        print(rval$theta)
    }

    # Create the return list object (foehnix object)
    res <- list(optimizer = rval, data = data, windfilter = windfilter,
                call = match.call(), formula = formula,
                control = control, switch = switch)

    # Store coefficients
    res$coef <- rval$theta; res$coef$concomitants = coef

    # Indizes of the samples dropped/used/filtered
    res$samples <- list(total = nrow(data),
                        na    = length(idx_notna),
                        wind  = length(idx_wind),
                        taken = length(idx_take))

    # The final result, the foehn probability. Creates an object
    # of the same class as the input "data" (currently only zoo!)
    # with two columns. the first contains the final foehn probability
    # (column name prob), the second column contains a flag. The
    # flag is as follows:
    # - NA  if not modelled (data for the model not available).
    # - 0   if foehn probability has been modelled, data not left out
    #       due to the windfilter rules.
    # - 1   if the windfilter removed the observations/sample, not
    #       used for the foehn classification model, but no missing
    #       observations.
    # TODO: data.frame option?
    # 
    # The following procedure is used:
    # - By default, use NA for both columns.
    # - If probabilities modelled: set first column to the modelled
    #   a-posteriory probability, set the second column to TRUE.
    # - If observations removed due to the windfilter options: set
    #   first column to 0 (probability for foehn is 0), set the
    #   second column to FALSE.
    # Foehn probability (a-posteriori probability)
    tmp <- zoo(matrix(NA, ncol = 2, nrow = nrow(data),
                      dimnames = list(NULL, c("prob", "flag"))),
               index(data))
    # Store a-posteriory probability and flag = TRUE
    tmp$prob[idx_take] <- rval$post
    tmp$flag[idx_take] <- 1
    # Store prob = 0 and flag = FALSE with removed due to windfilter rule
    if ( is.null(idx_wind) ) {
        idx <- which(! 1:nrow(tmp) %in% idx_notna)
    } else {
        idx <- 1:nrow(tmp)
        idx <- which(! idx %in% idx_notna | ! idx %in% idx_wind)
    }
    if ( length(idx) > 0 ) tmp[idx,] <- 0

    # Store on final object
    res$prob     <- tmp

    # Store execution time
    res$time <- as.numeric(Sys.time() - timing, units = "secs")

    # Return new object
    class(res) <- "foehnix"
    return(res)
}

predict.foehnix <- function(object, newdata = NULL, type = "response", ...) {

    # Allowed input types
    type <- match.arg(type, c("response", "all"))

    # Pass NA trough function
    hold <- options(); on.exit(options(hold))
    options(na.action = na.pass)

    # If no newdata is provided: take the data set on
    # which the model has been estimated.
    if ( is.null(newdata) ) newdata <- object$data

    # Probability model
    if ( is.null(object$coef$concomitants) ) {
        prob <- mean(object$optimizer$prob)
    } else {
        logitX <- model.matrix(object$formula, newdata)
        prob   <- plogis(drop(logitX %*% object$coef$concomitants))
    }

    # Calculate density
    y    <- model.response(model.frame(object$formula, newdata))
    d1   <- object$control$family$d(y, object$coef$mu1, exp(object$coef$logsd1))
    d2   <- object$control$family$d(y, object$coef$mu2, exp(object$coef$logsd2))
    post <- object$control$family$posterior(y, prob, object$coef)

    # If wind filter is used, set posterior probability to
    # 0 for all observations not inside the filter (they have not
    # been used for modelling as they are not assumed to show
    # any foehn).
    idx_isna <- which(is.na(y) | apply(logitX, 1, function(x) sum(is.na(x)) > 0))
    # Inverse wind filter
    idx_wind <- which(! 1:nrow(newdata) %in% foehnix_filter(newdata, object$windfilter))

    # Create return object of type zoo. By default:
    # - prob is the a-posteriory probability, flag is 1.
    # - For rows removed by the windfilter option: set prob = 0 and flag = 0
    # - For rows where input contained NA: set prob = NA and flag = NA
    res <- zoo(data.frame(prob = post, flag = rep(1, length(post))), index(newdata))
    if ( length(idx_wind) > 0 ) res[idx_wind,] <- 0
    if ( length(idx_isna) > 0 ) res[idx_isna,] <- NA

    # If type is response: return foehn probability
    if ( type == "response" ) return(res)

    # Else return everything we have
    # The "prob" is the probability of the concomitant model. Thus,
    # return as "ccmodel". "post" is the a-posteriori probability and
    # thus the probability of foehn. This is returned as "prob" for the
    # end-user. TODO: Confusing?
    res <- merge(res, zoo(data.frame(density1 = d1, density2 = d2, ccmodel = prob), index(newdata)))
    return(res)

}

# -------------------------------------------------------------------
# Returns fitted probabilities.
# -------------------------------------------------------------------
fitted.foehnix <- function(object, which = "probability", ...) {

    allowed <- c("probability", "flag")
    if ( is.numeric(which) ) { which <- allowed[as.integer(which)] }
    else { which <- match.arg(which, allowed, several.ok = TRUE) }

    if ( length(which) == 1 ) {
        # Only probabilities
        if ( which == "probability" ) return(object$prob$prob)
        # Else only flags
        return(object$prob$flag)
    }
    # Else both
    return(object$prob)
}





