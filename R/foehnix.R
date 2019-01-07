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
# - L@ST MODIFIED: 2019-01-07 21:31 on marvin
# -------------------------------------------------------------------



#' Fitting foehnix Mixture Model Without Concomitant Model.
#'
#' Fitting method for two-component foehnix mixture model without
#' concomitants.
#' Typically not called directly, interfaced via \code{\link{foehnix}}.
#' @param y numeric vector, covariate for the components of the
#'        mixture model, dimension \code{N}.
#' @param family object of class \code{\link{foehnix.family}}.
#' @param switch logical whether or not the two components should be switched.
#'        By default (\code{switch = FALSE}) the component which shows
#'        higher values of \code{y} is assumed to be the foehn cluster!
#'        Depending on what your covariate is you might need to switch
#'        the clusters (by setting \code{switch = TRUE}).
#' @param maxit positive integer, or vector of length 2 with positive
#'        integer values. Maximum number of iterations of the EM algorithm.
#'        Check manual of \code{\link{foehnix.control}} for more details.
#' @param tol numeric, or vector of length 2 containing numeric values.
#'        Tolerance for the EM algorithm.
#'        Check manual of \code{\link{foehnix.control}} for more details.
#' @param verbose logical, default \code{TRUE}. If set to \code{FALSE}
#'        verbose output will be suppressed.
#' @param \dots additional arguments, unused.
#'
#' @seealso
#' \code{\link{foehnix.family}},
#' \code{\link{foehnix}},
#' \code{\link{foehnix.control}},
#' \code{\link{foehnix.noconcomitant.fit}},
#' \code{\link{foehnix.reg.fit}},
#' \code{\link{iwls_logit}}.
#'
#' @author Reto Stauffer
#' @import stats
#' @import utils
foehnix.noconcomitant.fit <- function(y, family, switch = FALSE,
                    maxit = 100L, tol = 1e-5, verbose = TRUE, ...) {

    # Lists to trace log-likelihood path and the development of
    # the coefficients during EM optimization.
    llpath   <- list()
    coefpath <- list()

    # Given the initial probabilities: calculate parameters
    # for the two components (mu1, logsd1, mu2, logsd2) given
    # the selected family and calculate the a-posteriori probabilities.
    z     <- if ( ! switch ) as.numeric(y >= mean(y)) else as.numeric(y <= mean(y))
    theta <- family$theta(y, z, init = TRUE) # M-step

    # Initial probability (fifty fifty) and inital prior
    # probabilites for the component membership.
    prob  <- mean(z)
    post  <- family$posterior(y, prob, theta)

    # EM algorithm: estimate probabilities (prob; E-step), update the model
    # given the new probabilities (M-step). Always with respect to the
    # selected family.
    iter <- 0
    while ( iter < maxit[1L] ) {
        iter <- iter + 1;

        # M-step: update probabilites and theta
        prob  <- mean(post)
        theta <- family$theta(y, post, theta = theta)

        # E-step: calculate a-posteriori probability
        post  <- family$posterior(y, mean(prob), theta)

        # Store log-likelihood and coefficients of the current
        # iteration.
        llpath[[iter]] <- family$loglik(y, post, prob, theta)
        coefpath[[iter]] <- as.data.frame(theta)
        if ( verbose )
            cat(sprintf("EM iteration %d/%d, ll = %10.2f\r", iter, maxit[1L], llpath[[iter]]))

        # If the log-likelihood improvement falls below the
        # specified tolerance we assume that the algorithm
        # converged: stop iteration.
        if( is.na(llpath[[iter]]$full)) {
            print("Likelihood got NA! Start environment browser for debugging ...")
            browser()
        }

        # If the log-likelihood decreases: proceed!
        if ( iter == 1 ) next

        # Likelihood improvement felt below the threshold:
        # remove last iteration and return.
        if ( (llpath[[iter]]$full - llpath[[iter - 1]]$full) < tol[1L] ) {
            llpath[[iter]]   <- NULL  # Removes last likelihood entry
            coefpath[[iter]] <- NULL  # Removes last coefficient entry
            iter <- iter - 1; break   # Stop EM iterations.
        }

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
                 iter       = iter,
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


#' Fitting Unregularized foehnix Mixture Model With Concomitant Model.
#'
#' Fitting method for two-component foehnix mixture model with
#' additional concomitants.
#' Typically not called directly, interfaced via \code{\link{foehnix}}.
#'
#' @param y numeric vector, covariate for the components of the
#'        mixture model, dimension \code{N}.
#' @param logitX numeric matrix of dimension \code{N x P}, covariates
#'        for the concomitant model (logistic regression model matrix).
#' @param family object of class \code{\link{foehnix.family}}.
#' @param switch logical whether or not the two components should be switched.
#'        By default (\code{switch = FALSE}) the component which shows
#'        higher values of \code{y} is assumed to be the foehn cluster!
#'        Depending on what your covariate is you might need to switch
#'        the clusters (by setting \code{switch = TRUE}).
#' @param maxit positive integer, or vector of length 2 with positive
#'        integer values. Maximum number of iterations of the EM algorithm
#'        and the concomitant model.
#'        Check manual of \code{\link{foehnix.control}} for more details.
#' @param tol numeric, or vector of length 2 containing numeric values.
#'        Tolerance for the optimization (EM algorithm and concomitant model).
#'        Check manual of \code{\link{foehnix.control}} for more details.
#' @param verbose logical, default \code{TRUE}. If set to \code{FALSE}
#'        verbose output will be suppressed.
#' @param alpha #TODO: currently unused.
#' @param ... additional arguments, unused.
#'
#' @seealso
#' \code{\link{foehnix.family}},
#' \code{\link{foehnix}},
#' \code{\link{foehnix.control}},
#' \code{\link{foehnix.noconcomitant.fit}},
#' \code{\link{foehnix.reg.fit}},
#' \code{\link{iwls_logit}}.
#'
#' @author Reto Stauffer
#' @import stats
#' @import utils
foehnix.unreg.fit <- function(y, logitX, family, switch = FALSE,
                    maxit = 100L, tol = 1e-5, verbose = TRUE,
                    alpha = NULL, ...) {

    # Lists to trace log-likelihood path and the development of
    # the coefficients during EM optimization.
    llpath   <- list()
    coefpath <- list()
    
    # Given the initial probabilities: calculate parameters
    # for the two components (mu1, logsd1, mu2, logsd2) given
    # the selected family and calculate the a-posteriori probabilities.
    z     <- if ( ! switch ) as.numeric(y >= mean(y)) else as.numeric(y <= mean(y))
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
        if ( (llpath[[iter]]$full - llpath[[iter - 1]]$full) < tol[1L] ) {
            llpath[[iter]]   <- NULL  # Removes last likelihood entry
            coefpath[[iter]] <- NULL  # Removes last coefficient entry
            iter <- iter - 1; break   # Stop EM iterations.
        }

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
                 iter       = iter,
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


#' Fitting Regularized foehnix Mixture Model With Concomitant Model.
#'
#' Fitting method for two-component foehnix mixture model with regularization.
#' Typically not called directly, interfaced via \code{\link{foehnix}}.
#'
#' TODO: Method not yet implemented, as soon as the method itself
#' has been written: update manual page!
#'
#' @param y numeric vector, covariate for the components of the
#'        mixture model, dimension \code{N}.
#' @param logitX numeric matrix of dimension \code{N x P}, covariates
#'        for the concomitant model (logistic regression model matrix).
#' @param family object of class \code{\link{foehnix.family}}.
#' @param switch logical whether or not the two components should be switched.
#'        By default (\code{switch = FALSE}) the component which shows
#'        higher values of \code{y} is assumed to be the foehn cluster!
#'        Depending on what your covariate is you might need to switch
#'        the clusters (by setting \code{switch = TRUE}).
#' @param maxit positive integer, or vector of length 2 with positive
#'        integer values. Maximum number of iterations of the EM algorithm
#'        and the concomitant model.
#'        Check manual of \code{\link{foehnix.control}} for more details.
#' @param tol numeric, or vector of length 2 containing numeric values.
#'        Tolerance for the optimization (EM algorithm and concomitant model).
#'        Check manual of \code{\link{foehnix.control}} for more details.
#' @param verbose logical, default \code{TRUE}. If set to \code{FALSE}
#'        verbose output will be suppressed.
#' @param alpha #TODO: currently unused.
#' @param ... additional arguments, unused.
#'
#' @seealso \code{\link{foehnix}}, \code{\link{foehnix.control}},
#' \code{\link{foehnix.noconcomitant.fit}}, \code{\link{foehnix.reg.fit}},
#' \code{\link{iwls_logit}}.
#'
#' @author Reto Stauffer
#####foehnix.reg.fit <- function(formula, data, filter = NULL, family = "gaussian",
#####                    maxit = 100L, tol = 1e-5, standardize = TRUE,
#####                    alpha = NULL, nlambda = 100L, verbose = TRUE, ...) {
foehnix.reg.fit <- function(y, logitX, family, switch = FALSE,
                    maxit = 100L, tol = 1e-5, verbose = TRUE,
                    alpha = NULL, ...) {

    print("hallo from foehix.reg")
}


#' foehnix Two-Component Mixture-Model Control Object
#'
#' Used to control the \code{\link{foehnix}} mixture models.
#' @param family character specifying the distribution of the components in the
#'        mixture model. Allowed: \code{"gaussian"} and \code{"logistic"}.  For
#'        experts: custom \code{foehnix.family} objects can be provided as well.
#' @param switch logical whether or not the two components should be switched.
#'        By default (\code{switch = FALSE}) the component which shows
#'        higher values of \code{y} is assumed to be the foehn cluster!
#'        Depending on what your covariate is you might need to switch
#'        the clusters (by setting \code{switch = TRUE}).
#' @param left default is \code{-Inf}, left censoring or truncation point.  See
#'        also input \code{right} and input \code{truncated}. Can be set to any finite
#'        numeric value.
#' @param right default is \code{Inf}, right censoring or truncation point.  See
#'        also input \code{left} and input \code{truncated}. Can be set to any finite
#'        numeric value.
#' @param truncated logical. If set to \code{TRUE} truncation is used instead of
#'        \code{censored}. This only affects the \code{\link{foehnix}} model estimate
#'        if input \code{left} and/or input \code{right} are specified.
#' @param standardize logical flag, default is \code{TRUE}. Defines whether or
#'        not the model matrix for the concomitant model should be standardized for
#'        model estimation. Recommended.
#' @param maxit control argument for the iterative solvers. Default is
#'        \code{100L}, the maximum number of iterations for the EM algorithm and the
#'        IWLS backfitting algorithm for the concomitant model.  If a vector of length
#'        two is provided the first value is used for the EM algorithm, the second for
#'        the IWLS backfitting.
#' @param force.inflate logical, default is \code{FALSE}. \code{\link{foehnix}} creates
#'        a strictly regular time series object by inflating the data set using the
#'        smallest time interval in the data set. If the inflation rate is larger than
#'        2 the script will stop except the user forces inflation by specifying
#'        \code{force.inflate = TRUE}. See 'Details' section for more information.
#' @param tol similar as for \code{maxit}. Used to identify convergence of the
#'        iterative solvers. Default is \code{1e-8}, if two values are given the first
#'        will be used for the EM algorithm, the second one for the IWLS backfitting
#'        procedure. If set to \code{-Inf} \code{maxit} will be used as stopping
#'        criteria.
#' @param alpha TODO alpha parameter for the penalization of the concomitant model.
#' @param verbose logical, if set to \code{FALSE} output is suppressed.
#' @param ... currently set to hell.
#'
#' @details \code{\link{foehnix}} models are based on time series objects. 
#' For some methods (e.g., to create nice and easy to read time series plots and
#' count statistics)
#' \code{\link{foehnix}} inflates the time series object using the smallest time
#' interval in the data set. This can, possibly, yield very large data sets. Thus,
#' \code{\link{foehnix}} is pre-calculating the inflation rate, the fraction between
#' the length of the inflated data set versus the length of the data set provided by
#' the user. If this inflation rate exceeds 2 the script will raise an error!
#'
#' In this case the user should make sure that the time series object provided
#' is proper before continuing. A possible scenario: a user is performing foehn diagnosis
#' using 5 years of data from one station with 10 minute observations. This yields
#' (neglecting leap years) \code{5 * 365 * 144 = 262.800} observations. Imagine that there
#' is one incorrect observation reported one second after one of the regular
#' 10 minute records. The smallest time increment would thus be 1 second. This would
#' yield an inflated time series object with a total record length of
#' \code{5 * 365 * 86.400 = 157.680.000}. Even if only filled with missing values
#' (\code{NA}) this will be extremely memory demanding. To avoid this action
#' \code{\link{foehnix}} will stop in such situations.
#'
#' However, the user is allowed to overrule this condition by setting the
#' \code{force.inflate} option to \code{TRUE}.
#'
#' @seealso \code{\link{foehnix}}, \code{\link{foehnix.family}}.
#'
#' @author Reto Stauffer
#' @import utils
#' @export
foehnix.control <- function(family, switch, left = -Inf, right = Inf, truncated = FALSE, 
                            standardize = TRUE, maxit = 100L, tol = 1e-8,
                            force.inflate = FALSE,
                            alpha = NULL, verbose = TRUE, ...) {

    # "truncated" has to be logical
    stopifnot(inherits(truncated, "logical"))

    # Logical values
    stopifnot(inherits(standardize,   "logical"))
    stopifnot(inherits(verbose,       "logical"))
    stopifnot(inherits(force.inflate, "logical"))

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

    rval <- list(family = family, switch = switch,
                 left = left, right = right, truncated = truncated,
                 standardize = standardize, maxit = maxit, tol = tol,
                 force.inflate = force.inflate,
                 alpha = alpha, verbose = verbose)
    class(rval) <- c("foehnix.control")
    rval
}


#' @export
print.foehnix.control <- function(x, ...) str(x)


#' Foehn Classification Based on a Two-Component Mixture Model
#'
#' This is the main method of the foehnix package to estimate
#' two-component mixture models for automated foehn classification.
#' 
#' @param formula an object of class \code{formula} (or one that can be coerced
#'        to that class): a symbolic description of the model to be fitted.  The
#'        details of model specification are given under 'Details'.
#' @param data a regular (not necessarily strictly regular)
#'        time series object of class \code{zoo} containing the
#'        variables for the two-part mixture model.
#' @param switch logical. If set to \code{TRUE} the two estimated components
#'        will be switched. This is important if the covariate for the components
#'        (left hand side of input \code{formula} is smaller for cases with foehn than
#'        for cases without foehn, e.g., when using the temperature difference as main
#'        covariate.
#' @param filter a named list can be provided to apply a custom (simple) filter
#'        to the observations on \code{data}. Can be used to e.g., prespecify a
#'        specific wind sector for foehn winds. Please read the manual of the
#'        \code{\link{foehnix_filter}} method for more details and examples.
#' @param family character (at the moment \code{"gaussian"} or \code{"logistic"})
#'        or an object of class \code{foehnix.family}.
#' @param control additional control arguments, see \code{\link{foehnix.control}}.
#' @param ... forwarded to \code{\link{foehnix.control}}
#'
#' @return Returns an object of class \code{foehnix}.
#'
#' @details
#' The two-component mixture model can be specified via formula object where
#' the left hand side of the formula contains the variable explaining the two
#' components (only one variable), the right hand side of the formula specifies
#' the concomitant variables (multiple variables allowed).  As an example:
#' let's assume that our zoo object '\code{data}' contains the following
#' columns:
#' \itemize{
#'     \item \code{ff}: observed wind speed at target site
#'     \item \code{rh}: observed relative humidity at target site
#'     \item \code{diff_t}: temperature difference between target site and a station
#'           upstream of the foehn wind direction
#' }
#'
#' The specification for \code{formula} could e.g. look as follows:
#' \itemize{
#'     \item \code{ff ~ 1}: the two components of the mixture model will be
#'           based on the observed wind speed (\code{ff}), no concomitant
#'           variables are given (intercept only). 
#'     \item \code{ff ~ rh}: similar to the specification above, but using
#'            observed relative humidity (\code{rh}) as concomitant.
#'     \item \code{ff ~ rh + diff_t}: as above but with an additional second
#'           concomitant variable (observed temperature difference, \code{diff_t}).
#'     \item \code{diff_t ~ ff + rh}: using temperature difference as the main
#'           variable for the two components while \code{ff} and \code{rh} are
#'           used as concomitants.
#' }
#'  
#' Note that these are just examples and have to be adjusted given data
#' availability, location, structure/names of the variables in the \code{data}
#' object.
#' 
#' An optional numeric vector of length \code{2} can be provided to filter the
#' data. If set, only times (rows in \code{data}) where the observed wind
#' direction \code{dd} lies within the wind sector specified.  For the examples
#' the wind direction \code{dd} is assumed to be in degrees, meteorological
#' definition (\code{90} = from east, \code{180} = from south, \dots).  The
#' following condition is used:
#' \itemize{
#'     \item If \code{windsector[1L] < windsector[2L]}: use \code{data} where
#'           \code{data$dd >= windsector[1L] & data$dd <= windsector[2L]}
#'           (e.g., \code{windsector = c(90, 270)} for south wind sector).
#'     \item If \code{windsector[1L] > windsector[2L]}: use \code{data} where
#'           \code{data$dd >= windsector[1L] | data$dd <= windsector[2L]}
#'           (e.g., \code{windsector = c(270, 90)} for north wind sector).
#' }
#' 
#' TODO: Details for lambda. If \code{"auto"} the default is \code{"loglik"}
#' except that the correlation between two or more covariates used in
#' \code{formula} exceeds \code{0.75} (both positive or negative). In this case
#' \code{lambda.min = "AIC"} will be used automatically with additional
#' regularization.  If \code{lambda.min = "loglik"} an unpenalized estimate
#' will be returned.  If \code{lambda.min} is either AIC or BIC a ridge penalty
#' (L2 penalty) is used. A fixed set of penalties will be tested. The one
#' minimising either the AIC or BIC criteria will be used for the foehn
#' classification model.
#' 
#' @references Plavcan D, Mayr GJ, Zeileis A (2014).
#' Automatic and Probabilistic Foehn Diagnosis with a Statistical Mixture Model.
#' \emph{Journal of Applied Meteorology and Climatology}.
#' \bold{53}(3), 652--659. \doi{10.1175/JAMC-D-13-0267.1}
#' 
#' Gr\\"un B, Leisch F (2007).
#' Fitting Finite Mixtures of Generalized Linear Regressions in \emph{R}.
#' \emph{Computational Statistics \& Data Analysis}. \bold{51}(11), 5247--5252.
#' \doi{10.1016/j.csda.2006.08.014}
#' 
#' Gr\\"un B, Leisch F (2008).
#' FlexMix Version 2: Finite Mixtures with Concomitant Variables and Varying and Constant Parameters.
#' \emph{Journal of Statistical Software, Articles}.
#' \bold{28}(4), 1--35. \doi{10.18637/jss.v028.i04}
#' 
#' Fraley C, Raftery AE (2002).
#' Model-Based Clustering, Discriminant Analysis, and Density Estimation.
#' \emph{Journal of the American Statistical Association}.
#' \bold{97}(458), 611--631. \doi{10.1198/016214502760047131}
#'
#' @seealso
#' See \code{\link{foehnix_filter}} for more information about the
#' \code{filter} option.  See also: \code{\link{tsplot}}, \code{\link{windrose}}.
#' TODO: filter should be renamed to filter.
#' 
#' Foehnix family objects: \code{\link{foehnix.family}}.
#'
#' S3 methods for \code{foehnix} objects:
#' \code{\link{plot.foehnix}}, \code{\link{predict.foehnix}}, \code{\link{fitted.foehnix}},
#' \code{\link{print.foehnix}}, \code{\link{windrose.foehnix}}, \code{\link{coef.foehnix}},
#' ...
#'
#' @importFrom zoo zoo index is.regular
#' @import utils
#' @import stats
#' @author Reto Stauffer
#' @export foehnix
#'
# TODO: check what's going on if no intercept is requested by the user
#       for the concomitant model (e.g., ff ~ -1 + rh). The standardize/
#       destandardize function should technically be ready to support
#       this.
foehnix <- function(formula, data, switch = FALSE, filter = NULL,
                    family = "gaussian",
                    control = foehnix.control(family, switch, ...), ...) { 

    if ( "windfilter" %in% names(list(...)) )
        stop("STOP! The \"windfilter\" option has been replaced by the \"filter\" option!",
             "Please adjust your calls (replace \"windfilter\" with \"filter\"!")

    # Start timing (execution time of foehnix)
    timing <- Sys.time() # Measure execution time

    # Stop if input control is not of class foehnix.control
    stopifnot(inherits(control, "foehnix.control"))

    # If the original time series object is not regular: stop.
    if ( ! is.regular(data) )
        stop("The \"zoo\" time seris object on \"data\" has to be regular.")

    # Create strictly regular time series object with POSIXct
    # time stamp. Making a time series strictly regular means that the
    # time series is inflated such that a continuous time series is
    # getting created using the _smallest time step_. Depending on the data
    # set this can create a large data set.
    # Thus foehnix calculates the inflation rate (the ratio between the
    # data set provided by the user and the one which would be created
    # when making the time series object strictly regular). If the inflation
    # rate is more than a factor of two the script will stop and raise an error.
    # When `force.inflate` is set a warning instead of an error will be shown.
    index(data) <- as.POSIXct(index(data))
    if ( ! is.regular(data, strict = TRUE) ) {

        # deltat: smallest time increment/time interval
        interval <- deltat(data)
        # inflated_index: new fully strict time series object.
        inflated_index <- seq(min(index(data)), max(index(data)), by = interval)

        # If the inflation ratio is larger than 2: stop!
        if ( (length(inflated_index) / nrow(data)) > 2 & ! control$force.inflate ) {
            str <- paste("ERROR:\n\n",
                         "You have provided a time series object spanning the time period",
                         "%s to %s.",
                         "The smallest recorded time interval is %d seconds.",
                         "foehnix tries to inflate the time series to create a strictly",
                         "regular time series object which, in this case, would yield a",
                         "data set of dimension %d x %d (%d values) which is %.2f times",
                         "the original data set. To avoid running into memory issues",
                         "foehnix stops here! We ask you to check your data set.",
                         "This condition can be overruled by setting the input argument",
                         "force.inflate = TRUE if needed. For more details please read the",
                         "foehnix.control manual page.")
            stop(sprintf(str, min(index(data)), max(index(data)), deltat(data),
                         length(inflated_index), ncol(data),
                         length(inflated_index) * ncol(data),
                         length(inflated_index) / nrow(data)))
        }
        # Keep the number of observations (rows) added due to inflation.
        N_inflated <- length(inflated_index) - nrow(data)
        data <- merge(data, zoo(, inflated_index))
    }

    # Extracting model.frame used for the concomitant model,
    # and the vector y used for the clustering (main covariate).
    # Keep missing values.
    mf <- try(model.frame(formula, data, na.action = na.pass))
    if ( inherits(mf, "try-error") )
        stop(paste("Cannot create model matrix given the formula provided. Please",
                   "check that all variables used in the formula exist in the \"data\"",
                   "time series object!"))
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
    filter_obj <- foehnix_filter(data, filter)


    # Take all elements which are not NA and where the
    # foehnix_filter routine returned an index on "good" (not
    # removed due to filter rules OR due to missing values).
    idx_take <- idx_notna[idx_notna %in% filter_obj$good]
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

    # If there was only one iteration: drop a warning
    if ( rval$iter == 1 )
        warning(paste("The EM algorithm stopped after one iteration!",
                      "The coefficients returned are the initial coefficients.",
                      "Indicates that the model as specified is not suitable",
                      "for the data. Suggestion: check model (e.g, using",
                      "plot(...) and summary(..., detailed = TRUE)) and",
                      "try a different model specification (change formula)."))

    # Create the return list object (foehnix object)
    res <- list(optimizer = rval, data = data, filter = filter,
                filter_obj = filter_obj,
                inflated = N_inflated,
                call = match.call(), formula = formula,
                control = control, switch = switch)

    # Store coefficients
    res$coef <- rval$theta; res$coef$concomitants = coef

    # Calculate the weightes standard error of the estimated
    # coefficients for the test statistics.
    # First: calculate weighted sum of squared residuals for
    # both components
    res_c1 <- (y - res$coef$mu1) * (1 - rval$post)
    res_c2 <- (y - res$coef$mu2) *      rval$post
    mu1.se <- sqrt((sum(res_c1^2) / (sum((1 - rval$post)^2) * (sum(1 - rval$post) - 1))))
    mu2.se <- sqrt((sum(res_c2^2) / (sum(     rval$post^2 ) * (sum(    rval$post) - 1))))

    # Standard errors for intercept of mu1 (component1) and mu2 (component 2)
    res$mu.se  <- setNames(c(mu1.se, mu2.se), c("mu1.se", "mu2.se"))

    # The final result, the foehn probability. Creates an object
    # of the same class as the input "data" (currently only zoo!)
    # with two columns. the first contains the final foehn probability
    # (column name prob), the second column contains a flag. The
    # flag is as follows:
    # - NA  if not modelled (data for the model not available).
    # - 0   if foehn probability has been modelled, data not left out
    #       due to the filter rules.
    # - 1   if the filter removed the observations/sample, not
    #       used for the foehn classification model, but no missing
    #       observations.
    # TODO: data.frame option?
    # 
    # The following procedure is used:
    # - By default, use NA for both columns.
    # - If probabilities modelled: set first column to the modelled
    #   a-posteriory probability, set the second column to TRUE.
    # - If observations removed due to the filter options: set
    #   first column to 0 (probability for foehn is 0), set the
    #   second column to FALSE.
    # Foehn probability (a-posteriori probability)
    tmp <- zoo(matrix(NA, ncol = 2, nrow = nrow(data),
                      dimnames = list(NULL, c("prob", "flag"))), index(data))
    # Store a-posteriory probability and flag = TRUE
    tmp$prob[idx_take] <- rval$post
    tmp$flag[idx_take] <- 1
    # Store prob = 0 and flag = FALSE with removed due to filter rule
    if ( length(filter_obj$bad) > 0 ) tmp[filter_obj$bad,] = 0

    # Store on final object
    res$prob     <- tmp

    # Store execution time
    res$time <- as.numeric(Sys.time() - timing, units = "secs")

    # Return new object
    class(res) <- "foehnix"
    return(res)
}


#' Predict Method for foehnix Mixture Models
#'
#' Some details.
#' @param object a \code{\link{foehnix}} mixture model object.
#' @param newdata if \code{NULL} (default) the prediction of the
#'        underlying training data set will be returned
#'        (see also \code{\link{fitted}}). Else \code{newdata}
#'        has to be a \code{zoo} object providing the required
#'        variables which have been used for model fitting
#'        and filtering (see \code{\link{foehnix}}).
#' @param type character, one of \code{"response"} (default),
#'        \code{"all"}.
#' @param ... additional arguments, ignored.
#' @return Returns a \code{zoo} object with foehn probabilities
#' and (if \code{type = "all"}) additional information. See 'Details'
#' section for more information.
#'
#' @details Used for prediction (perform foehn diagnosis given the
#' estimated parameters on a new data set (\code{newdata}). If no
#' new data set is provided (\code{newdata = NULL}) the prediction
#' is made on the internal data set, the data set which has been
#' used to train the \code{\link{foehnix}} mixture model.
#' If a new data set is provided (\code{newdata}) the foehn diagnosis
#' will be performed on this new data set, e.g., based on a set
#' of new observations when using \code{\link{foehnix}} for operational
#' near real time foehn diagnosis.
#'
#' Note that \code{newdata} has to be a \code{zoo} object containing
#' the required information to perform the \code{\link{foehnix}} diagnosis,
#' namely the variables used for classification (see
#' \code{\link{formula.foehnix}} plus the ones used to filter the data
#' (see \code{\link{foehnix}} input argument \code{filter}).
#'
#' Usually \code{type = "response"} is used which returns the foehn
#' probabilities. If \code{type = "all"} a set of additional values will
#' be returned, namely:
#' \itemize{
#'     \item \code{density1} density of the first component of the mixture model.
#'     \item \code{density2} density of the second component (foehn component) of the
#'           mixture model.
#'     \item \code{ccmodel} probability from the concomitant model.
#' }
#' Note that the foehn probability is simply given by:
#' \itemize{
#'     \item ccmodel * density2 / ((1 - ccmodel) * density1 + ccmodel * density2)
#' }
#'
#' @import zoo
#' @import stats
#' @author Reto Stauffer
#' @export
predict.foehnix <- function(object, newdata = NULL, type = "response", ...) {

    # Allowed input types
    type <- match.arg(type, c("response", "all"))

    # Pass NA trough function
    hold <- options(); on.exit(options(hold))
    options(na.action = stats::na.pass)

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
    filter_obj <- foehnix_filter(newdata, object$filter)
    print(filter_obj)

    # Create return object of type zoo. By default:
    # - prob is the a-posteriory probability, flag is 1.
    # - For rows removed by the filter option: set prob = 0 and flag = 0
    # - For rows where input contained NA: set prob = NA and flag = NA
    res <- zoo(data.frame(prob = post, flag = rep(1, length(post))), index(newdata))
    if ( length(filter_obj$ugly) > 0 ) res[filter_obj$ugly,] <- NA
    if ( length(filter_obj$bad)  > 0 ) res[filter_obj$bad,]  <- 0


    # If type is response: return foehn probability
    if ( type == "response" ) return(res)

    # Else return everything we have
    # The "prob" is the probability of the concomitant model. Thus,
    # return as "ccmodel". "post" is the a-posteriori probability and
    # thus the probability of foehn. This is returned as "prob" for the
    # end-user. TODO: Confusing?
    res <- merge(res, zoo(data.frame(density1 = d1, density2 = d2, ccmodel = prob),
                               index(newdata)))
    return(res)

}


#' Returns Fitted Values of foehnix Mixture Models
#'
#' Extracts fitted probabilities and/or flags from a \code{\link{foehnix}}
#' mixture model.
#'
#' @param object a \code{\link{foehnix}} model object.
#' @param which what to get as return. Can a character string, one of
#'        \code{"probability"} or \code{"flag"}, or \code{"both"}.
#'        Alternatively integers can be used (\code{1}, \code{2}, or \code{c(1,2)}).
#' @param ... additional arguments, ignored.
#' @return Returns a univariate or multivariate \code{zoo} object.
#'
#' @author Reto Stauffer
#' @export
fitted.foehnix <- function(object, which = "probability", ...) {

    allowed <- c("probability", "flag", "both")
    if ( is.numeric(which) ) { which <- allowed[as.integer(which)] }
    else { which <- match.arg(which, allowed, several.ok = TRUE) }

    if ( length(which) == 1 ) {
        # Only probabilities
        if ( which == "probability" ) return(object$prob$prob)
        if ( which == "flag" ) return(object$prob$flag)
        # Else only flags
    }
    # Else both
    return(object$prob)
}



