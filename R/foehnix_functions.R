
utils::globalVariables(c("time_mid", "yday_mid", "value"))

#' Standardize (Model) Matrix
#'
#' Function to standardize the columns of a matrix, used to
#' standardize the model matrix before estimating the regression
#' coefficients of a generalized linear model (\code{\link[foehnix]{iwls_logit}}).
#'
#' @param x matrix of dimension \code{N x p}.
#' @param ... additional arguments, ignored.
#' @return Returns a matrix of the same dimension as input \code{x}
#' but with standardized data. The return object is of class
#' \code{c("standardized", "matrix")} which comes with some handy
#' S3 methods.
#'
#' @examples
#' # Example data set
#' data("airquality")
#' airquality <- na.omit(airquality)
#'
#' # Create model matrix
#' X <- model.matrix(Ozone ~ ., data = airquality)
#' print(head(X))
#'
#' # Standardize
#' S <- standardize(X)
#' print(head(S))
#'
#' is.standardized(X)
#' is.standardized(S)
#'
#' # Get parameters used for standardization
#' center(S)
#' scale(S)
#'
#' # Destandardize
#' D <- destandardize(S)
#'
#' # Check
#' all.equal(D, X, check.attributes = FALSE)
#'
#' @author Reto Stauffer
#' @export
standardize <- function(x, ...) UseMethod("standardize")

#' @rdname standardize
#' @export
standardize.matrix <- function(x, ...) {
    # Scale covariates
    scaled_center <- structure(rep(0, ncol(x)), names = colnames(x))
    scaled_scale  <- structure(rep(1, ncol(x)), names = colnames(x))
    for ( i in 1:ncol(x) ) {
        if ( sd(x[,i], na.rm = TRUE) == 0 ) next
        scaled_center[i] <- mean(x[,i], na.rm = TRUE)
        scaled_scale[i]  <- sd(x[,i], na.rm = TRUE)
        x[,i] <- (x[,i] - scaled_center[i]) / scaled_scale[i]
    }
    attr(x, "scaled:center") <- scaled_center
    attr(x, "scaled:scale")  <- scaled_scale
    class(x) <- c("standardized", class(x))
    return(x)
}


#' @usage
#' # Returns 'scaled:scale' used for standardization
#' scale(x, ...)
#' @rdname standardize
#' @export
scale.standardized <- function(x, ...) return(attr(x, "scaled:scale"))


#' @usage
#' # Returns 'scaled:center' used for standardization
#' center(x, ...)
#' @rdname standardize
#' @export
center <- function(x, ...) UseMethod("center")
#' @export
center.standardized <- function(x, ...) return(attr(x, "scaled:center"))

#' Destandardize a Standardized Matrix
#'
#' @details Reverse function of \code{\link[foehnix]{standardize}}.
#'
#' @param x standardized matrix of class \code{standardized}.
#' @param ... forwarded, unused.
#'
#' @seealso \code{\link[foehnix]{standardize}}
#' @author Reto Stauffer
#'
#' @examples
#' # See example on R documentation for \code{?standardize}.
#'
#' @export
destandardize <- function(x, ...) UseMethod("destandardize")

#' @export
#' @rdname standardize
destandardize.standardized <- function(x, ...) {
    center <- center(x)
    scale  <- scale(x)
    for ( i in seq_along(center) ) x[,i] <- x[,i] * scale[i] + center[i]
    matrix(x, ncol = ncol(x), nrow = nrow(x), dimnames = dimnames(x))
}



#' Check if Matrix is Standardized
#'
#' Returns \code{TRUE}
#' if input \code{x} is a standardized matrix, else \code{FALSE}.
#'
#' @param x a matrix or standardized matrix.
#' @param ... ignored.
#'
#' @seealso \code{\link[foehnix]{standardize}}.
#' @export
is.standardized <- function(x, ...) UseMethod("is.standardized")

#' @export
is.standardized.matrix <- function(x, ...) inherits(x, "standardized")



#' Destandardize Regression Coefficients
#'
#' Destandardize coefficients. Brings coefficients back to
#' the "real" scale if standardized coefficients are used when
#' estimating the logistic regression model (concomitant model).
#'
#' @param beta regression coefficients estimated on standardized data.
#' @param X object of class \code{\link[foehnix]{standardize}}.
#' @return Returns destandardized regression coefficients, same object
#' as input \code{beta}.
#'
#' @seealso \code{\link[foehnix]{standardize}}. Used in \code{\link[foehnix]{foehnix}}
#' and \code{\link[foehnix]{iwls_logit}}.
#'
#' @rdname standardize
#' @export
destandardize_coefficients <- function(beta, X) {
    scaled_center = attr(X, "scaled:center")
    scaled_scale  = attr(X, "scaled:scale")
    # Do we have an intercept?
    ic <- grep("^\\(Intercept\\)$", rownames(beta))
    if ( length(ic) == 1 ) {
        # Descaling intercept
        beta[ic,]  <- beta[ic,] - sum(beta[-ic,] * scaled_center[-ic]  / scaled_scale[-ic])
        # Descaling all other regression coefficients
        beta[-ic,] <- beta[-ic,] / scaled_scale[-ic]
    } else {
        beta <- beta / scaled_scale
    }
    return(beta)
}


#' @usage
#' # Log-likelihood sum
#' logLik(object, ...)
#' @rdname foehnix
#' @export
logLik.foehnix <- function(object, ...) structure(object$optimizer$loglik, names = "loglik")


#' @usage
#' # Number of observations used to train the model
#' nobs(object, ...)
#' @rdname foehnix
#' @export
nobs.foehnix <- function(object, ...) return(object$nobs)


#' @usage
#' # Akaike information criterion
#' AIC(object, ...)
#' @rdname foehnix
#' @export
AIC.foehnix <- function(object, ...)    structure(object$optimizer$AIC, names = "AIC")


#' @usage
#' # Bayesian information criterion
#' BIC(object, ...)
#' @rdname foehnix
#' @export
BIC.foehnix <- function(object, ...)    structure(object$optimizer$BIC, names = "BIC")

#' @usage
#' # Ignorance (mean negative log-likelihood)
#' IGN(object, ...)
#' @rdname foehnix
#' @export
IGN.foehnix <- function(object, ...) {
    structure(-logLik(object) / nobs(object), names = "IGN")
}

#' Returns the Ignorance of an Object
#'
#' Returns the ignorance (mean negative log-likelihood)
#' of the \code{object}. 
#'
#' @param object the object to be evaluated.
#' @param ... forwarded to generic methods.
#' @seealso \code{\link[foehnix]{foehnix}}
#' @export
IGN <- function(object, ...) UseMethod("IGN")


#' Returns Effective Degrees of Freedom
#'
#' Function which returns the effective degrees of freedom.
#' @param object the object from which the effective degrees
#'        of freedom should be returned.
#' @param ... forwarded to class specific edf methods.
#' @export
edf <- function(object, ...) UseMethod("edf")

#' @usage
#' # Effective degrees of freedom
#' edf(object, ...)
#' @rdname foehnix
#' @export
edf.foehnix <- function(object, ...)     structure(object$optimizer$edf, names = "edf")


#' @usage
#' # Print object
#' print(x, ...)
#' @rdname foehnix
#' @export
print.foehnix <- function(x, ...) print(summary(x, ...))


#' Write Estimated Probabilities to CSV File
#'
#' Write the results of a \code{\link[foehnix]{foehnix}} model into a CSV
#' text file. Custom date/time information can be specified using the
#' input argument \code{format}. By default UNIX timestamp will be used.
#'
#' @param x a \code{\link[foehnix]{foehnix}} object
#' @param file character, name of the target file
#' @param info logical, whether or not header information
#'      should be written
#' @param format NULL or a character to specify the format in which the
#'      date/time information should be written to the \code{file} (forwarded to
#'      \code{strftime})
#' @param ... currently ignored
#'
#' @return Invisible return of the data.frame written to the output file.
#' @author Reto Stauffer
#' @export
write.csv.foehnix <- function(x, file, info = TRUE, format = NULL, ...) {

    # Prepare the data set
    if (is.null(format)) {
        datetime <- data.frame(timestamp = as.numeric(index(x$prob)))
        fmt_data <- "%10d; %6.4f; %4d"
        fmt_head <- "%10s; %6s; %4s"
    } else {
        datetime <- data.frame(datetime = strftime(index(x$prob), format), 
                               stringsAsFactors = FALSE)
        nmax <- max(nchar(datetime$datetime), na.rm = TRUE)
        fmt_data <- paste0("%", nmax, "s; %6.4f; %4d")
        fmt_head <- paste0("%", nmax, "s; %6s; %4s")
        if (anyDuplicated(datetime$datetime))
            warning(sprintf("Custom format \"%s\" results in non-unique datetime information.",
                    format))
    }
    # Combine
    data <- cbind(datetime, as.data.frame(x$prob, row.names = 1:nrow(x$prob)))

    # Custom information
    if (info) {
        formula_to_str <- function(x)
            sprintf("%s ~ %s", as.character(x)[2L], as.character(x)[3L])
        info <- list(
             paste(rep("-", 70), collapse = ""),
             "Results from foehnix classification model",
             sprintf("Object name:     %s", deparse(substitute(x))),
             sprintf("Model formula:   %s", formula_to_str(formula(x))),
             sprintf("Output created:  %s", Sys.time()),
             "Description:",
             sprintf(" - %s: date time information", names(data)[1L]),
             " - prob: estimated foehn probability",
             " - flag: (1) within or (0) outside defined wind secor,",
             "         (NA) not possible to provide probabilities due to missing data",
             paste(rep("-", 70), collapse = "")
           )
        info <- sprintf("# %s", unlist(info))
    }

    # Format output
    fmtfun <- function(x, idx, fmt, con) {
        writeLines(do.call(sprintf, c(as.list(x[idx, ]), list(fmt = fmt))),
                   con = con)
    }

    # Remove file
    if (file.exists(file)) file.remove(file)
    # Open text connection
    fid = file(file, open = "a")
    # Write info
    if (is.character(info)) writeLines(info, con = fid)
    # Write header
    writeLines(do.call(sprintf, c(as.list(names(data)), fmt = fmt_head)), con = fid)
    # Write data
    dead_end <- sapply(1:nrow(data), fmtfun, x = data, fmt = fmt_data, con = fid)
    # Close file connection
    close(fid)

    # Invisible return of the data.frame written to the text connection
    invisible(x)
}

#' Data Output
#'
#' Generic method to write data to CSV. By default this method
#' falls back to \code{utils::write.csv(...)}.
#'
#' @param ... arguments passed to generic method.
#'
#' @seealso foehnix()
#'
#' @export
write.csv <- function(...) UseMethod("write.csv")

#' @export
write.csv.default <- function(...) utils::write.csv(...)


#' Get Estimated Mixture Model Coefficients
#'
#' Returns the estimated coefficients of a \code{\link[foehnix]{foehnix}}
#' mixture model for both, the components and the concomitant model (if specified).
#'
#' @param object a \code{\link[foehnix]{foehnix}} object.
#' @param type character, either \code{"parameter"} \code{"coefficients"}, see
#'        'Details' section.
#' @param ... additional arguments, ignored.
#' @return Returns a \code{coef.foehnix} object with the estimated
#' coefficients.
#' @details Returns the coefficients of the mixture model.  If \code{type =
#' "parameter"} the parameters of the mixture model components are returned on
#' the 'true' scale (parameter scale), else on the link scale (as used for
#' optimization).  For each component the location and scale parameters are
#' returned.  If a concomitant model has been specified, the coefficients of
#' the (possibly regularized) logistic regression model are returned as well
#' (concomitant model).
#'
#' @seealso \code{\link[foehnix]{foehnix}}, \code{\link[foehnix]{iwls_logit}}.
#'
#' @author Reto Stauffer
#' @export
coef.foehnix <- function(object, type = "parameter", ...) {

    # One of the two types: parameter (destandardized if required),
    # or coefficient (standardized coefficients if standardized was TRUE).
    type <- match.arg(type, c("parameter", "coefficient"))
    if ( type == "parameter" ) {
        rval <- rbind(matrix(c(object$coef$mu1, exp(object$coef$logsd1),
                               object$coef$mu2, exp(object$coef$logsd2)), ncol = 1,
                             dimnames = list(c("mu1", "sigma1", "mu2", "sigma2"), NULL)),
                      object$coef$concomitants)
    } else {
        rval <- rbind(matrix(c(object$coef$mu1, object$coef$logsd1,
                               object$coef$mu2, object$coef$logsd2), ncol = 1,
                             dimnames = list(c("mu1", "logsd1", "mu2", "logsd2"), NULL)),
                      object$coef$concomitants)
    }
    rval <- stats::setNames(as.vector(rval), rownames(rval))
    
    # Appending some attributes and a new class
    attr(rval, "concomitants") <- ! is.null(object$coef$concomitants)
    attr(rval, "formula")      <- object$formula
    attr(rval, "family")       <- object$control$family
    class(rval) <- c("coef.foehnix", class(rval))
    rval
}

#' @export
coefficients.foehnix <- function(object, type = "parameter", ...)
    coef.foehnix(object, type = type, ...)

#' @export
print.coef.foehnix <- function(x, ...) {
    cat("Coefficients of foehnix model\n")
    cat(sprintf("Model formula:           %s\n",
                paste(as.character(attr(x, "formula"))[c(2,1,3)], collapse = " ")))
    print(attr(x, "family"))
    if ( ! attr(x, "concomitants") ) {
        cat("No concomitant model in use\n")
    }
    cat("\nCoefficients\n")
    print(structure(as.numeric(x), names = names(x)))
}


#' @usage
#' # Model formula
#' formula(x, ...)
#' @rdname foehnix
#' @export
formula.foehnix <- function(x, ...) x$formula


#' @usage
#' # Model/object summary
#' summary(object, eps = 1e-4, detailed = FALSE, ...)
#' @rdname foehnix
#' @export
summary.foehnix <- function(object, eps=1e-4, detailed = FALSE, ...) {

    rval <- list()
    rval$call     <- object$call
    rval$inflated <- object$inflated
    rval$prob     <- object$prob
    rval$coef     <- coef(object, type = "parameter")

    # Posteriors
    separation <- matrix(NA, ncol = 4, nrow = 2,
                         dimnames = list(c("Component 1 (foehn)", "Component 2 (no foehn)"),
                                         c("prior", "size", "post>0", "ratio")))
    priorfun <- function(x) { x <- mean(x, na.rm = TRUE); c(x, 1 - x) }
    sizefun  <- function(x) { N <- sum(!is.na(x)); x <- sum(x >= 0.5, na.rm = TRUE); c(x, N - x) }
    postfun  <- function(x, eps) { x <- na.omit(x); c(sum(x > eps), sum((1 - x) > eps)) }

    # Analog to flexmix summary
    separation[, "prior"]   <- priorfun(object$optimizer$prob) # mean prior probability
    separation[, "size"]    <- sizefun(object$optimizer$post)  # on posterior probability
    separation[, "post>0"]  <- postfun(object$optimizer$post, eps = eps) # on posterior prob
    separation[, "ratio"]   <- separation[, "size"] / separation[, "post>0"]

    # Optimizer statistics
    rval$separation <- separation # cluster summary (separation)
    rval$filter_obj <- object$filter_obj
    rval$time       <- object$time
    rval$logLik     <- logLik(object)
    rval$AIC        <- AIC(object)
    rval$BIC        <- BIC(object)
    rval$edf        <- edf(object)
    rval$n.iter     <- object$optimizer$n.iter
    rval$maxit      <- object$optimizer$maxit
    rval$detailed   <- detailed
    rval$converged  <- object$optimizer$converged

    # Appending concomitant model
    rval$ccmodel    <- object$optimizer$ccmodel

    class(rval) <- "summary.foehnix"
    return(rval)
}

#' @export
print.summary.foehnix <- function(x, ...) {

    # Model call
    cat("\nCall: "); print(x$call)

    sum_na <- sum(is.na(x$prob$flag))
    sum_0  <- sum(x$prob$flag == 0, na.rm = TRUE)
    sum_1  <- sum(x$prob$flag == 1, na.rm = TRUE)

    mean_occ  <- 100 * sum(x$prob$prob >= .5, na.rm = TRUE) / sum(!is.na(x$prob$flag))
    mean_n    <- sum(!is.na(x$prob$flag))
    mean_prob <- 100 * mean(x$prob$prob[!is.na(x$prob$flag)])

    # Additional information about the data/model
    if (x$inflated) {
        cat(sprintf("\nNumber of observations (total) %8d (%d due to inflation)\n",
                    nrow(x$prob), x$inflated))
    } else {
        cat(sprintf("\nNumber of observations (total) %8d (no inflation)\n",
                    nrow(x$prob), x$inflated))
    }
    cat(sprintf("Removed due to missing values  %8d (%3.1f percent)\n",
                sum_na, sum_na / nrow(x$prob) * 100))
    cat(sprintf("Outside defined wind sector    %8d (%3.1f percent)\n", 
                sum_0, sum_0 / nrow(x$prob) * 100))
    cat(sprintf("Used for classification        %8d (%3.1f percent)\n",
                sum_1, sum_1 / nrow(x$prob) * 100))
    cat(sprintf("\nClimatological foehn occurance %.2f percent (on n = %d)\n", mean_occ, mean_n))
    cat(sprintf("Mean foehn probability %.2f percent (on n = %d)\n", mean_prob, mean_n))

    cat(sprintf("\nLog-likelihood: %.1f, %d effective degrees of freedom\n",   x$logLik, x$edf))
    cat(sprintf("Corresponding AIC = %.1f, BIC = %.1f\n", x$AIC, x$BIC))
    cat(sprintf("Number of EM iterations %d/%d (%s)\n", x$n.iter, x$maxit,
                ifelse(x$converged, "converged", "not converged")))
    if ( x$time < 60 ) {
        cat(sprintf("Time required for model estimation: %.1f seconds\n", x$time))
    } else {
        cat(sprintf("Time required for model estimation: %.1f minutes\n", x$time / 60))
    }

    # If "detailed" has been set to "TRUE" we will also print the
    # test statistics of the coefficients of the mixture model.
    if ( x$detailed ) {
        # Cluster summary/ratio
        # Shows mean prior probability, the number of observations assigned to the
        # corresponding clusters, the number of observations where the probability
        # exceeds eps (with a default of eps = 10−4), and the ratio of the latter two
        # numbers. For well-seperated components, a large proportion of observations with
        # non-vanishing posteriors pnk should also be assigned to the corresponding
        # cluster, giving a ratio close to 1.
        cat("\nCluster separation (ratios close to 1 indicate\nwell separated clusters):\n")
        print(round(x$separation, 2))

        # If there is a concomitant model: show estimated coefficients and
        # z statistics.
        if ( ! is.null(x$ccmodel) ) {
            cat("\n---------------------------------\n")
            summary(x$ccmodel)
        }
    } else {
        cat("\nUse summary(object, detailed = TRUE) to get additional test statistics.\n")
    }
}


#' Convert input "windsector" to list if needed
#'
#' Converts user inputs to list. Wind sector information is used
#' to highlight specific wind sectors on the plots (e.g.,
#' \code{\link[foehnix]{tsplot}}, \code{\link[foehnix]{windrose}}).
#'
#' @param x \code{NULL}, \code{list}, \code{matrix}, or \code{data.frame}.
#'      see 'Details' for more information.
#' @return Returns a \code{list} object with the windsector information as used
#' by the different functions and methods of the \code{foehnix} package.
#'
#' @details Some foehnix functions allow to specify custom wind sectors. This
#' function is a convenience function which takes inputs in different formats
#' (e.g., as \code{matrix}, \code{data.frame}, ...) and converts the
#' information in the format the foehnix functions expect wind sector
#' definitions.
#'
#' @return Returns \code{NULL} (if input was \code{NULL}) or a named or unnamed
#' with one or multiple entries. Each entry is a numeric vector of length two
#' and specifies a wind sector.
#'
#' @examples
#' # No wind sector (NULL) simply returns NULL
#' foehnix:::windsector_convert(NULL)
#'
#' # Input can also be:
#' # - unnamed list
#' # - a matrix
#' # - a data.frame without row names
#' foehnix:::windsector_convert(matrix(c(10, 30, 90, 140), byrow = TRUE, ncol = 2))
#' foehnix:::windsector_convert(list(c(10, 30), c(90, 140)))
#' foehnix:::windsector_convert(data.frame(from = c(30, 90), to = c(30, 140)))
#'
#' # Or named objects can be used
#' # - named list
#' # - matrix with rownames
#' # - data.frame with rownames
#' # If names are set these names will be used to label the
#' # highlighted wind sectors.
#' foehnix:::windsector_convert(matrix(c(10, 30, 90, 140), byrow = TRUE, ncol = 2,
#'                          dimnames = list(c("A", "B"), c("from", "to"))))
#' foehnix:::windsector_convert(list(A = c(10, 30), B = c(90, 140)))
#' foehnix:::windsector_convert(structure(data.frame(from = c(30, 90), to = c(30, 140)),
#'                                        row.names = c("foo", "bar")))
#' 
#' @author Reto Stauffer
windsector_convert <- function(x) {
    # Check input
    stopifnot(inherits(x, c("NULL", "list", "matrix", "data.frame")))
    if (inherits(x, c("NULL", "list"))) return(x)
    # If matrix convert to list
    if (inherits(x, "data.frame")) x <- as.matrix(x)
    hadnames <- !is.null(rownames(x))
    x <- as.list(as.data.frame(t(x)))
    if (!hadnames) names(x) <- NULL
    return(x)
}



