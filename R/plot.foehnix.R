# -------------------------------------------------------------------
# - NAME:        plot.foehnix.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-16
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2019-01-31 08:16 on marvin
# -------------------------------------------------------------------


#' foehnix Model Assessment Plots
#' 
#' Visual representation \code{\link{foehnix}} model optimization.
#' 
#' @param x a \code{\link{foehnix}} mixture model object.
#' @param which \code{NULL} (default), character, character string,
#'         integer, or numeric. Allowed characters: \code{loglik},
#'         \code{loglikcontribution}, \code{coef}, and \code{hist}. If \code{which}
#'         is numeric/integer or a vector of numerics/integers the numbers
#'         correspond to \code{loglik} (\code{1}), \code{loglikpath} (\code{2}),
#'         or \code{coef} (\code{3}).
#' @param log logical, if \code{TRUE} the x-axis is shown on the log scale,
#'        else on the iteration scale.
#' @param ... additional arguments, unused.
#' @param ask boolean, default is \code{TRUE}. User will be asked to show the
#'        next figure if multiple figures are requested. Can be set to \code{FALSE}
#'        to overwrite the default.
#' 
#' @details
#' There are currently three different plot types.
#' \itemize{
#'     \item \code{"loglik"} shows the log-likelihood sum path trough
#'         the iterations of the EM algorithm for parameter estimation.
#'     \item \code{"loglikcontribution"} shows the log-likelihood
#'         contribution (initial value subtracted; all paths start
#'         with \code{0}).
#'     \item \code{coef} shows the development of the (standardized)
#'         coefficients during EM optimization. Parameters of the
#'         components are shown on the real scale, the coefficients
#'         of the concomitant model (if used) are shown on the
#'         standardized scale.
#' }
#'
#' @import graphics
#' @author Reto Stauffer
#' @export
plot.foehnix <- function(x, which = NULL, log = TRUE, ..., ask = TRUE) {

    # Define plot type
    allowed <- c("loglik","loglikcontribution", "coef", "hist")
    if ( is.null(which) ) {
        which <- allowed
    } else if ( inherits(which, c("integer", "numeric")) ) {
        which <- allowed[as.integer(which)]
    } else {
        which <- match.arg(tolower(which), allowed, several.ok = TRUE)
    }
    if(any(is.na(which))) stop("\"which\" argument not valid")

    # Keep user params
    hold <- par(no.readonly = TRUE); on.exit(par(hold))
    if ( length(which) > 1 & ask ) par(ask = TRUE)

    flagging <- function(x, log = FALSE) {
        at <- if ( log ) log(1L:length(x)) else 1L:length(x)
        points(at, x, pch = 1, cex = 1,
               col = c(1, as.integer(diff(x) > 0) + 2))
    }

    # Plotting likelihood path if requested
    if ( "loglik" %in% which ) {
        ll <- x$optimizer$loglikpath
        ylim <- range(ll) + c(0, 0.2) * diff(range(ll))
        # Plot
        lty <- c(2,3,1); col <- c("gray60", "gray60", 1); lwd = c(1,1,2)
        at <- if ( log ) log(1:nrow(ll)) else 1:nrow(ll)
        matplot(at, ll, ylim = ylim, type = "l",
                lwd = lwd, lty = lty, col = col,
                ylab = "log-likelihood",
                xlab = "log(EM iteration)",
                main = "foehnix log-likelihood path")
        for ( i in 1:ncol(ll) ) flagging(ll[,i], log = log)
        legend("top", ncol = length(lty), lty = lty, col = col,
               lwd = lwd, legend = colnames(ll))
    }

    # Plotting likelihood path if requested
    if ( "loglikcontribution" %in% which ) {
        ll <- x$optimizer$loglikpath
        for ( i in 1:ncol(ll) ) ll[,i] <- ll[,i] - ll[1,i]
        ylim <- range(ll) + c(0, 0.2) * diff(range(ll))
        # plot
        lty <- c(2,3,1); col <- c("gray60", "gray60", 1); lwd = c(1,1,2)
        at <- if ( log ) log(1:nrow(ll)) else 1:nrow(ll)
        matplot(at, ll, ylim = ylim, type = "l",
                lwd = lwd, lty = lty, col = col,
                ylab = "log-likelihood contribution",
                xlab = ifelse(log, "log(EM iteration)", "EM iteration"),
                main = "foehnix log-likelihood contribution")
        for ( i in 1:ncol(ll) ) flagging(ll[,i], log = log)
        legend("top", ncol = length(lty), lty = lty, col = col,
               lwd = lwd, legend = colnames(ll))
    }

    # Path of estimated coefficients
    if ( "coef" %in% which ) {
        # Extract path
        path <- x$optimizer$coefpath
        at   <- if ( log ) log(1:nrow(path)) else 1:nrow(path)

        # Model without concomitant variables
        if ( is.null(x$optimizer$ccmodel) ) {
            ylim <- range(path) + c(0, 0.1) * diff(range(path))
            matplot(log(1L:nrow(path)), path, ylim = ylim, type = "l",
                    ylab = "coefficient (components)",
                    xlab = ifelse(log, "log(EM iteration)", "EM iteration"),
                    main = "coefficient path (components)")
            tmp <- 1L:ncol(path)
            legend("top", ncol = max(tmp), lty = tmp, col = tmp,
                   legend = colnames(path))

        # Model with additional concomitants
        } else {
            holdx <- par(no.readonly = TRUE)
            par(mfrow = c(1,2))
            # Components
            idx_comp <- which(! grepl("^cc\\..*$", names(path)))
            lwd  <- rep(c(2,1), 2); lty = rep(c(1,2), 2); col = rep(c(2, 4), each = 2)
            ylim <- range(path[,idx_comp]) + c(0, 0.1) * diff(range(path[,idx_comp]))
            matplot(at, path[,idx_comp], ylim = ylim, type = "l",
                    lty  = lty, lwd = lwd, col = col,
                    ylab = "coefficient (components)",
                    xlab = ifelse(log, "log(EM iteration)", "EM iteration"),
                    main = "coefficient path (components)")
            legend("top", ncol = length(lty), lty = lty, col = col,
                   lwd = lwd, legend = colnames(path)[idx_comp])

            # Concomitant model
            idx_cc <- which(grepl("^cc\\..*$", names(path)))
            lwd <- c(2, rep(1, length(idx_cc)))
            col <- lty <- 1:length(idx_cc)
            ylim <- range(path[,idx_cc]) + c(0, 0.1) * diff(range(path[,idx_cc]))
            matplot(at, path[,idx_cc], ylim = ylim, type = "l",
                    lwd  = lwd, lty = lty, col = col,
                    ylab = "concomitant coefficients (standardized)",
                    xlab = ifelse(log, "log(EM iteration)", "EM iteration"),
                    main = "coefficient path (concomitants)")
            tmp <- 1L:length(idx_cc)
            legend("top", ncol = length(lty), lty = lty, col = col,
                   lwd = lwd, legend = gsub("^cc\\.", "", colnames(path)[idx_cc]))
            par(holdx)
        }

    }

    # Conditional histogram plot
    if ( "hist" %in% which ) {

        # Create response vector
        hold_opt <- options("na.action"); options(na.action = "na.pass")
        y <- model.response(model.frame(x$formula, x$data))

        # Combine response vector with estimated probabilities
        tmp <- data.frame(y    = as.numeric(y),
                          prob = as.numeric(x$prob$prob),
                          flag = as.numeric(x$prob$flag))

        # Remove missing values
        tmp <- na.omit(subset(tmp, tmp$flag == 1 & !is.na(y)))

        # If left/right censoring/truncation has been specified:
        if ( has.left(x$control$family) )  tmp$y <- pmax(x$control$family$left,  tmp$y)
        if ( has.right(x$control$family) ) tmp$y <- pmin(x$control$family$right, tmp$y)

        # Plot if we have data
        if ( nrow(tmp) > 0 ) {
            par(mfrow = c(1,2))
            # Position where to draw the density
            at   <- seq(min(tmp$y), max(tmp$y), length = 501)

            bk   <- seq(min(tmp$y), max(tmp$y), length = 50)
            h1   <- hist(tmp$y[which(tmp$prob <  .5)], plot = FALSE, breaks = bk, include.lowest = TRUE)
            h2   <- hist(tmp$y[which(tmp$prob >= .5)], plot = FALSE, breaks = bk, include.lowest = TRUE)
            ylim <- c(0, max(h1$density, h2$density, na.rm = TRUE))
            xlim <- range(bk)

            # Plotting conditional component 1 histogram
            plot(h1, freq = FALSE, xlim = xlim, ylim = ylim,
                 main = "Conditional Histogram\nComponent 1 (no foehn)",
                 border = "gray50", xlab = expression(paste("y[",pi < 0.5,"]")))
            lines(at, x$control$family$d(at, x$coef$mu1, exp(x$coef$logsd1)), col = 2, lwd = 2)

            # Plotting conditional component 2 histogram
            plot(h2, freq = FALSE, xlim = xlim, ylim = ylim,
                 main = "Conditional Histogram\nComponent 2 (foehn)",
                 border = "gray50", xlab = expression(paste("y[",pi >= 0.5,"]")))
            lines(at, x$control$family$d(at, x$coef$mu2, exp(x$coef$logsd2)), col = 4, lwd = 2)

        }
    }

}


