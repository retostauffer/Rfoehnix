# -------------------------------------------------------------------
# - NAME:        plot.foehnix.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-16
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-17 20:11 on marvin
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Plot routine for foehnix classes.
# -------------------------------------------------------------------
plot.foehnix <- function(x, which = NULL, ...) {

    # Define plot type
    allowed <- c("loglik","loglikcontribution", "coef", "hist")
    if ( is.null(which) ) {
        which <- allowed
    } else if ( inherits(which, c("integer", "numeric")) ) {
        which <- allowed[as.integer(which)]
    } else {
        which <- match.arg(which, allowed, several.ok = TRUE)
    }

    # Keep user params
    hold <- par(no.readonly = TRUE); on.exit(par(hold))
    if ( length(which) > 1 ) par(ask = TRUE)

    flagging <- function(x, log = FALSE) {
        at <- if ( log ) log(1L:length(x)) else 1L:length(x)
        points(at, x, pch = 1, cex = 0.5,
               col = c(1, as.integer(diff(x) > 0) + 2))
    }

    # Plotting likelihood path if requested
    if ( "loglik" %in% which ) {
        ll <- x$optimizer$loglikpath
        ylim <- range(ll) + c(0, 0.1) * diff(range(ll))
        # Plot
        matplot(log(1L:nrow(ll)), ll, ylim = ylim, type = "l",
                ylab = "log-likelihood sum",
                xlab = "log(EM iteration)",
                main = "foehnix log-likelihood path")
        for ( i in 1:ncol(ll) ) flagging(ll[,i], log = TRUE)
        tmp <- 1L:ncol(ll)
        legend("top", ncol = max(tmp), lty = tmp, col = tmp,
               legend = colnames(ll))
    }

    # Plotting likelihood path if requested
    if ( "loglikcontribution" %in% which ) {
        ll <- x$optimizer$loglikpath
        for ( i in 1:ncol(ll) ) ll[,i] <- ll[,i] - ll[1,i]
        ylim <- range(ll) + c(0, 0.1) * diff(range(ll))
        # plot
        matplot(log(1L:nrow(ll)), ll, ylim = ylim, type = "l",
                ylab = "log-likelihood contribution",
                xlab = "log(EM iteration)",
                main = "foehnix log-likelihood contribution")
        for ( i in 1:ncol(ll) ) flagging(ll[,i], log = TRUE)
        tmp <- 1L:ncol(ll)
        legend("top", ncol = max(tmp), lty = tmp, col = tmp,
               legend = colnames(ll))
    }

    # Path of estimated coefficients
    if ( "coef" %in% which ) {
        # Extract path
        path <- x$optimizer$coefpath

        # Model without concomitant variables
        if ( is.null(x$optimizer$ccmodel) ) {
            ylim <- range(path) + c(0, 0.1) * diff(range(path))
            matplot(log(1L:nrow(path)), path, ylim = ylim, type = "l",
                    ylab = "coefficient (components)",
                    xlab = "log(EM iteration)",
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
            ylim <- range(path[,idx_comp]) + c(0, 0.1) * diff(range(path[,idx_comp]))
            matplot(log(1L:nrow(path)), path[,idx_comp], ylim = ylim, type = "l",
                    ylab = "coefficient (components)",
                    xlab = "log(EM iteration)",
                    main = "coefficient path (components)")
            tmp <- 1L:length(idx_comp)
            legend("top", ncol = max(tmp), lty = tmp, col = tmp,
                   legend = colnames(path)[idx_comp])

            # Concomitant model
            idx_cc <- which(grepl("^cc\\..*$", names(path)))
            ylim <- range(path[,idx_cc]) + c(0, 0.1) * diff(range(path[,idx_cc]))
            matplot(log(1L:nrow(path)), path[,idx_cc], ylim = ylim, type = "l",
                    ylab = "concomitant coefficients (standardized)",
                    xlab = "log(EM iteration)",
                    main = "coefficient path (concomitants)")
            tmp <- 1L:length(idx_cc)
            legend("top", ncol = max(tmp), lty = tmp, col = tmp,
                   legend = colnames(path)[idx_cc])
            par(holdx)
        }

    }

    # Conditional histogram plot
    if ( "hist" %in% which ) {

        # Create response vector
        hold_opt <- options("na.action"); options(na.action = "na.pass")
        y <- model.response(model.frame(x$formula, x$data))

        # Combine response vector with estimated probabilities
        tmp <- data.frame(y = as.numeric(y), prob = as.numeric(x$prob))

        # If wind filter is used, set posterior probability to
        # 0 for all observations not inside the filter (they have not
        # been used for modelling as they are not assumed to show
        # any foehn).
        idx_wind <- windfilter_get_indizes(x$data, x$windfilter)
        if ( ! is.null(idx_wind) ) tmp <- tmp[idx_wind,]

        # Remove missing values
        tmp <- na.omit(tmp)

        # If left/right censoring/truncation has been specified:
        if ( has.left(x$control$family) )  tmp$y <- pmax(x$control$family$left,  tmp$y)
        if ( has.right(x$control$family) ) tmp$y <- pmin(x$control$family$right, tmp$y)

        # Plot if we have data
        if ( nrow(tmp) > 0 ) {
            par(mfrow = c(1,2))
            # Position where to draw the density
            at   <- seq(min(tmp$y), max(tmp$y), length = 501)

            bk   <- seq(min(tmp$y), max(tmp$y), length = 50)
            h1   <- hist(y[which(x$prob <  .5)], plot = FALSE, breaks = bk)
            h2   <- hist(y[which(x$prob >= .5)], plot = FALSE, breaks = bk)
            ylim <- c(0, max(h1$density, h2$density, na.rm = TRUE))
            xlim <- range(bk)

            # Plotting conditional component 1 histogram
            plot(h1, freq = FALSE, xlim = xlim, ylim = ylim,
                 main = "Conditional Histogram\nComponent 1",
                 border = "gray50", xlab = expression(paste("y[",pi < 0.5,"]")))
            lines(at, x$control$family$d(at, x$coef$mu1, exp(x$coef$logsd1)), col = 2, lwd = 2)

            # Plotting conditional component 2 histogram
            plot(h2, freq = FALSE, xlim = xlim, ylim = ylim,
                 main = "Conditional Histogram\nComponent 2",
                 border = "gray50", xlab = expression(paste("y[",pi >= 0.5,"]")))
            lines(at, x$control$family$d(at, x$coef$mu2, exp(x$coef$logsd2)), col = 2, lwd = 2)

        }
    }

}

