# -------------------------------------------------------------------
# - NAME:        windrose.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-16
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-20 18:21 on marvin
# -------------------------------------------------------------------

#' Windrose Plot
#'
#' Plotting a windrose using meteorological wind direction and
#' wind speed (or gust speed).
#'
#' @param x \code{zoo} object or \code{data.frame} or \code{\link{foehnix}}
#'        mixture model object.
#' @param which character, one of \code{"unconditional"}, \code{"nofoehn"}, 
#'        \code{"foehn"}, or \code{"combined"}.
#' @param ddvar character, name of the wind direction varaible in \code{x} or as used
#'        for training the \code{\link{foehnix}} mixture model.
#' @param ffvar character, name of the wind speed varaible in \code{x} or as used
#'        for training the \code{\link{foehnix}} mixture model.
#' @param ... additional arguments, currently unused.
#'
#' @details TODO: not yet finished and/or tested!
windrose <- function(x, which = NULL, ddvar = "dd", ffvar = "ff", ...) UseMethod("windrose")

#' @rdname windrose
windrose.foehnix <- function(x, which = NULL, ddvar = "dd", ffvar = "ff", ...) {

    # First: checking argument "which"
    allowed <- c("unconditional", "nofoehn", "foehn", "combined")
    if ( is.null(which) ) which <- allowed
    if ( is.numeric(which) ) {
        which <- as.integer(which)
        if ( any(! which %in% seq_along(allowed)) )
            stop(paste("Nonsuitable input \"which\". Out of range. Allowed:",
                       sprintf("%s (or %s) or combinations of them.",
                               paste(sprintf("\"%s\"", allowed), collapse = ", "),
                               paste(seq_along(allowed), collapse = ", "))))
        which <- allowed[which]
    }
    if ( ! all(which %in% allowed) ) {
        stop(paste("Nonsuitable input \"which\". Out of range. Allowed:",
                   sprintf("%s (or %s) or combinations of them.",
                           paste(sprintf("\"%s\"", allowed), collapse = ", "),
                           paste(seq_along(allowed), collapse = ", "))))
    }

    # Checking dd
    if ( inherits(ddvar, "character") ) {
        if ( ! ddvar %in% names(x$data) ) {
            stop(paste(sprintf("Cannot find ddvar = \"%s\" in model data.", ddvar),
                       sprintf("Specify ddvar as one of \"%s\"", paste(names(x$data), collapse = ", ")),
                       "or provide a new univariate object of class \"zoo\" to provide the data.",
                       "For more information please read see ?windrose.foehnix."))
        } else {
            # Else picking ff from x$data
            dd <- x$data[,ddvar]
        }
    } else if ( ! inherits(ddvar, "zoo") ) {
        stop("Input ddvar has to be of class 'zoo' or 'character'.")
    }

    # Checking ff
    if ( inherits(ffvar, "character") ) {
        if ( ! ffvar %in% names(x$data) ) {
            stop(paste(sprintf("Cannot find ffvar = \"%s\" in model data.", ffvar),
                       sprintf("Specify ddvar as one of \"%s\"", paste(names(x$data), collapse = ", ")),
                       "or provide a new univariate object of class \"zoo\" to provide the data.",
                       "For more information please read see ?windrose.foehnix."))
        } else {
            # Else picking ff from x$data
            ff <- x$data[,ffvar]
        }
    } else if ( ! inherits(ddvar, "zoo") ) {
        stop("Input ddvar has to be of class 'zoo' or 'character'.")
    }

    # Combine probabilities, ff, and dd
    data <- na.omit(merge(x$prob, dd, ff))
    if ( nrow(data) == 0 )
        stop("Data sets do not match, no overlapping data found for probabilities of foehn, dd, and ff.")

    # Final step: combine dd, ff, probabilities, and start plotting.
    data <- na.omit(merge(dd, ff, x$prob)); names(data)[ncol(data)] <- "prob"
    if ( nrow(data) == 0 )
        stop("No data left after combining \"dd\", \"ff\", and \"foehn probabilities\".")

    # Else start plotting
    hold <- par(no.readonly = TRUE); on.exit(par(hold))
    mfrow = ceiling(length(which) / 2)
    mfcol = ceiling(length(which) / mfrow)
    par(mfrow = c(mfrow, mfcol))

    # Unconditional wind rose
    if ( "unconditional" %in% which )
        windrose(data$dd, data$ff, main = "Unconditional", hue = c(10,130))
    # No foehn windrose
    if ( "nofoehn" %in% which )
        with(subset(data, prob < 0.5),  windrose(dd, ff, hue = c(100, 180), main = "No Foehn"))
    if ( "foehn" %in% which )
        with(subset(data, prob >= 0.5), windrose(dd, ff, hue = c(-20, 30), main = "Foehn"))

    # 
    print("oh, a windrose for a foehnix object .... #TODO code it!")

}

#TODO: Very specific function (just a copy of the Luzern analysis).
# Usage needs to be changed drastically.
windrose.default <- function(dd, ff, interval = 10, circle = seq(0, 2, by = .05),
    windsector = NULL, main = NULL, hue = c(10,100)) {

    # Interval check
    if ( ! floor(360/interval)*interval == 360 ) stop("interval wrong")

    # Both, dd and ff, have to be univariate.
    if ( ! is.null(dim(dd)) | ! is.null(dim(ff)) )
        stop("Both, \"dd\" and \"ff\" have to be univariate.")

    if ( inherits(dd, "zoo") & inherits(ff, "zoo") ) {
        tmp <- na.omit(merge(dd, ff))
        if ( nrow(tmp) == 0 )
            stop("No data left after merging \"dd\" and \"ff\": no matching time indizes!")
        # Else split again
        dd <- tmp$dd; ff <- tmp$ff; rm(tmp)
    }

    # Convert to numeric
    dd <- try(as.numeric(dd)); if ( inherits(dd, "try-error") ) stop("Wrong input \"dd\".")
    ff <- try(as.numeric(ff)); if ( inherits(ff, "try-error") ) stop("Wrong input \"ff\".")

    # Combine dd and ff. If length does not match: stop.
    if ( ! length(dd) == length(ff) )
        stop("Length of \"dd\" values does not match length of \"ff\" values. Stop.")

    # Breaks for classification
    dd <- ifelse(dd > (360 - interval/2), dd - 360, dd)
    dd.breaks <- seq(-interval / 2, 360, by=interval)
    ff.breaks <- pretty(ff)

    tab <- xtabs( ~ cut(dd, dd.breaks, include.lowest = TRUE) +
                    cut(ff, ff.breaks, include.lowest = TRUE) )
    tab <- t(apply(tab, 1, cumsum)) / length(dd)

    rotate <- function(ff,dd) {
        x   <- (90 + dd) / 180 * pi
        mat <- matrix( c(sin(x),-cos(x),cos(x),sin(x)), ncol = 2)
        data <- matrix(c(0,ff), ncol = 2) %*% mat
        return(data)
    }

    # Drawing a circle
    drawCircle <- function(x) {
        tmp <- matrix(c(rep(x,361), seq(0,360)), ncol = 2, byrow = FALSE)
        tmp <- t(apply(tmp, 1, function(x) rotate(x[1L], x[2L])))
        lines(tmp[,1], tmp[,2], col = "gray", lty = 2)
    }

    # Title
    if ( is.null(main) ) main <- "Windrose"

    # Start plotting down here
    lim <- max(abs(tab), na.rm = TRUE)
    plot(0, type = "n", xlim = c(-1,1)*lim*1.05, ylim = c(-1,1)*lim*1.05,
         xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
         xlab = NA, ylab = NA, main = NA, asp = 1)
    mtext(side = 3, line = 2, main, font = 2, cex = 1.2)

    # If secotr is set: colorize
    if ( ! is.null(windsector) ) {
        polygon( c(0,windsector[1,1], windsector[2,1], 0),
                 c(0,windsector[1,2], windsector[2,2], 0), col = "gray90", border = NA)
    }

    # Adding wind direction labels
    lines(c(0,0), c(-lim,lim))
    lines(c(-lim,lim), c(0,0))
    axis(side = 1, at = 0, labels = "S", las = 1)
    axis(side = 2, at = 0, labels = "W", las = 1)
    axis(side = 3, at = 0, labels = "N", las = 1)
    axis(side = 4, at = 0, labels = "E", las = 1)

    if ( is.null(hue) ) hue <- c(-100,80)
    cols <- rev(colorspace::heat_hcl(ncol(tab), h= hue, c. = c(60, 10),
                                     l = c(25, 95), power = c(0.7,2)))

    dd <- dd.breaks[-1] - diff(dd.breaks[1:2])/2
    for ( i in ncol(tab):1 ) { 
        tmp <- t(apply(cbind(tab[,i], dd), 1, function(x) rotate(x[1L], x[2L])))
        tmp <- rbind(tmp,tmp[1,])
        polygon(tmp[,1], tmp[,2], col = cols[i], border = NA)
    }

    # Draw circles
    if ( is.null(circle) ) circle <- pretty(ff)
    for ( i in circle ) { if ( i == 0 ) next else drawCircle(i) }

    # If windsector is set: draw windsector
    if ( ! is.null(windsector) ) {
        lines(c(0, windsector[1,1]), c(0, windsector[1,2]))
        lines(c(0, windsector[2,1]), c(0, windsector[2,2]))
    }
    legend("bottomleft", bty = "n", legend = sprintf("N = %d", length(dd)))

    # Adding legend
    legend("topleft", fill = cols, legend = colnames(tab), bg = "white",
          title = "Wind speed [m/s]")
    box()

}
