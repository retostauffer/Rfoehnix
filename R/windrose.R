# -------------------------------------------------------------------
# - NAME:        windrose.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-16
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2019-01-16 21:41 on marvin
# -------------------------------------------------------------------

#' Windrose Plot
#'
#' Plotting a windrose using meteorological wind direction and
#' wind speed (or gust speed).
#'
#' @seealso windrose.default windrose.foehnix
#'
#' @export
windrose <- function(x, ...) UseMethod("windrose")


#' foehnix Mixture Model Windrose Plot
#'
#' Windrose plot based on a \code{\link{foehnix}} mixture model object.
#'
#' @param x object of class \code{\link{foehnix}}, see 'Details' section.
#' @param type \code{NULL} or character, one of \code{density} or \code{histogram}.
#'        If \code{NULL} both types will be plotted.
#' @param which \code{NULL} or character, one of \code{unconditional}, \code{nofoehn},
#'        or \code{foehn}. If \code{NULL} all three will be plotted.
#' @param ddvar character, name of the column in the training data set which contains
#'        the wind direction information.
#' @param ffvar character, name of the column in the training data set which contains
#'        the wind speed (or gust speed) data.
#' @param mfcol integer, number of columns of subplots.
#' @param maxpp integer (\code{>0}), maximum plots per page. Not all plots fit on one
#'        page the script loops trough.
#'
#' @details Allows to draw windrose plots from \code{\link{foehnix}} mixture model
#' object or a set of observations. If input \code{x} to \code{\link{windrose}} is
#' an object of class \code{foehnix} (as returned by \code{\link{foehnix}}) the
#' data set which the classification is based on is used to plot the windrose.
#' If inputs \code{dd} and \code{ff} are given (univariate \code{zoo} time series
#' objects or \code{numeric} vectors) windrose plots can be plotted for observations
#' without the need of a \code{foehnix} object.
#'
#' Two \code{type}s are available: circular density plots and circular
#' histograms. If the input argument \code{x} is a \code{foehnix} object an additional
#' input argument \code{which} is available to specify the subset which should be used
#' to create the windrose plots. The following inputs are allowed:
#'
#' \itemize{
#'    \item \code{which = "unconditional"}: unconditional windrose (all observations
#'          of the data set used to estimate the \code{\link{foehnix}} model).
#'    \item \code{which = "nofoehn"}: windrose of all observations which have not been
#'          classified as foehn (probability \code{< 0.5}).
#'    \item \code{which = "foehn"}: windrose of all observations classified as foehn events
#'          (probability \code{>= 0.5}).
#'    \item \code{which = NULL}: all three subsets will be plotted (individual
#'          windroses).
#' }
#' Specific combinations can be specified by calling the windrose function with e.g.,
#' \code{type = "histogram"} and \code{which = c("foehn", "nofoehn")} (will show
#' histograms for foehn and no foehn events), or \code{type = NULL} and \code{which = "foehn"}
#' (will show density and histogram plot for foehn events).
#' By default (\code{type = NULL}, \code{which = NULL}) all combinations will be plotted.
#'
#' If \code{dd} and \code{ff} are set only the \code{type} argument is available
#' (\code{type = "histogram"} or \code{type = "density"}).
#'
#' @examples
#' # Loading observations (data.frame), convert to zoo
#' data("ellboegen",  package = "foehnix")
#' data("sattelberg", package = "foehnix")
#' ellboegen  <- zoo(ellboegen[,-1],  as.POSIXct(ellboegen[,1],  origin = "1970-01-01"))
#' sattelberg <- zoo(sattelberg[,-1], as.POSIXct(sattelberg[,1], origin = "1970-01-01"))
#' 
#' # Modify sattelberg variable names (crest_ identifies Sattelberg
#' # observations, our crest station) and combine both data sets.
#' names(sattelberg) <- paste0("crest_", names(sattelberg))
#' data <- merge(ellboegen, sattelberg)
#' 
#' # Dry adiabatic temperature difference between 
#' # Sattelberg (data$crest_t) and Ellboegen (data$t) corrected by
#' # 1027/10 degrees.
#' data$diff_t <- data$crest_t + 10.27 - data$t
#'
#' # Before estimating a model: plot a wind rose for all observations
#' windrose(data$dd, data$ff, type = "histogram")
#' windrose(data$dd, data$ff, type = "density")
#'
#' # Estimate a foehnix foehn classification model
#' mod <- foehnix(diff_t ~ ff + rh, data = data, verbose = FALSE)
#'
#' # Plotting wind roses
#' windrose(mod)
#'
#' # Only density windrose for foehn events
#' windrose(mod, type = "density", which = "foehn")
#'
#' @rdname windrose
#' @author Reto Stauffer
#' @export
windrose.foehnix <- function(x, type = NULL, which = NULL, ddvar = "dd", ffvar = "ff",
                             mfcol = 2, maxpp = Inf, ...) {

    # Just to be on the very safe side.
    stopifnot(inherits(x, "foehnix"))

    # -------------------
    # Checking argument which
    allowed <- c("unconditional", "nofoehn", "foehn")
    if ( is.null(which) ) which <- allowed
    which <- match.arg(which, allowed, several.ok = TRUE)

    # Checking input type
    allowed <- c("density", "histogram")
    if ( is.null(type) ) type <- allowed
    type <- match.arg(type, allowed, several.ok = TRUE)

    # Extend inputs which/type, create list of windrose plots to show.
    tmp <- expand.grid(type = type, which = which)
    which <- sprintf("%s_%s", tmp$type, tmp$which)
    rm(type)

    # -------------------
    # Check if the 'ddvar' (wind direction variable) is in the data set.
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

    # -------------------
    # Check if the 'ffvar' (wind speed variable) is in the data set.
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

    # Combine dd, ff, probabilities, and start plotting.
    data <- na.omit(merge(dd, ff, x$prob)); names(data)[ncol(data)] <- "prob"
    if ( nrow(data) == 0 )
        stop("No data left after combining \"dd\", \"ff\", and \"foehn probabilities\".")

    # Else start plotting
    hold <- par(no.readonly = TRUE); on.exit(par(hold))

    # Number of rows and columns needed
    if ( length(which) > 1 ) {
        maxpp <- min(maxpp, length(which))
        mfcol = max(ceiling(maxpp / 2), mfcol)
        mfrow = ceiling(maxpp / mfcol)
        if ( mfrow == 2 & mfcol == 1 ) { mfrow <- 1; mfcol = 2; }
        par(mfrow = c(mfrow, mfcol))
    } else { mfrow = 1; mfcol = 1; }

    if ( (mfcol * mfrow) < length(which) ) par(ask = TRUE)

    # Plot type = "density"
    if ( "density_unconditional" %in% which )
        with(data,                      windrose(dd, ff, hue = c(10, 130),
                       main = expression("Unconditional")))
    if ( "density_nofoehn" %in% which )
        with(subset(data, prob < 0.5),  windrose(dd, ff, hue = c(100, 180),
                       main = expression(paste("No Foehn [",pi < 0.5,"]"))))
    if ( "density_foehn" %in% which )
        with(subset(data, prob >= 0.5), windrose(dd, ff, hue = c(-20, 30),
                       main = expression(paste("Foehn [",pi >= 0.5,"]"))))
    # Plot type = "histogram"
    if ( "histogram_unconditional" %in% which )
        with(data,                      windrose(dd, ff, hue = c(10, 130), type = "histogram",
                       main = expression("Unconditional")))
    if ( "histogram_nofoehn" %in% which )
        with(subset(data, prob <  0.5), windrose(dd, ff, hue = c(100, 180), type = "histogram",
                       main = expression(paste("No Foehn [",pi < 0.5,"]"))))
    if ( "histogram_foehn" %in% which )
        with(subset(data, prob >= 0.5), windrose(dd, ff, hue = c(-20, 30), type = "histogram",
                       main = expression(paste("Foehn [",pi >= 0.5,"]"))))


}



#' Get Count Matrix for Windrose Plot
#'
#' The \code{\link{windrose}} method provides one windrose/wind count
#' plot type. This function returns a matrix with counts for different
#' multivariate bins (binning along wind direction dd and wind speed ff).
#'
#' @param x data object of type zoo or data.frame. Needs to contain
#'        at least the two columns \code{dd} (meteorological wid direction
#'        in degrees, \code{]0, 360[}) and \code{ff} with wind speed
#'        (range >= 0).
#' @param dd.breaks numeric vector with breaks along wind direction.
#'        Default is \code{seq(0, 360, by = 30)}.
#' @param ff.breaks numeric vector with breaks along wind speed.
#'        default is \code{pretty(x$ff)}.
#' @return Returns a matrix of dimension \code{length(dd.breaks) x length(ff.breaks)}
#'         with counts \code{>= 0}.
windrose_get_counts <- function(x, dd.breaks = seq(0, 360, by = 30),
                                   ff.breaks = pretty(x$ff)) {
    x <- as.matrix(xtabs(~ cut(x$dd, dd.breaks, include.lowest = TRUE) +
                           cut(x$ff, ff.breaks, include.lowest = TRUE)))
    x[which(is.na(x), arr.ind = TRUE)] <- 0
    x
}


#' Get Density Matrix for Windrose Plot
#'
#' The \code{\link{windrose}} method provides one 'density wind rose'
#' method (the classical wind rose plot).
#' This function returns a matrix with densities for different
#' multivariate bins (binning along wind direction dd and wind speed ff).
#'
#' @param x data object of type zoo or data.frame. Needs to contain
#'        at least the two columns \code{dd} (meteorological wid direction
#'        in degrees, \code{]0, 360[}) and \code{ff} with wind speed
#'        (range >= 0).
#' @param dd.breaks numeric vector with breaks along wind direction.
#'        Default is \code{seq(0, 360, by = 30)}.
#' @param ff.breaks numeric vector with breaks along wind speed.
#'        default is \code{pretty(x$ff)}.
#' @return Returns a matrix of dimension \code{length(dd.breaks) x length(ff.breaks)}
#'         with densities \code{>= 0}.
windrose_get_density <- function(x, dd.breaks = seq(0, 360, by = 30),
                                    ff.breaks = pretty(x$ff)) {
    N <- nrow(x)
    x <- as.matrix(xtabs(~ cut(x$dd, dd.breaks, include.lowest = TRUE) +
                           cut(x$ff, ff.breaks, include.lowest = TRUE)))
    x[which(is.na(x), arr.ind = TRUE)] <- 0
    t(apply(x, 1, cumsum)) / N
}


#' Get Color Matrix and Legend for Windrose Plot
#'
#' The \code{\link{windrose}} method provides one windrose/wind count
#' plot type. Based on the count matrix (\code{\link{windrose_get_counts})
#' this function returns a matrix with hex colors (with alpha channel)
#' and a data.frame used for the legend.
#'
#' @param x 2-D matrix with counts (see \code{\link{windrose_get_counts}}).
#' @param col single hex color, or a vector of hex colors. Alpha channel
#'        will be removed.
#' @param p numeric value, power parameter for non-linear color transformation.
#' @param ncol integer, if \code{col} is a single value the color will be repeated
#'        \code{ncol} times. Default is \code{50L}.
#' @return Returns a list with two elements: \code{colormatrix} of the same
#' dimension as \code{x} with colors for the different bins based on the
#' counts, and \code{legend} with levels and colors for the color legend of the
#' plot.
#'
#' @importFrom grDevices rgb
windrose_get_cols <- function(x, col, p = 1, ncol = 50L) {
    # Extend colors if needed
    if ( length(col) == 1 ) col <- rep(col, ncol)
    # Remove alpha
    col <- substr(col, 0, 7)
    # Calculate new colors. 
    fun <- function(x, col, p) {
        alpha <- as.integer(round((x / max(x))^p * 99 + 1))
        alpha <- substr(rgb(1, 1, 1, 1:100 / 100), 8, 10)[alpha]
        x <- round((x / max(x)) / max(x / max(x)) * (length(col) - 1) + 1)
        x <- sprintf("%s%s", col[x], alpha)
    }
    colormatrix <- matrix(fun(x, col, p), ncol = ncol(x), dimnames = dimnames(x))

    # Create legend element
    leg <- data.frame(level = pretty(range(x), 5))
    leg$color <- fun(leg$level, col, p)

    list(colormatrix = colormatrix, legend = leg)
}



#' Default Wind Rose Plot
#'
#' Windrose plot, can handle two type of plots (see \code{type}) using
#' the same information.
#'
#' @param dd zoo object with meteorological wind directions (\code{]0, 360[},
#'        0/360 is 'wind from north', 90 'wind from east', 180 'wind from south',
#'        and 270 'wind from west'.
#' @param ff zoo object with sind speed data (\code{>= 0}).
#' @param interval numeric, a fraction of 360. If 360 cannot be devided by
#'        \code{interval} without a rest the script will stop. Interval
#'        for the segments along the wind direction \code{dd}. Default \code{10}.
#' @param type character, one of \code{density} or \code{histogram}, controls
#'        the plot type.
#' @param windsector TODO: currently unused.
#' @param main character, title. If no title is set a default one will be shown.
#' @param hue numeric vector of length 1 or two (for \code{type = "density"}
#'        two are used, for \code{type = "histogram"} only the first one will be
#'        used at the moment). Hue(s) for \code{colorspace::heat_hcl(...)}.
#' @param power numeric \code{>0}, power parameter for the alpha channel if 
#'        \code{type = "histogram"}. If \code{1} the alpha channel is linear
#'        for the whole range of \code{ff}, values \code{!= 1} change the
#'        alpha behaviour.
#' @param dd a vector with meteorological wind directions within \code{[0-360]},
#'        see 'Details' section.
#' @param ff a vector with wind speed measurements within \code{[0,Inf]},
#'        see 'Details' section.
#'
#' @rdname windrose
#' @author Reto Stauffer
#' @export
windrose.default <- function(dd, ff, interval = 10, type = "density", 
    windsector = NULL, main = NULL, hue = c(10,100), power = .5) {

    # Check "dd" values
    if ( ! length(dd) == length(ff) )
        stop("Unequal length of wind speed and wind direction values!")
    if ( min(dd, na.rm = TRUE) < 0 | max(dd, na.rm = TRUE) > 360 )
        stop("Wind direction not within the expected range of [0, 360]!")
    if ( min(ff, na.rm = TRUE) < 0 )
        stop("Got wind speeds below 0!")

    # Plot type
    type = match.arg(type, c("density", "histogram"))

    # Stop if alpha is strange
    if ( length(power) != 1 ) stop("\"power\" has to be of length 1.")
    if ( power[1L] < 0 ) stop("\"power\" has to be > 0")

    # Interval check
    if ( ! floor(360/interval)*interval == 360 ) stop("interval wrong")

    # Both, dd and ff, have to be zoo objects.
    stopifnot(inherits(dd, "zoo"))
    stopifnot(inherits(ff, "zoo"))

    # Combine dd, ff, probabilities, and start plotting.
    data <- na.omit(merge(dd, ff))
    if ( nrow(data) == 0 ) stop("No data left after combining \"dd\" and \"ff\".")

    # Breaks for classification
    dd.breaks <- seq(-interval / 2, 360, by=interval)
    ff.breaks <- pretty(ff)

    # ----------------
    # Prepare data for the plots


    # Picking some colors
    cols <- rev(colorspace::heat_hcl(length(ff.breaks) - 1, h= hue, c. = c(60, 10),
                                     l = c(25, 95), power = c(0.7,2)))
    # Type "density"
    if ( type == "density") {

        tab <- windrose_get_density(data, dd.breaks, ff.breaks)
        # Breaks of densities
        density.breaks <- pretty(tab)

        # X and Y limits
        xlim <- ylim <- max(density.breaks) * c(-1, 1)
        xlim <- xlim * c(1, 1.5)
        # Title
        if ( is.null(main) ) main <- "Windrose"

    # Type "histogram"
    } else {

        # Create matrizes with counts
        counts <- windrose_get_counts(data, dd.breaks, ff.breaks)
        # Convert counts into colors
        # Picking some colors
        if ( is.null(hue) ) hue <- c(-100,80)

        # X and Y limits
        xlim <- ylim <- max(ff.breaks) * c(-1, 1)
        xlim <- xlim * c(1, 1.5)
        
        # Picking some colors
        tab  <- windrose_get_cols(counts, tail(cols,1L), p = power)

        # Title
        if ( is.null(main) ) main <- "Windrose Histogram"
    }

    # ----------------
    # Base plot
    #lim <- max(abs(tab), na.rm = TRUE) * c(-1.05, 1.05)
    plot(0, type = "n", asp = 1, bty = "n", main = NA,
         xlim = xlim, xaxt = "n", xlab = NA,
         ylim = ylim, yaxt = "n", ylab = NA)
    mtext(side = 3, at = 0, cex = 1.2, font = 2, line = .1, main)

    # If secotr is set: colorize
    # TODO: not yet implemented
    if ( ! is.null(windsector) ) {
        polygon( c(0,windsector[1,1], windsector[2,1], 0),
                 c(0,windsector[1,2], windsector[2,2], 0), col = "gray90", border = NA)
    }

    # Adding polygons (density)
    if ( type == "density" ) {
        for ( i in ncol(tab):1 ) { 
            tmp <- (-1) * ddff2uv(dd.breaks + interval / 2, c(tab[,i], tab[1,i]))
            polygon(tmp$u, tmp$v, col = cols[i], border = NA)
        }
    # Plot type "histogram"
    } else {
        # ----------------
        n <- round(diff(dd.breaks[1L:2L]))
        # Drawing segments
        for ( d in 2:length(dd.breaks) ) {
            for ( f in 2:length(ff.breaks) ) {
                tmp <- rbind(ddff2uv(seq(dd.breaks[d-1], dd.breaks[d], length = n),
                                     rep(ff.breaks[f-1], n)),
                             ddff2uv(seq(dd.breaks[d], dd.breaks[d-1], length = n),
                                     rep(ff.breaks[f], n)))
                tmp <- rbind(tmp, head(tmp,1)) *  -1
                polygon(tmp$u, tmp$v, col = tab$colormatrix[d-1,f-1], border = NA)
            }
        }
    }

    # Circles/axis
    if ( type == "density" ) {
        at  <- density.breaks
        fmt <- "%.2f"
    } else {
        at <- ff.breaks
        fmt <- ifelse(all(round(at) == at), "%.0f", "%.1f")
    }
    for ( d in at ) {
        # Add circle
        tmp <- ddff2uv(seq(0, 360), d)
        lines(tmp$u, tmp$v, col = "gray50", lty = 3)
        # Else add label
        if ( d <= 0 ) next
        tmp <- ddff2uv(45, d)
        text(tmp$u, tmp$v, sprintf(fmt, d), cex = .5)
    }
    lines(c(0,0), c(-1,1) * ifelse(type == "density", max(density.breaks), max(ff.breaks)))
    lines(c(-1,1) * ifelse(type == "density", max(density.breaks), max(ff.breaks)), c(0,0))

    # If windsector is set: draw windsector
    # TODO: Not yte implemented
    if ( ! is.null(windsector) ) {
        lines(c(0, windsector[1,1]), c(0, windsector[1,2]))
        lines(c(0, windsector[2,1]), c(0, windsector[2,2]))
    }

    # ----------------
    # Adding legend
    at <- if ( type == "density" ) max(density.breaks) else max(ff.breaks)
    at <- seq(0, at, length = ifelse(type == "density", ncol(tab), nrow(tab$legend)) * 2)
    x0 <- max(at, na.rm = TRUE) * 1.05
    x1 <- max(at, na.rm = TRUE) * 1.05 + diff(at[1:2])

    if ( type == "density" ) {
        for ( i in 1:ncol(tab) ) {
            y0 <- at[(i-1)*2 + 1]; y1 <- at[i*2]
            rect(x0, y0, x1, y1, col = cols[i])
            text(x1, (y0+y1)/2, colnames(tab)[i], pos = 4)
        }
    } else {
        for ( i in 1:nrow(tab$legend) ) {
            y0 <- at[(i-1)*2 + 1]; y1 <- at[i*2]
            rect(x0, y0, x1, y1,
                 col = tab$legend$color[i], border = "gray50")
            text(x1, (y0+y1)/2, sprintf("%d", tab$legend$level[i]), pos = 4)
        }
    }
    text(x0, -mean(tail(at, 2)), pos = 4, sprintf("N = %d", nrow(data)))

}
