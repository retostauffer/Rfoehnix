

# Some global variable(s) to make R CMD check happy
utils::globalVariables(c("prob", "maxpp"))


#' Windrose Plot for foehnix Mixture Models and Observations
#'
#' foehnix Mixture Model Windrose Plot
#'
#' Windrose plot based on a \code{\link{foehnix}} mixture model object.
#'
#' @param x object of class \code{\link{foehnix}} if the windrose plot is applied
#'        to a model, or a vector of wind directions for \code{windrose.default}
#'        (see 'Details' section).
#' @param ... forwarded to \code{\link{windrose.default}}.
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
#' # Loading combined demo data set
#' data <- demodata("tyrol") # default
#'
#' # Before estimating a model: plot a wind rose for all observations
#' windrose(data$dd, data$ff, type = "histogram")
#' windrose(data$dd, data$ff, type = "density")
#'
#' # Estimate a foehnix foehn classification model
#' filter <- list(dd = c(43, 223), crest_dd = c(90, 270)) 
#' mod <- foehnix(diff_t ~ ff + rh, data = data,
#'                filter = filter, verbose = FALSE)
#'
#' # Plotting wind roses
#' windrose(mod)
#'
#' # Only density windrose for foehn events
#' windrose(mod, type = "density", which = "foehn")
#'
#' # Using custom names: by default wind direction is expected
#' # to be called 'dd', wind speed is expected to be called 'ff'.
#' # However, ddvar and ffvar allow to change that (only if
#' # windrose is called with a foehnix object as input).
#' # An example:
#' # - make a copy of data to data2
#' # - rename dd to winddir, crest_dd to crest_winddir
#' # - estimate the same foehnix model as above using the
#' #   new variable names
#' # - plot windrose with custom names for wind direction (winddir)
#' #   and wind speed (windspd).
#'
#' data2 <- data
#' names(data2)[which(names(data2) == "ff")]       <- "windspd"
#' names(data2)[which(names(data2) == "dd")]       <- "winddir"
#' names(data2)[which(names(data2) == "crest_dd")] <- "crest_winddir"
#' print(head(data2))
#' 
#' filter2 <- list(winddir = c(43, 223), crest_winddir = c(90, 270)) 
#' mod2 <- foehnix(diff_t ~ windspd + rh, data = data2,
#'                 filter = filter2, verbose = FALSE)
#'
#' windrose(mod2, type = "density", which = "foehn",
#'          ddvar = "winddir", ffvar = "windspd")
#'
#' @author Reto Stauffer
#' @export
windrose <- function(x, ...) UseMethod("windrose")

#' @rdname windrose
#' @export
windrose.foehnix <- function(x, type = NULL, which = NULL, ddvar = "dd", ffvar = "ff",
                             mfcol = 2L, maxpp = Inf, ...) {

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

    # Input argument mfcol
    if ( inherits(mfcol, "numeric") ) { mfcol <- as.integer(mfcol) }
    else { stopifnot(inherits(mfcol, "integer")) }
    stopifnot(mfcol > 0)

    # Input argument maxpp
    if ( !maxpp == Inf ) {
        if ( maxpp == -Inf ) stop("maxpp is not finite")
        if ( inherits(maxpp, "numeric") ) { maxpp <- as.integer(maxpp) }
        stopifnot(inherits(maxpp, "integer"))
        stopifnot(maxpp > 0)
    }

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
    # Step 1: calculate number of pages/rows/columns based on
    # the number of requested plots (given which/type) and the
    # user specification mfcol/maxpp.
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
    tmp <- list()
    if ( "density_unconditional" %in% which )
        tmp[["density_unconditional"]] <- with(data,
              windrose(dd, ff, hue = c(10, 130),
                       main = expression("Unconditional"), ...))
    if ( "density_nofoehn" %in% which )
        tmp[["density_nofoehn"]] <- with(subset(data, prob < 0.5),
              windrose(dd, ff, hue = c(100, 180),
                       main = expression(paste("No Foehn [",pi < 0.5,"]")), ...))
    if ( "density_foehn" %in% which )
        tmp[["density_foehn"]] <- with(subset(data, prob >= 0.5),
              windrose(dd, ff, hue = c(-20, 30),
                       main = expression(paste("Foehn [",pi >= 0.5,"]")), ...))
    # Plot type = "histogram"
    if ( "histogram_unconditional" %in% which )
        tmp[["histogram_unconditional"]] <- with(data,
              windrose(dd, ff, hue = c(10, 130), type = "histogram",
                       main = expression("Unconditional"), ...))
    if ( "histogram_nofoehn" %in% which )
        tmp[["histogram_nofoehn"]] <- with(subset(data, prob <  0.5),
              windrose(dd, ff, hue = c(100, 180), type = "histogram",
                       main = expression(paste("No Foehn [",pi < 0.5,"]")), ...))
    if ( "histogram_foehn" %in% which )
        tmp[["histogram_foehn"]] <- with(subset(data, prob >= 0.5),
              windrose(dd, ff, hue = c(-20, 30), type = "histogram",
                       main = expression(paste("Foehn [",pi >= 0.5,"]")), ...))

    # Invisible return of all windrose-returns, mainly for testing.
    invisible(tmp)
}



#' Default Wind Rose Plot
#'
#' Windrose plot, can handle two type of plots (see \code{type}) using
#' the same information.
#'
#' @param x either an univariate object (\code{numeric} vector or \code{zoo})
#'        containing meteorological wind directions or a multivariate \code{zoo} or
#'        \code{data.frame} object containing both, wind speed and wind
#'        meteorological wind direction. More details provided in the 'Details' section.
#' @param ff zoo object or numeric vector with sind speed data (\code{>= 0}).
#' @param interval numeric, a fraction of 360. If 360 cannot be devided by
#'        \code{interval} without a rest the script will stop. Interval
#'        for the segments along the wind direction \code{dd}. Default \code{10}.
#' @param type \code{NULL} or character, one of \code{density} or \code{histogram}.
#'        If \code{NULL} both types will be plotted.
#' @param windsector list, matrix, or data frame for highlighting one or multiple
#'        wind sectors. See 'Examples' ('Highlighting wind sectors').
#' @param main character, title. If no title is set a default one will be shown.
#' @param hue numeric vector of length 1 or two (for \code{type = "density"}
#'        two are used, for \code{type = "histogram"} only the first one will be
#'        used at the moment). Hue(s) for \code{colorspace::heat_hcl(...)}.
#' @param power numeric \code{>0}, power parameter for the alpha channel if 
#'        \code{type = "histogram"}. If \code{1} the alpha channel is linear
#'        for the whole range of \code{ff}, values \code{!= 1} change the
#'        alpha behavior.
#' @param ... currently unused.
#' @param dd can be used if univariate objects or vectors are provided for
#'        wind speed and meteorological wind direction (see 'Details' section).
#' @param filter a custom set of filter rules (see \code{\link{foehnix_filter}}).
#' @param var.dd string, custom name of the wind direction variable in \code{x}
#'        if \code{x} is a multivariate object (see 'Details' section).
#' @param var.ff string, custom name of the wind speed variable in \code{x}
#'        if \code{x} is a multivariate object (see 'Details' section).
#'
#' @details The \code{\link{windrose}} function can be used in different ways.
#' The main purpose is to plot (conditional) wind roses for
#' \code{\link{foehnix}} objects (uses \code{windrose.foehnix}), but there is
#' this generic \code{\link{windrose.default}} method which can be used to
#' create wind roses on non-foehnix objects.
#'
#' \code{\link{windrose.default}} can either with called with two univariate
#' inputs for wind direction and wind speed, or with a multivariate \code{zoo}
#' object or \code{data.frame} which provides both, wind speed and wind
#' direction. Moreover, additional variables which can be used in combination
#' with the \code{filter} option. Examples are provided in the example section.
#'
#' Univariate inputs/vectors: the \code{\link{windrose.default}} function requires both,
#' wind direction and wind speed. If univariate objects/vectors are used at least
#' the two inputs \code{dd} (identical to \code{x}) and \code{ff} have to be specified.
#' \code{dd} (\code{x}) is the meteorological wind direction in degrees
#' (\code{]0, 360[}, 0/360 is 'wind from north', 90 'wind from east',
#' 180 'wind from south', and 270 'wind from west').
#' \code{ff} has to be of the same length as \code{dd} containing the corresponding
#' wind speed.
#'
#' Multivariate input: rather than providing \code{dd} (\code{x}) and \code{ff}
#' a multivariate \code{zoo} object or \code{data.frame} can be provided when
#' calling the function containing (at least) wind direction and wind speed. By
#' default, the method expects the wind direction variable to be named
#' \code{"dd"}, the wind speed named \code{"ff"}.  Custom names can be
#' specified using the two input arguments \code{var.ff} and \code{var.dd}.
#'        
#' Custom filter: the optional \code{filter} input can be used if input
#' \code{x} is a multivariate object and provides a convenient
#' way to subset/filter the data. A detailed description how to define
#' the filter rules can be found on the documentation page of 
#' the \code{\link{foehnix_filter}} method.
#'
#' @examples
#' # Loading observation data for station Ellboegen.
#' # The object returned is a zoo time series object
#' # containing (see ?ellboegen).
#' data <- demodata("ellboegen")
#' class(data)
#' head(data)
#' 
#' # Extract dd/ff and create a windrose,
#' # using two vectors as input:
#' dd <- as.vector(data$dd)
#' ff <- as.vector(data$ff)
#' windrose(dd, ff,
#'          main = "Demo Windrose\nUse vector inputs")
#' windrose(dd = dd, ff = ff,
#'          main = "Demo Windrose\nUse vector inputs")
#' 
#' # Using univariate zoo objects as input:
#' windrose(dd = data$dd, ff = data$ff,
#'          main = "Demo Windrose\nUnivariate zoo objects as inputs")
#' 
#' # Or specify a multivariate zoo object or data.frame
#' # object on input x:
#' windrose(data,
#'          main = "Demo Windrose\nUse multivariate zoo object")
#' windrose(as.data.frame(data),
#'          main = "Demo Windrose\nUse multivariate data.frame object")
#' 
#' # Custom names for ff/dd
#' copy <- data
#' names(copy)[1:2] <- c("wind_dir", "wind_spd")
#' windrose(copy, main = "Demo Windrose\nMultivariate zoo, custom names",
#'          var.dd = "wind_dir", var.ff = "wind_spd")
#'
#' # Highlighting wind sectors
#' windrose(data, windsector = list(c(43, 223)),
#'          main = "Windrose\nUnnamed Wind Sector")
#' windrose(data, windsector = matrix(c(43, 223), nrow = 1),
#'          main = "Windrose\nUnnamed Wind Sector")
#' windrose(data, windsector = data.frame(from = 43, to = 223),
#'          main = "Windrose\nUnnamed Wind Sector")
#' windrose(data, windsector = list(A = c(100, 160), B = c(310, 10)),
#'          main = "Windrose\nNamed Wind Sector")
#' sectors <- matrix(c(100, 160, 310, 10), nrow = 2, byrow = TRUE,
#'                   dimnames = list(c("Down", "Up"), NULL))
#' windrose(data, windsector = sectors, 
#'          main = "Windrose\nUnnamed Wind Sector")
#' sectors <- matrix(seq(0, 350, by = 10), ncol = 2, byrow = TRUE)
#' windrose(data, windsector = sectors, main = "Yey")
#' 
#' # Custom filter: for details, see ?foehnix_filter
#' # Example 1:
#' # - Plot windrose for all observations where the wind
#' #   direction was within 43-233 degrees (south southeast)
#' filter1 <- list(dd = c(43, 223))
#' windrose(data, main = "Custom filter on wind direction,\n43 - 223 degrees",
#'          type = "hist", filter = filter1)
#' 
#' # Example 2:
#' # - Plot windrose for all observations where the wind
#' #   speed was > 5 meters per second.
#' filter2 <- list(ff = function(x) x > 10)
#' windrose(data, main = "Custom filter on wind speed\nonly observations where ff > 5",
#'          type = "hist", filter = filter2)
#' 
#' # Example 3:
#' # - Plot windrose for specific months (create new variable
#' #   'month' in advance):
#' data$month <- as.POSIXlt(index(data))$mon + 1
#' par(mfrow = c(1,2))
#' windrose(data, main = "Wind rose for January",
#'          filter = list(month = function(x) x == 1))
#' windrose(data, main = "Wind rose for July",
#'          filter = list(month = function(x) x == 7))
#' 
#' # Example 4:
#' # Similar to example 3, but for 
#' data$hour <- as.POSIXlt(index(data))$hour
#' par(mfrow = c(1,2))
#' windrose(data, main = "Wind rose for midnight (00 UTC)",
#'          filter = list(hour = function(x) x == 0))
#' windrose(data, main = "Wind rose for noon (12 UTC)",
#'          filter = list(hour = function(x) x == 12))
#' 
#' # Example 5:
#' # A more complex filter: midnight, winter (Dez/Jan/Feb)
#' # for wind speeds exceeding 5 meters per second.
#' filter5 <- function(x) x$month %in% c(12, 1, 2) & x$hour == 0 & x$ff > 5
#' par(mfrow = c(1,2))
#' windrose(data, main = "DJF 12 UTC (ff > 5)\nComplex Filter", filter = filter5)
#' windrose(data, main = "DJF 12 UTC (ff > 5)\nComplex Filter", filter = filter5, type = "hist")
#'
#' @rdname windrose.default
#' @export

#TODO: Merge docstring of windrose and windrose.default and windrose.foehnix,
# tricky to keep track of duplicates and missings as it is.
windrose.default <- function(x, ff,
                             interval = 10, type = "density", 
                             windsector = NULL, main = NULL, hue = c(10,100),
                             power = .5, ..., dd = NULL, filter = NULL,
                             var.ff = "ff", var.dd = "dd") {

    # If "x" is missing we need "dd"
    if (missing(x)) x <- dd
    if (is.null(x)) stop("either \"x\" or \"dd\" have to be specified")
    dd <- x

    # If an additional filter is set "x" has to be a multivariate
    # object (zoo or data.frame). If not, stop.
    if (!is.null(filter)) {
        if (is.null(ncol(x))) {
            stop("if a filter is set the first input argument",
                 "needs to be a multivariate zoo object or a data.frame!")
        }
        # Apply foehnix filter. Takes care that the variables exist.
        # Only take those flagged as "good".
        filter_obj <- foehnix_filter(x, filter)
        if(length(filter_obj$good) == 0)
            stop("no data left after applying the filter")

        # Subset the input "x", reduce data set to those flagged as "good"
        x <- x[filter_obj$good, ]
    } else {
        filter_obj <- NULL
    }

    windsector <- windsector_convert(windsector)

    # If ff is missing but x is of class zoo or data.frame with
    # at least two columns: try to find the corresponding variables
    # needed on object 'x'
    if (NCOL(x) > 1) {
        if (!var.ff %in% names(x)) {
            stop(sprintf("input \"x\" is a multi-column object, but cannot find var.ff = \"%s\"",
                         var.ff))
        } else if (!var.dd %in% names(x)) {
            stop(sprintf("input \"x\" is a multi-column object, but cannot find var.dd = \"%s\"",
                         var.dd))
        }
        # Overwrite dd/ff
        dd <- x[, var.dd]
        ff <- x[, var.ff]
    }

    # Check "dd" values
    if (!length(dd) == length(ff)) {
        stop("Unequal length of wind speed and wind direction values!")
    } else if (min(dd, na.rm = TRUE) < 0 | max(dd, na.rm = TRUE) > 360) {
        stop("Wind direction not within the expected range of [0, 360]!")
    } else if (min(ff, na.rm = TRUE) < 0) {
        stop("Got wind speeds below 0!")
    } else if (isTRUE(all.equal(sd(ff), 0))) {
        stop("variable ff is constant")
    } else if (isTRUE(all.equal(sd(dd), 0))) {
        stop("variable dd is constant")
    }

    # Plot type
    type = match.arg(type, c("density", "histogram"))

    # Stop if alpha is strange
    if ( length(power) != 1 ) stop("\"power\" has to be of length 1.")
    if ( power[1L] < 0 ) stop("\"power\" has to be > 0")

    # Interval check
    if ( ! floor(360/interval)*interval == 360 ) stop("interval wrong")

    # If both input objects are of class zoo: merge
    if ( inherits(dd, "zoo") & inherits(ff, "zoo") ) {
        stopifnot(inherits(dd, "zoo"))
        stopifnot(inherits(ff, "zoo"))
    
        # Combine dd, ff, probabilities, and start plotting.
        data <- na.omit(merge(dd, ff))
    # Else check if length of dd and ff match and create a
    # data.frame.
    } else {
        if ( ! length(dd) == length(ff) )
            stop("Length of input objects for wind direction and wind speed do not match.")
        data <- data.frame(dd = as.numeric(dd), ff = as.numeric(ff))
    }
    if ( nrow(data) == 0 ) stop("No data left after combining \"dd\" and \"ff\".")

    # Breaks for classification
    dd.breaks <- seq(-interval / 2, 360, by=interval)
    ff.breaks <- pretty(ff)

    # Picking some colors
    cols <- rev(colorspace::heat_hcl(length(ff.breaks) - 1, h= hue, c. = c(60, 10),
                                     l = c(25, 95), power = c(0.7,2)))

    # ----------------
    # Prepare data for the plots
    # Type "density"
    if ( type == "density") {

        # Create data table
        tab <- windrose_get_density(data, dd.breaks, ff.breaks)
        counts <- NULL # Default, required for the checks

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
    if (!is.null(names(windsector))) {
        xlim <- xlim * 1.2; ylim <- ylim * 1.2
    }
    # Base plot
    plot(0, type = "n", asp = 1, bty = "n", main = NA,
         xlim = xlim, xaxt = "n", xlab = NA,
         ylim = ylim, yaxt = "n", ylab = NA)
    mtext(side = 3, at = 0, cex = 1.2, font = 2, line = .1, main)

    # Adding wind sectors (if required)
    if (!is.null(windsector))
        windrose_add_windsector(windsector, if (type == "density") density.breaks else ff.breaks)

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
    msg <- if(is.null(filter)) {
               sprintf("N = %d", nrow(data))
           } else {
               sprintf("N = %d/%d\ncustom data filter", nrow(data), filter_obj$total)
           }
    text(x0, -mean(tail(at, 2)), pos = 4, msg)

    # Invisible return of some properties, mainly for testing.
    invisible(list(xlim       = xlim,
                   counts     = counts,
                   interval   = interval,
                   ff.breaks  = ff.breaks,
                   filter_obj = filter_obj,
                   dd.breaks  = dd.breaks,
                   tab        = tab))

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
#' plot type. Based on the count matrix (\code{\link{windrose_get_counts}})
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


#' Adding Wind Sector for Windrose Plots
#'
#' The windrose plot allows to specify one or multiple
#' segments to be highlighted on the plot. This is a
#' helper function to place the segments.
#'
#' @param x list containing vectors of length 2 or a matrix with two
#'        rows. If the list is named, or the matrix has row names,
#'        the names will be used to label the segments.
#' @param ff wind speed (in case of plot type "density", or density in 
#'        case of plot type "histogram". Used to draw the polygon and
#'        to plot the labels (if needed).
#' @param offset offset for text adjustment.
#' 
#' @author Reto Stauffer
windrose_add_windsector <- function(x, ff, offset = .02) {

    # Looping over all entries, draw wind sectors and
    # add labels if names are given.
    for (i in seq_along(x)) {

        # If the first entry is > second entry: subtract
        # 360 from entry 1.
        if (diff(x[[i]]) < 0) x[[i]] <- x[[i]] - c(360, 0)

        # Calculate the "wind directions" to draw the outer
        # bow (segment along a circle).
        dd <- seq(x[[i]][1L], x[[i]][2L], by = min(diff(x[[i]]) / 10, 1))

        # Create coordinates for the polygon
        tmpup <- ddff2uv(dd, max(ff))
        if (min(ff) == 0) {
            tmplo <- matrix(0, nrow = 1, ncol = 3, dimnames = list(NULL, c("u", "v", "rad")))
            tmp   <- rbind(tmpup, tmplo[nrow(tmplo):1, ])
        } else {
            tmplo <- ddff2uv(dd, min(ff))
            tmp   <- rbind(tmpup, tmplo[nrow(tmplo):1, ], tmpup[1L,])
        }

        # Draw the polygon
        polygon(-tmp$u, -tmp$v, col = "gray90", border = NA)#1, lty = 3)

        # If we have names: draw name
        if (!is.null(names(x))) {
            # Calculate the position (x/y)
            ddrad <- uv2ddff(mean(tmpup[, "u"]), mean(tmpup[, "v"]), rad = TRUE)$dd
            idx <- as.numeric(cut(ddrad, breaks = seq(0, 2 * pi, by = pi / 8), include.lowest = TRUE))
            idx <- floor(ifelse(idx > 15, idx - 15, idx) / 2) + 1
            # Adjustment
            pos <- c(3, 4, 4, 4, 1, 2, 2, 2)[idx]
            # Setting wind sector label
            text(sin(ddrad) * max(ff) * (1 + offset),
                 cos(ddrad) * max(ff) * (1 + offset),
                 names(x)[i], pos = pos, xpd = TRUE)
        }
    }
}



