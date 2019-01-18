# -------------------------------------------------------------------
# - NAME:        tsplot.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-16
# -------------------------------------------------------------------
# - DESCRIPTION: Time series plot of observations plus classification
#                based on a foehnix mixture model.
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2019-01-18 19:31 on marvin
# -------------------------------------------------------------------


#' foehnix Time Series Plot Config
#'
#' The foehnix package comes with a default time series plotting
#' function (see \code{\link[foehnix]{tsplot}}) with a pre-specified
#' behavior regarding variable names, colors, and labels.
#' The \code{\link[foehnix]{tsplot}} function, however, allows to
#' set some custom specifications, e.g., if the names of your
#' observations (variable name of the observations in the time
#' series object used to train the \code{\link[foehnix]{foehnix}}
#' model) are different, if different labels are required, or
#' if you do not like our pretty colors! This function returns
#' a control object for the time series plots for customization.
#'
#' @param ... a set of named inputs to overwrite the defaults.
#'     see 'Details' section.
#' @param var used when calling the \code{\link[foehnix]{get.tsplot.control}}
#'     function. Name of the (original!) variable name
#' @param property the property which should be returned by
#'     \code{\link[foehnix]{get.tsplot.control}}.
#'
#' @details By default the \code{\link[foehnix]{tsplot}} function
#'     expects that the variable names are called
#' \itemize{
#'      \item \code{t}: dry air temperature (degrees Celsuis)
#'      \item \code{rh}: relative humidity in percent
#'      \item \code{diff_t}: temperature difference to a nearby
#'             crest station
#'      \item \code{dd}: wind direction in (meteorological) degrees
#'      \item \code{ff}: wind speed in meters per second
#'      \item \code{ffx}: gust speed in meters per second
#' }
#' If \code{\link[foehnix]{tsplot}} can find these variables,
#' it will plot the corresponding observations, label the plots,
#' and uses a set of default colors.
#' The \code{\link[foehnix]{tsplot.control}} function allows
#' to overrule the defaults by specifying custom values, e.g.:
#' imagine that your wind speed variable is not \code{ff} but
#' \code{windspd}. You can easily tell \code{\link[foehnix]{tsplot}}
#' that it has to use this custom name by calling \code{\link[foehnix]{tsplot}}
#' as follows:
#' \itemize{
#'      \item \code{tsplot(x, ff = "windspd")}
#' }
#' If your wind speed variable is not even in meters per second but knots, and
#' you dislike our default color, you can also provide a list to overwrite the
#' default \code{name}, default \code{color} and default \code{label} by calling:
#' \itemize{
#'      \item \code{tsplot(x, ff = list(name = "windspd", color = "#0000ff", label = "wind speed in knots")}
#' }
#' In the same way it can be used to overrule the defaults for all other
#' variables used in the time series plotting function (see list above).
#'
#' @return Returns an object of class \code{c("tsplot.control", "data.frame")}
#' with the specification for the time series plot.
#'
#' @export
#' @author Reto Stauffer
tsplot.control <- function(...) {
    def <- "
    var;    name;    color;   label
    t;      t;       #FF0000; air temperature [C]
    rh;     rh;      #009900; relative humidity [%]
    diff_t; diff_t;  orange;  temperature difference [C]
    dd;     dd;      black;   wind direction [deg]
    ff;     ff;      #005ce6; wind speed [m/s]
    ffx;    ffx;     #5c00e6; gust speed [m/s]"
    # Reading data.frame definition from the string above.
    def <- read.table(textConnection(def), sep = ";",
                      header = TRUE,
                      strip.white = TRUE, comment.char = "",
                      colClasses = rep("character", 3))

    # User arguments
    arg <- list(...)
    for ( n in names(arg) ) {
        idx <- which(def$name == n)
        if ( length(idx) == 0 ) next
        # Else modify the definition.
        # If input is a character: modify name
        if ( inherits(arg[[n]], "character") ) {
            def[idx,"name"] <- arg[[n]]
        # If input is a list: modify the elements we have
        # in the definition above.
        } else if ( inherits(arg[[n]], "list") ) {
            for ( m in names(arg[[n]]) ) {
                m <- match.arg(m, names(def))
                if ( m %in% names(def) ) def[idx,m] <- arg[[n]][[m]]
            }
        } else {
            warning("Got unexpected input on 'tsplot.control'. Will be ignored.")
        }
    }

    # Add custom class and return
    class(def) <- c("tsplot.control", "data.frame")
    return(def)
}

#' @rdname tsplot.control
get <- function(x, ...) UseMethod("get")
get.tsplot.control <- function(x, var, property) {
    # Check if we have the property (match.arg)
    property <- match.arg(property, names(x))
    # Try to find the variable (match.arg)
    var <- match.arg(var, x$var)
    # Return
    return(x[which(x$var == var), property])
}

#' Time Series Plot of foehnix Models
#' 
#' Development time series plots of estimated \code{\link{foehnix}}
#' models. TODO: they are very specific at the moment!
#' 
#' @param x object of type \code{foehnix}.
#' @param start POSIXt object or an object which can be converted to POSIXt.
#' @param end POSIXt object or an object which can be converted to POSIXt.
#' @param ndays integer, number of days used when looping trough the time series.
#' @param varnames list of custom variable names, see 'Details' section.
#' @param ... additional arguments, ignored.
#' @param xtra optional zoo object with probabilities in \code{]0,1[} to compare
#'         two classification algorithms.
#' @param ask logical, default is \code{TRUE}.
#' 
#' @details Development method to access plausability of the estimated foehn
#' proabilities.  For software release this method should either be removed or
#' made much more general. At the moment the method heavily depends on the
#' names of the data used as input for \code{\link{foehnix}}.
#'
#' This time series plotting function creates a relatively specific plot
#' and also expects, by default, a set of default variables and variable
#' names. This function uses the data set provided on the \code{data}
#' input argument when calling the \code{\link{foehnix}} method.
#' As they might differ from the \code{foehnix} defaults the
#' \code{varnames} input argument allows to specify custom names.
#' The ones expected:
#' 
#' \itemize{
#'      \item \code{t}: dry air temperature
#'      \item \code{rh}: relative humidity (in percent)
#'      \item \code{diff_t}: temperature difference between the
#'            crest and the valley station
#'      \item \code{dd}: meteorological wind direction (\code{[0, 360]})
#'      \item \code{ff}: wind speed
#' }
#'
#' Custom names can be specified by renaming the defaults based on a
#' named list. As an example: assume that wind direction is called
#' \code{winddir} and wind speed is called \code{windspd} in your data set.
#' Specify the following input to rename/respecify the defaults:
#' \itemize{
#'       \item \code{varnames = list(dd = "winddir", ff = "windspd")}
#' }
#'
#' Please note: if a variable cannot be found (no matter whether
#' the default variable names have been renamed or not) this specific
#' variable will be ignored. Subplots will not be displayed if no
#' data are available at all.
#'
#' @author Reto Stauffer
#' @export
tsplot <- function(x, start = NULL, end = NULL, ndays = 10,
                   control = tsplot.control(...),
                   ..., xtra = NULL, ask = TRUE) {

    # Input 'x' needs to be of class foehnix
    if ( ! inherits(x, "foehnix") )
        stop("tsplot is only for objects of class \"foehnix\".")

    # Stop if control is not of tsplot.control
    stopifnot(inherits(control, "tsplot.control"))

    # Check available data. This allows us to check
    # which of the default subplots can be drawn.
    check <- function(x, control, check) {
        return(sum(check %in% control$var) > 0)
    }
    doplot <- list(
        "temp"        = check(names(x$data), control, c("t", "rh")),
        "tempdiff"    = check(names(x$data), control, c("diff_t")),
        "wind"        = check(names(x$data), control, c("dd", "ff", "ffx")),
        "prob"        = TRUE
    )
    Nplots <- sum(sapply(doplot, function(x) return(x)))
    if ( Nplots == 0 )
        stop(sprintf(paste("Cannot find any of the required variables!",
             "One reason: your variable names do not match any of",
             "the default variable names (%s).",
             "The 'varnames' input argument allows you to",
             "change these names (please see ?tsplot manual page)."),
             paste(names(vars), collapse = ", ")))

    # Helper function to add the gray boxes (background)
    add_boxes <- function(x, col = "gray90") {
        dx  <- as.numeric(diff(index(x)[1:2]), unit = "secs") / 2
        up   <- which(diff(x >= .5) == 1) + 1
        down <- which(diff(x >= .5) == -1)
        if ( length(up) == 0 | length(down) == 0 ) return();
        if ( min(down) < min(up) ) up <- c(1, up)
        isna <- which(is.na(x))
        if ( length(up) > 0 & length(down) > 0 ) {
            y <- par()$usr[3:4]
            for ( i in seq(1, length(up))) {
                if ( length(isna) > 0 ) {
                    if ( any(isna > up[i]) ) {
                        to <- min(min(isna[isna > up[i]]), down[i])
                    } else { to <- down[i]; }
                } else {
                    to <- down[i]
                }
                if ( is.na(to) ) to <- nrow(tmp)
                rect(index(x)[up[i]] - dx, y[1L], index(x)[to] + dx, y[2L],
                     col = col, border = NA)
            }
        }
    }

    # Helper function to add the vertical lines
    add_midnight_lines <- function(x) {
        ndays <- as.numeric(diff(range(index(x))), unit = "days")
        if ( ndays < 50 ) {
            at <- as.POSIXct(unique(as.Date(index(x))))
            abline(v = at, col = 1)
        }
    }

    # Convert start/end to POSIXct
    if ( ! is.null(start) ) {
        start <- try(as.POSIXct(start))
        if ( inherits(start, "try-error") )
            stop("Invalid input for \"start\". Cannot be converted to POSIXt.")
    }
    if ( ! is.null(end) ) {
        end <- try(as.POSIXct(end))
        if ( inherits(end, "try-error") )
            stop("Invalid input for \"end\". Cannot be converted to POSIXt.")
    }

    # Default plot type/plot interval if start and end are not
    # provided:
    if ( is.null(start) & is.null(end) ) {
        # Extracting zoo index (range of dates)
        dates <- as.POSIXct(as.Date(range(index(x$prob))))
        if ( max(index(x$prob)) > dates[2L] ) dates <- dates + c(0,86400)
        # If less than ndays days: plot all 10 days.
        if ( as.numeric(diff(dates), units = "days") <= ndays ) {
            start <- dates[1L]; end <- dates[2L]
        # Else create a set of sequences to plot
        } else {
            start <- seq(dates[1L], dates[2L], by = 86400 * ndays)
            end   <- start + 86400 * ndays
            start <- start[start < dates[2L]]
            end   <- pmin(end[start < dates[2L]], dates[2L])
        }
    } else {
        if ( is.null(end) & length(start) != 1 )
            stop("If input \"end\" is not provided \"start\" has to be of length 1")
        if ( is.null(start) & length(end) != 1 )
            stop("If input \"start\" is not provided \"end\" has to be of length 1")
        if ( is.null(end) )   end   <- max(x$prob)
        if ( is.null(start) ) start <- min(x$prob) 
    }
    # Check whether both (start and end) are of same length
    if ( ! length(start) == length(end) )
        stop("Input \"start\" and \"end\" have to be of same length!")

    # Check if time range is valid
    if ( all(start > max(index(x$prob))) | all(end < min(index(x$prob))) )
        stop("All time periods defined by start/end outside specified data set.")

    # Keep user settings (will be reset when this function ends)
    hold <- par(no.readonly = TRUE); on.exit(par(hold))


    # Combine foehn probabilities and observations
    data <- merge(x$prob, x$data)
    names(data)[1L] <- "prob"

    # Looping over the different periods we have to plot
    for ( k in seq_along(start) ) {

        # Parameters of the graphical output device
        par(mfrow = c(Nplots, 1), ask = FALSE, mar = rep(0.1, 4), xaxs = "i", oma = c(4.1, 4.1, 2, 5.1))

        tmp <- window(data, start = start[k], end = end[k])
        # No data, or only missing data?
        if ( nrow(tmp) == 0 | sum(!is.na(tmp)) == 0 ) {
            tmp <- paste("No data (or only missing values) for the time period",
                         strftime(start[k], "%Y-%m-%d %H:%M"), "to",
                         strftime(end[k], "%Y-%m-%d %H:%M"))
            warning(sprintf("%s. Skip plotting.", str))
            next
        }

        # Air temperature
        if ( doplot$temp ) {
            # If temperature is in the data set: plot temperature,
            # if not, set up an empty plot (required to be able to
            # add relative humidity and temperature differences).
            param <- get(control, "t", "name")
            if ( param %in% names(tmp) ) {
                plot(tmp[,param], type = "n", ylab = NA, xaxt = "n", bty = "n")
                add_boxes(tmp$prob); add_midnight_lines(tmp)
                lines(tmp[,param], lwd = 2,
                      col = get(control, "t", "color"))
                mtext(side = 2, line = 3, get(control, "t", "label"))
                box()
            } else {
                plot(NA, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
                     xlim = range(index(tmp)))
            }
    
            # Relative humidity
            param <- get(control, "rh", "name")
            if ( param %in% names(tmp) ) {
                par(new = TRUE)
                plot(tmp[,param], type = "n", lwd = 2, yaxt = "n",
                     ylim = c(0,150), yaxs = "i", xaxt = "n", bty = "n")
                add_polygon(tmp[,param], col = "#009900")
                abline(h = seq(20, 100, by = 20), lty = 3,
                       col = sprintf("%s50", get(control, "rh", "color")))
                axis(side = 4, at = seq(20, 100, by = 20))
                mtext(side = 4, line = 3, "relative humidity")
                box()
            }
    
            # Temperature difference
            param <- get(control, "diff_t", "name")
            if ( param %in% names(tmp) ) {
                plot(tmp[,param], type = "n", xaxt = "n", bty = "n")
                add_boxes(tmp$prob); add_midnight_lines(tmp)
                lines(tmp[,param], lwd = 2,
                      col = get(control, "diff_t", "color"))
                abline(h = seq(-20,20, by = 1), col = "gray80", lty = 3)
                abline(h = 0, col = 1)
                mtext(side = 2, line = 3, "temperature difference")
                box()
            }
        }
    
        # Plotting wind direction and wind speed
        if ( doplot$wind ) {
            plot(NA, type = "n", xaxt = "n", ylab = "", xlim = range(index(tmp)),
                     ylim = c(0, 360), yaxt = "n", bty = "n")
            add_boxes(tmp$prob); add_midnight_lines(tmp)
            param <- get(control, "dd", "name")
            if ( param %in% names(tmp) ) {
                points(tmp[,param], col = "black", pch = 19, cex = 0.5)
                axis(side = 2, at = seq(90, 360 - 90, by = 90))
                mtext(side = 2, line = 3, "wind direction")
                box()
            }
        
            # Adding wind speed
            pff  <- get(control, "ff", "name")
            pffx <- get(control, "ffx", "name")
            if ( any(c(pff, pffx) %in% names(tmp)) ) {
                # Get y limits
                if ( all(c(pff, pffx) %in% names(tmp)) ) {
                    ymax <- max(max(tmp[,pff], na.rm = TRUE), max(tmp[,pffx], na.rm = TRUE))
                    ylab <- sprintf("%s\n%s", get(control, "ffx", "label"), get(control, "ff", "label"))
                } else if ( pff %in% names(tmp) ) {
                    ymax <- max(tmp[,pff], na.rm = TRUE)
                    ylab <- get(control, "ff", "label")
                } else {
                    ymax <- max(tmp[,pffx], na.rm = TRUE)
                    ylab <- get(control, "ffx", "label")
                }
                ylim <- c(0, ymax*1.05)
                # Plotting an empty sub-figure
                par(new = TRUE)
                print(range(index(tmp)))
                print(ylim)
                plot(NA, type = "n", yaxs = "i", yaxt = "n", xaxt = "n",
                     xlim = range(index(tmp)), ylim = ylim)
                # Adding ffx if existing
                if ( pffx %in% names(tmp) )
                    lines(tmp[, pffx], col = get(control, "ffx", "color"))
                # Adding ff if existing
                if ( pff %in% names(tmp) )
                    add_polygon(tmp[, pff], col = get(control, "ff", "color"))
                axis(side = 4, at = pretty(tmp[,param]))
                mtext(side = 4, line = 3, ylab)
                box()
            }
        }
    
        # Foehn prob
        plot(tmp$prob * 100, type = "n", ylab = NA, ylim = c(-4,104), yaxs = "i") 
        add_boxes(tmp$prob); add_midnight_lines(tmp)
        if ( ! is.null(xtra) ) lines(xtra * 100, col = "gray50", lty = 5)
        abline(h = seq(0, 100, by = 20), col = "gray", lty = 3)
        mtext(side = 2, line = 3, "foehn probability")
        add_polygon(tmp$prob * 100, col = "#FF6666", lower.limit = -4)
        # Adding RUG
        at <- index(tmp$prob)[which(tmp$prob >= .5)]
        if ( length(at) > 0 ) axis(side = 1, at = at, labels = NA, col = 2)
        # Adding "missing data" RUG
        at <- index(tmp$prob)[which(is.na(tmp$prob))]
        if ( length(at) > 0 ) axis(side = 1, at = at, labels = NA, col = "gray50")
        box()
        if ( ! is.null(xtra) )
            legend("left", bg = "white", col = c("#FF6666", "gray50"), lty = c(1,5),
                   legend = c("foehnix", "xtra"))
    
        # Adding a title to the plot
        title <- sprintf("Foehn Diagnosis %s to %s", start[k], end[k])
        mtext(side = 3, outer = TRUE, title, font = 2, cex = 1.2, line = 0.5)

        # If multiple periods have to be plotted: set ask = TRUE
        if ( ask & k < length(start) ) readline("Press Enter for next plot> ")

    } # End of loop over start/end (loop index k)


}


#' Add Polygon to Plot
#'
#' Helper function to plot a filled polygon based on a \code{zoo}
#' time series object with nice missing data handling.
#'
#' @param x an univariate \code{zoo} time series object.
#' @param col character, a hex color. Default is \code{"#ff0000"} (red).
#' @param lower.limit numeric, the lower limit used to plot the polygon.
#'        default is \code{0}.
#' @param lwd line width argument.
#'
#' @examples
#' library("zoo")
#' # Create a time series object
#' set.seed(3)
#' a <- zoo(sin(1:100/80*pi) + 3 + rnorm(100, 0, 0.3), 201:300)
#' 
#' # Plot
#' par(mfrow = c(1,3))
#' plot(a, type = "n", main = "Demo Plot 1",
#'      ylim = c(-1, max(a)+1), xaxs = "i", yaxs = "i")
#' add_polygon(a)
#' 
#' # Specify lower.limit to -1 (lower limit of our ylim),
#' # add different color, change line style.
#' plot(a, type = "n", main = "Demo Plot 2",
#'      ylim = c(-1, max(a)+.2), xaxs = "i", yaxs = "i")
#' add_polygon(a, col = "#00CCBB", lower.limit = -1, lwd = 3)
#' 
#' # Using an "upper limit".
#' plot(a, type = "n", main = "Demo Plot 3",
#'      ylim = c(-1, max(a)+.2), xaxs = "i", yaxs = "i")
#' add_polygon(a, col = "#00BBFF", lower.limit = par()$usr[4L])
#' 
#' # Make a copy and add some missing values
#' b <- a
#' b[2:10]  <- NA
#' b[50:55] <- NA
#' b[70]    <- NA
#' 
#' # Plot
#' par(mfrow = c(1,1))
#' 
#' # Same as "Demo Plot 2" with the time series which
#' # contains missing values (b).
#' plot(b, type = "n", main = "Demo Plot 2 With Missing Values",
#'      ylim = c(-1, max(b, na.rm = TRUE)+.2), xaxs = "i", yaxs = "i")
#' add_polygon(b, col = "#00CCBB", lower.limit = -1, lwd = 3)
#' 
#' @import zoo
#' @import graphics
#' @author Reto Stauffer
#' @export
add_polygon <- function( x, col = "#ff0000", lower.limit = 0, lwd = 1 ) {
    # Need hex color
    if ( ! grepl("^#[A-Za-z0-9]{6}$",col) ) stop("Sorry, need hex color definition for polygon plots.")
    # All elements NA?
    if ( all( is.na(x) ) ) return(invisible(NULL))
    # Else find valid blocks and plot them. Start with 1
    i <- 1
    while ( i <= length(x) ) {
        if ( all(is.na(x)) ) break
        i1 <- min( which( !is.na(x) ) )
        if ( i1 > 1 ) { x <- x[-c(seq(1,i1-1))]; i1 <- 1 }
        # Else check first NA
        if ( ! any(is.na(x)) ) { i2 <- length(x) } else { i2 <- min( which( is.na(x) ) ) - 1 }
        # Create x/y coordinate vectors for the polygon function
        p_x <- as.numeric(zoo::index(x[i1:i2])); p_x <- c(p_x,max(p_x),min(p_x))
        p_y <- c(as.numeric(x[i1:i2]),lower.limit, lower.limit )
        # Plotting
        graphics::polygon( p_x, p_y, col = sprintf("%s20",col), border = NA )
        graphics::lines( x[i1:i2],   col = col, lwd = lwd )
        # Remove plotted data from time series and continue
        x <- x[-c(i1:i2)]
    }

}

