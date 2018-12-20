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
# - L@ST MODIFIED: 2018-12-20 18:09 on marvin
# -------------------------------------------------------------------

#' Time Series Plot of foehnix Models
#' 
#' Development time series plots of estimated \code{\link{foehnix}}
#' models. TODO: they are very specific at the moment!
#' 
#' @param x object of type \code{foehnix}.
#' @param start POSIXt object or an object which can be converted to POSIXt.
#' @param end POSIXt object or an object which can be converted to POSIXt.
#' @param ndays integer, number of days used when looping trough the time series.
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
#' TODO: this is very specific to our data and our variable names.
#'       either provide something like this and force the users to
#'       follow our naming conventions, or make it much more
#'       flexible/generig.
#'
#' @author Reto Stauffer
#' @export
tsplot <- function(x, start = NULL, end = NULL, ndays = 10, ..., xtra = NULL, ask = TRUE) {

    if ( ! inherits(x, "foehnix") )
        stop("tsplot is only for objects of class \"foehnix\".")

    add_boxes <- function(x, col = "gray90") {
        dx  <- as.numeric(diff(index(x)[1:2]), unit = "secs") / 2
        up   <- which(diff(x >= .5) == 1) + 1
        down <- which(diff(x >= .5) == -1)
        if ( min(down) < min(up) ) up <- c(1, up)
        isna <- which(is.na(x))
        if ( length(up) > 0 & length(down) > 0 ) {
            y <- par()$usr[3:4]
            for ( i in seq(1, length(up))) {
                if ( length(isna) > 0 ) {
                    to <- min(min(isna[isna > up[i]]), down[i])
                } else {
                    to <- down[i]
                }
                if ( is.na(to) ) to <- nrow(tmp)
                rect(index(x)[up[i]] - dx, y[1L], index(x)[to] + dx, y[2L],
                     col = col, border = NA)
            }
        }
    }
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

    # If multiple periods have to be plotted: set ask = TRUE
    if ( length(start) > 1 ) par(ask = ask) else par(ask = FALSE)

    # Combine foehn probabilities and observations
    data <- merge(x$prob, x$data)
    names(data)[1L] <- "prob"

    # Looping over the different periods we have to plot
    par(mfrow = c(4,1), mar = rep(0.1, 4), xaxs = "i", oma = c(4.1, 4.1, 2, 4.1))
    for ( k in seq_along(start) ) {

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
        plot(tmp$t, type = "n", ylab = NA, xaxt = "n", bty = "n")
        add_boxes(tmp$prob); add_midnight_lines(tmp)
        lines(tmp$t, col ="red", lwd = 2)
        mtext(side = 2, line = 3, "dry air temperature")
        box()
    
        # Relative humidity
        par(new = TRUE)
        plot(tmp$rh, type = "n", lwd = 2, yaxt = "n", ylim = c(0,150), yaxs = "i", xaxt = "n", bty = "n")
        add_polygon(tmp$rh, col = "#009900")
        abline(h = seq(20, 100, by = 20), lty = 3, col = "#00990060")
        axis(side = 4, at = seq(20, 100, by = 20))
        mtext(side = 4, line = 3, "relative humidity")
        box()
    
        # Temperature difference
        plot(tmp$diff_t, type = "n", xaxt = "n", bty = "n")
        add_boxes(tmp$prob); add_midnight_lines(tmp)
        lines(tmp$diff_t, col = "orange", lwd = 2)
        abline(h = seq(-20,20, by = 1), col = "gray80", lty = 3)
        abline(h = 0, col = 1)
        mtext(side = 2, line = 3, "temperature difference")
        box()
    
        # Wind speed and direction
        if ( ! is.null(x$windsector) ) {
            stopifnot("dd" %in% names(tmp))
            if ( x$windsector[1L] < x$windsector[2L] ) {
                ddflag <- ifelse(tmp$dd < x$windsector[1L] | tmp$dd > x$windsector[2L], 1, 2) 
            } else {
                ddflag <- ifelse(tmp$dd > x$windsector[1L] | tmp$dd < x$windsector[2L], 1, 2) 
            }
        } else { ddflag <- rep(2, nrow(tmp)) }
        plot(NA, type = "n", xaxt = "n", ylab = "", xlim = range(index(tmp)),
                 ylim = c(0, 360), yaxt = "n", bty = "n")
        add_boxes(tmp$prob); add_midnight_lines(tmp)
        if ( "dd" %in% names(tmp) ) {
            points(tmp$dd, col = c("gray50","black")[ddflag], pch = c(1, 19)[ddflag], cex = c(.3, .5)[ddflag])
        }
        axis(side = 2, at = seq(90, 360 - 90, by = 90))
        mtext(side = 2, line = 3, "wind direction")
        if ( ! is.null(x$windsector) ) abline(h = x$windsector, col = "gray", lty = 3)
        box()
    
        # Adding wind speed
        par(new = TRUE)
        plot(tmp$ff, type = "n", ylim = c(0, max(tmp$ff, na.rm = TRUE)) * 1.05,
             yaxs = "i", yaxt = "n", xaxt = "n")
        add_polygon(tmp$ff, col = "#005ce6")
        axis(side = 4, at = pretty(tmp$ff))
        mtext(side = 4, line = 3, "wind speed")
        box()
    
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

