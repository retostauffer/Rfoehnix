

# Some global variable(s) to make R CMD check happy
utils::globalVariables("vars")


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
#' @param style character, name of the style template (\code{default}, \code{advanced},
#'      \code{bw}) or path to a file containing the required information.
#' @param windsector vector or list to highlight specific wind sectors.
#'      See \code{\link[foehnix]{foehnix:::windsector_convert}} for details
#' @param var used when calling the \code{\link[foehnix]{tsplot_get_control}}
#'     function. Name of the (original!) variable name
#' @param property the property which should be returned by
#'     \code{\link[foehnix]{tsplot_get_control}}
#' @param x a \code{\link[foehnix]{tsplot.control}} object.
#' @param args list of named arguments used when calling
#'      \code{tsplot_get_control}
#'
#' @details By default the \code{\link[foehnix]{tsplot}} function
#'     expects that the variable names are called
#' \itemize{
#'      \item \code{t}: dry air temperature (degrees Celsius)
#'      \item \code{crest_t}: dry air temperature crest (degrees Celsius)
#'      \item \code{rh}: relative humidity (percent)
#'      \item \code{crest_rh}: relative humidity crest (percent)
#'      \item \code{diff_t}: temperature difference to a nearby
#'             crest station
#'      \item \code{dd}: wind direction (meteorological degrees)
#'      \item \code{dd}: wind direction crest (meteorological degrees)
#'      \item \code{ff}: wind speed (meters per second)
#'      \item \code{crest_ff}: wind speed crest (meters per second)
#'      \item \code{ffx}: gust speed (meters per second)
#'      \item \code{crest_ffx}: gust speed crest (meters per second)
#' }
#'
#' Different \code{style} presets are available. Styles can be accessed using
#' the \code{style} input argument. Different styles also enable/disable 
#' specific observations (e.g., the \code{"default"} style supresses the plotting
#' of crest-station observations, even if present).
#'
#' Can be set to \code{NULL} to be disabled. If the variable exists in the
#' data set but was (manually) set to \code{NULL} it will be neglected during
#' plotting. E.g., \code{tsplot(mod, crest_ff = NULL, crest_ffx = NULL)} to
#' disable crest station wind speed and gust speed (on plot).
#'
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
tsplot.control <- function(style = c("default", "bw", "advanced"),
                           windsector = NULL, ...) {

    # Evaluate template. Check if "style" is a file name (containing a ".").
    # If so, check if file exists. Else use standard evaluation (one of the
    # characters mentioned above).
    if (length(style) == 1L && grepl(".*\\..*", style)) {
        style_file <- style
    } else {
        style      <- match.arg(style)
        style_file <- system.file(sprintf("tsplot.control/%s.csv", style), package = "foehnix")
    }
    stopifnot(file.exists(style_file))

    # Reading data.frame definition from the string above.
    def <- try(read.table(style_file, sep = ";", header = TRUE,
                          strip.white = TRUE, comment.char = "",
                          colClasses = rep(c("logical", "character", "integer",
                                             "numeric", "integer", "character"),
                                           times = c(1, 2, 1, 2, 1, 3))), silent = TRUE)
    if (inherits(def, "try-error"))
        stop(sprintf("Problems reading style file \"%s\": unexpected content.",
                     style_file))

    # Remove disabled elements
    def <- subset(def, enabled)
    # Convert definition to list
    def <- lapply(setNames(def$var, def$var),
                  function(x, df) as.list(subset(df, var == x, -var)), df = def)

    # User arguments
    arg <- list(...)
    for (var in names(arg)) {
        if (!var %in% names(def)) next
        # Drop from 'def' - will not be considered during plotting.
        if (is.null(arg[[var]])) { def[[var]] <- NULL; next }
        # Else modify the definition.
        # If input is a character: modify name
        if (inherits(arg[[var]], "character")) {
            def[[var]]$name <- arg[[var]]
        # If input is a list: modify the elements we have
        # in the definition above.
        } else if (inherits(arg[[var]], "list")) {
            for (m in names(arg[[var]])) {
                def[[var]][[m]] <- arg[[var]][[m]]
            }
        } else {
            warning("Got unexpected input on 'tsplot.control'. Will be ignored.")
        }
    }

    # Polygons use hex + alpha. Thus, we do have to convert
    # all colors to hex.
    for (var in names(def)) {
        tmp  <- grDevices::col2rgb(def[[var]]$col) / 255
        def[[var]]$col <- colorspace::hex(colorspace::sRGB(tmp[1,], tmp[2,], tmp[3,]))
    }

    # Append wind sector definition (if set)
    if (!is.null(windsector)) def$foehnix_windsector$data <- windsector_convert(windsector)

    # Add custom class and return
    class(def) <- c("tsplot.control", class(def))
    return(def)
}

#' @rdname tsplot.control
tsplot_get_control <- function(x, var, property, args = list()) {
    if (!var %in% names(x)) return(NA) # Not available
    # Return parameters for the plot
    allowed <- list(args_plot    = c("type", "lty", "lwd", "col", "pch", "cex"),
                    args_lines   = c("type", "lty", "lwd", "col", "pch", "cex"),
                    args_polygon = c("type", "lty", "lwd", "col", "pch", "cex", "border"))

    # If asking for "arguments for a specific plot type":
    # Create arguments list used with do.call
    if (grepl("^args_.*", property)) {
        res <- args
        for (n in names(x[[var]])) {
            if (!n %in% allowed[[property]] | n %in% names(res)) next
            res[[n]] <- x[[var]][[n]]
        }
    # Return the value of this property
    } else {
        res <- x[[var]][[property]]
    }
    return(res)
}

#' Time Series Plot of foehnix Models
#' 
#' Development time series plots of estimated \code{\link[foehnix]{foehnix}}
#' models. TODO: they are very specific at the moment!
#' 
#' @param x object of type \code{foehnix} or a list with \code{foehnix} and
#'     univariate \code{zoo} objects, see 'Details'.
#' @param start POSIXt object or an object which can be converted to POSIXt.
#' @param end POSIXt object or an object which can be converted to POSIXt.
#' @param ndays integer, number of days used when looping trough the time series.
#' @param control an object of class \code{\link[foehnix]{tsplot.control}}.
#' @param ... additional arguments forwarded to \code{\link[foehnix]{tsplot.control}}.
#'        can be used to rename varaiables and to change the look of the time series
#'        plot. Please see 'Examples' and the manual of the function
#'        \code{\link[foehnix]{tsplot.control}} for more details.
#' @param ask logical, default is \code{TRUE}.
#' 
#' @details Development method to access plausability of the estimated foehn
#' proabilities.  For software release this method should either be removed or
#' made much more general. At the moment the method heavily depends on the
#' names of the data used as input for \code{\link[foehnix]{foehnix}}.
#'
#' This time series plotting function creates a relatively specific plot
#' and also expects, by default, a set of default variables and variable
#' names. This function uses the data set provided on the \code{data}
#' input argument when calling the \code{\link[foehnix]{foehnix}} method.
#' As they might differ from the \code{foehnix} defaults the
#' \code{varnames} input argument allows to specify custom names.
#' The ones expected:
#' 
#' \itemize{
#'      \item \code{t}: dry air temperature
#'      \item \code{crest_t}: dry air temperature crest
#'      \item \code{rh}: relative humidity (in percent)
#'      \item \code{crest_rh}: relative humidity crest (in percent)
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
#' TODO: describe input 'x' 
#'
#' @examples
#' # Loading demo data for Tyrol (Ellboegen and Sattelberg)
#' data <- demodata("tyrol")
#' filter <- list(dd = c(43, 223), crest_dd = c(90, 270))
#' 
#' # Create foehnix foehn classification model, provide full data
#' # set with all parameters
#' mod1 <- foehnix(diff_t ~ ff + rh, data = data,
#'                 filter = filter, switch = TRUE, verbose = FALSE)
#' 
#' # Create foehnix foehn classification model, provide full data
#' # set with all parameters
#' sub  <- subset(data, select = c(rh, ff, dd))
#' mod2 <- foehnix(ff ~ rh, data = sub, filter = list(dd = c(43, 223)),
#'                 verbose = FALSE)
#' 
#' # Plotting the time series of the a period in 2018
#' tsplot(mod1, start = "2018-03-01", end = "2018-03-10")
#' 
#' # The same for the second model which is based on a subset
#' # of the observation data and only includes 'ff' (wind speed)
#' # 'dd' (wind direction), and 'rh' (relative humitity) of the
#' # target station. Thus, only a subset of all subplots will be shown.
#' tsplot(mod2, start = "2018-03-01", end = "2018-03-10")
#' 
#' # To compare the estimated foehn probabilities of both models,
#' # both 'foehnix' objects can be provided as a list. The plots
#' # are based on the data of the first object (mod1), the probabilities
#' # of the second 'foehnix' model will be added in the last subplot.
#' tsplot(list(mod1, mod2), start = "2018-03-01", end = "2018-03-10")
#' 
#' # If the input is a named list, the names are used for the legend
#' tsplot(list("full model" = mod1, "subset model" = mod2), start = "2018-03-01", end = "2018-03-10")
#' 
#' # The first element in the list has to be a 'foehnix' object,
#' # but as additional inputs univariate time series with probabilities
#' # (in the range [0,1]) can be provided. This allows to compare
#' # 'foehnix' classification against other classification algorithms,
#' # if available.
#' probs <- fitted(mod2) # Time series of probabilities of 'mod2'
#' tsplot(list("full model" = mod1, "zoo" = probs), start = "2018-03-01", end = "2018-03-10")
#' 
#' # Additional arguments can be provided to
#' # - use differt names in the time series plots
#' # - change the look of the plot.
#' # Some examples
#' 
#' # Imagine that the variable names in the data set have the
#' # following names:
#' # - winddir: wind direction
#' # - windspd: wind speed
#' data <- demodata("ellboegen")
#' names(data)[which(names(data) == "dd")] <- "winddir"
#' names(data)[which(names(data) == "ff")] <- "windspd"
#' 
#' # Estimate a foehnix model
#' mod3 <- foehnix(windspd ~ rh, data = data, filter = list(winddir = c(43, 223)), verbose = FALSE)
#' 
#' # The time serie plot expects wind speed and wind direction
#' # to be called 'dd' and 'ff', but they can be renamed. If only
#' # a string is provided (e.g., dd = "winddir") this specifies
#' # the name of the variable in the data set ('data').
#' tsplot(mod3, dd = "winddir", ff = "windspd", start = "2018-03-01", end = "2018-03-10")
#' 
#' # For each element a list can be provided:
#' # - 'name': new name of the variable
#' # - 'color': color used for plotting
#' # - 'label': label for the axis on the plot
#' # See also ?tsplot.control for more information.
#' tsplot(mod3, dd = list(name = "winddir", col = "cyan",    ylab = "WIND DIRECTION LABEL"),
#'              ff = list(name = "windspd", col = "#FF00FF", ylab = "WIND SPEED LABEL"),
#'              rh = list(col = 4, ylab = "RELHUM LABEL"),
#'              t  = list(col = 5, ylab = "TEMPERATURE LABEL"),
#'              prob = list(col = "yellow", ylab = "PROBABILITY LABEL"),
#'              start = "2018-03-01", end = "2018-03-10")
#'
#' @seealso \code{\link[foehnix]{tsplot.control}}.
#'
#' @author Reto Stauffer
#' @export
tsplot <- function(x, start = NULL, end = NULL, ndays = 10,
                   control = tsplot.control(...),
                   ..., ask = TRUE) {


    # Stop if control is not of tsplot.control
    stopifnot(inherits(control, "tsplot.control"))
    stopifnot(inherits(ndays, c("integer", "numeric")))
    if(inherits(ndays, "numeric")) ndays <- as.integer(ndays)
    if(ndays <= 0) stop("invalid argument for \"ndays\"")

    # The function allows that 'x' is either a single
    # foehnix object or a list of foehnix objects (or zoo).
    # The first element in the list has to be a foehnix model,
    # all others can be foehnix models or univariate zoo objects
    # containing probabilities ([0,1]).
    # The next lines do the following:
    # - if 'x' is a foehnix object: set xtra to NULL and continue
    # - if 'x' is a list:
    #   - check if x[[1]] is a foehnix model. If not, stop.
    #   - check that all others, if any, are foehnix or zoo objects.
    #   - if zoo objects are provided: check that they are univariate
    #     and contain values within [0,1]. If not, stop.
    #   - store x[[2]], x[[3]], ... on "xtra", store x[[1]] as x.
    #   - keep names, if there are any (on xtra_names)
    if (inherits(x, "list") && length(x) == 1 ) x <- x[[1]]
    if (inherits(x, "foehnix")) {
        xtra <- NULL; xtra_names <- NULL
    } else {
        if (! inherits(x[[1]], "foehnix"))
            stop("the first element on \"x\" has to be of class \"foehnix\"")
        # Take xtra objects.
        xtra <- list()
        for (i in 2:length(x)) {
            if ( ! inherits(x[[i]], c("zoo", "foehnix")) )
                stop(sprintf("input object %d is neither a zoo object nor a foehnix object", i))
            if ( inherits(x[[i]], "zoo") ) {
                if ( ! is.null(dim(x[[i]])) )
                    stop(sprintf("only univariate zoo objects are allowed (input object %d)", i))
                ##if ( ! all(is.na(x[[i]])) ) { 
                ##    if ( min(x[[i]], na.rm = TRUE) < 0 | max(x[[i]], na.rm = TRUE) > 1 )
                ##        stop(sprintf("Values in (x[[%d]]) have to be within [0,1]!", i))
                ##}
            }
            # All fine? Store
            xtra[[i - 1]] <- x[[i]]
        }
        xtra_names <- names(x)
        # Store the main foehnix object to x
        x <- x[[1]]
    }

    # Probabilities out of range?
    if(!all(is.na(x$prob$prob))) {
        if ( min(x$prob$prob, na.rm = TRUE) < 0 | max(x$prob$prob, na.rm = TRUE) > 1 )
            stop("probabilities outside range (have to be within 0-1)")
    }
    if(inherits(xtra, "list")) {
        range_check <- function(x) {
            if(inherits(x, "foehnix")) x <- x$prob$prob
            if(all(is.na(x))) return(FALSE)
            x <- range(x, na.rm = TRUE)
            return(min(x) < 0 | max(x) > 1)
        }
        tmp <- sapply(xtra, range_check)
        if(any(tmp))
            stop(sprintf("probabilities in input object(s) %s outside range (have to be within 0-1)",
                         paste(which(tmp), collapse = ", ")))
    }


    # Check available data. This allows us to check
    # which of the default subplots can be drawn.
    check <- function(available, control, check) {
        idx <- which(names(control) %in% check)
        if (length(idx) == 0) return(FALSE)
        # Check if we can find $name
        x <- sapply(control[idx], function(x) x$name)
        return(any(x %in% available))
    }
    # List with logicals whether or not we have to plot the panels.
    # Note that the order is important for the calculation of the
    # panel height for layout(...).
    doplot <- list(
        "crest_wind"  = check(names(x$data), control, c("crest_dd", "crest_ff", "crest_ffx")),
        "temp"        = check(names(x$data), control, c("t", "crest_t", "rh")),
        "tempdiff"    = check(names(x$data), control, c("diff_t")),
        "wind"        = check(names(x$data), control, c("dd", "ff", "ffx")),
        "prob"        = TRUE
    )
    Nplots <- sum(sapply(doplot, function(x) return(x)))

    # Convert start/end to POSIXct
    if (! is.null(start)) {
        start <- try(as.POSIXct(start), silent = TRUE)
        if (inherits(start, "try-error"))
            stop("Invalid input for \"start\". Cannot be converted to POSIXt.")
    }
    if (! is.null(end)) {
        end <- try(as.POSIXct(end), silent = TRUE)
        if (inherits(end, "try-error"))
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

    # For convenience:
    get <- tsplot_get_control

    # Looping over the different periods we have to plot
    for (k in seq_along(start)) {

        # Parameters of the graphical output device
        #par(mfrow = c(Nplots, 1), ask = FALSE, mar = rep(0.1, 4),
        #    xaxs = "i", oma = c(4.1, 4.1, 2, 5.1))
        par(ask = FALSE, mar = rep(0.1, 4), xaxs = "i", oma = c(4.1, 4.1, 2, 5.1))
        hgt <- ifelse(names(doplot)[unlist(doplot)] == "tempdiff", 1, 2)
        layout(matrix(1:Nplots, ncol = 1), heights = hgt)
        # Pick subset to plot
        tmp <- window(data, start = start[k], end = end[k])

        # Calculate the limits for the gray boxes (where prob >= .5)
        prob_boxes <- tsplot_calc_prob_boxes(tmp$prob)

        # No data, or only missing data?
        if (nrow(tmp) == 0 | sum(!is.na(tmp)) == 0) {
            tmp <- paste("No data (or only missing values) for the time period",
                         strftime(start[k], "%Y-%m-%d %H:%M"), "to",
                         strftime(end[k], "%Y-%m-%d %H:%M"))
            warning(sprintf("%s. Skip plotting.", str))
            next
        }

        # Crest wind - if required
        if (doplot$crest_wind)  tsplot_add_wind(tmp, control, prob_boxes, TRUE)

        # Air temperature
        if (doplot$temp)        tsplot_add_temp(tmp, control, prob_boxes)
    
        # Plotting temperature difference
        if (doplot$tempdiff)    tsplot_add_tempdiff(tmp, control, prob_boxes)
    
        # Plotting wind direction and wind speed
        if (doplot$wind)        tsplot_add_wind(tmp, control, prob_boxes, FALSE)

        # Foehn prob (main object 'x')
        tsplot_add_foehn(tmp, control, prob_boxes, xtra, xtra_names)

        # If multiple periods have to be plotted: set ask = TRUE
        if (ask & k < length(start)) readline("Press Enter for next plot> ")

    } # End of loop over start/end (loop index k)

}

#' Helper function to add the gray boxes (background)
tsplot_add_boxes <- function(x, col = "gray90") {
    # Loaded from parent env
    if ( length(x$up) > 0 ) {
        y <- par()$usr[3:4]
        for ( i in seq_along(x$up) ) {
            to <- which(x$down >= x$up[i])
            to <- ifelse(length(to) == 0, length(x$index), x$down[to[1L]])
            rect(x$index[x$up[i]] - x$dx, y[1L],
                 x$index[to] + x$dx, y[2L],
                 col = col, border = NA)
        }
    }
}

# Helper function to add vertical lines (midnight)
tsplot_add_midnight_lines <- function(x) {
    ndays <- as.numeric(diff(range(index(x))), unit = "days")
    if ( ndays < 50 ) {
        at <- as.POSIXct(unique(as.Date(index(x))))
        abline(v = at, col = 1)
    }
}


# Calculate the bounds for the gray boxes
# (probability of foehn >= .5)
tsplot_calc_prob_boxes <- function(x) {
    tmp <- as.vector(x > 0.5)
    tmp[is.na(tmp)] <- FALSE
    res <- list(index = index(x), dx = deltat(x) / 2, up = c(), down = c())
    for (i in seq_along(tmp)) {
        # Initial value
        if (i == 1) {
            if (tmp[i]) res$up <- append(res$up, i)
            next
        }
        # Going up
        if (!tmp[i - 1L] & tmp[i]) {
            res$up <- append(res$up, i)
        # Going down
        } else if (!tmp[i] & tmp[i - 1L] | is.na(tmp[i])) {
            res$down <- append(res$down, i - 1)
        }
    }
    return(res)
}


tsplot_add_legend <- function(pos, control, x, crest_x, legend = c("station", "crest")) {
    get <- tsplot_get_control
    legend(pos, ncol = 2, legend = legend, bty = "n",
           col = c(get(control, x, "col"), get(control, crest_x, "col")),
           lty = c(get(control, x, "lty"), get(control, crest_x, "lty")),
           lwd = c(get(control, x, "lwd"), get(control, crest_x, "lwd")),
           pch = c(get(control, x, "pch"), get(control, crest_x, "pch")))
}


#' Helper functions to add data to time series plots
#'
#' Used inside \code{\link[foehnix]{tsplot}}.
#'
#' @param x \code{zoo} object with the data
#' @param control \code{\link[foehnix]{tsplot.control}} object
#' @param prob_boxes object as returned from \code{tsplot_calc_prob_boxes}
#'
#' @rdname tsplot_add
#' @author Reto Stauffer
tsplot_add_temp <- function(x, control, prob_boxes) {
    get <- tsplot_get_control # For convenience
    # If temperature is in the data set: plot temperature,
    # if not, set up an empty plot (required to be able to
    # add relative humidity and temperature differences).
    param_t       <- get(control, "t", "name")
    param_crest_t <- get(control, "crest_t", "name")
    if (any(c(param_t, param_crest_t) %in% names(x))) {
        idx    <- grep(sprintf("^(%s)$", paste(param_t, param_crest_t, sep = "|")), names(x))
        # ylimits for temperatures - will be use once more later on
        ylim_t <- range(x[, idx], na.rm = TRUE)
        plot(NA, ylim = ylim_t, xlim = range(index(x)),
             type = "n", ylab = NA, xaxt = "n", bty = "n")
        tsplot_add_boxes(prob_boxes)
        tsplot_add_midnight_lines(x)
        mtext(side = 2, line = 3, get(control, names(x)[idx[1L]], "ylab"))
        box()
    }

    # Relative humidity
    param_rh       <- get(control, "rh", "name")
    param_crest_rh <- get(control, "crest_rh", "name")
    if (any(c(param_rh, param_crest_rh) %in% names(x))) {
        if (any(c(param_t, param_crest_t) %in% names(x))) par(new = TRUE)
        # Plotting relative humidity data
        plot(NA, type = "n", lwd = 2, yaxt = "n",
             xlim = range(index(x)), ylim = c(0,150), yaxs = "i", xaxt = "n", bty = "n")
        tsplot_add_boxes(prob_boxes)
        tsplot_add_midnight_lines(x)
        # Arguments for the polygon
        if (param_crest_rh %in% names(x))
            do.call(lines, get(control, "crest_rh", "args_lines", list(x = x[, param_crest_rh])))
        if (param_rh %in% names(x))
            do.call(add_polygon, get(control, "rh", "args_polygon", list(x = x[, param_rh])))
        # Adding horizontal lines
        abline(h = seq(20, 100, by = 20), lty = 3,
               col = sprintf("%s50", get(control, "rh", "col")))
        # Labeling
        axis(side = 4, at = seq(20, 100, by = 20))
        tmp <- ifelse(param_rh %in% names(x), param_rh, param_crest_rh)
        mtext(side = 4, line = 3, get(control, tmp, "ylab"))
        box()
    }

    # After relative humidity has been added (optionally)
    # add temperature observation. Either both (temperature at the
    # target station and at the crest) or only one of both.
    if (all(c(param_t, param_crest_t) %in% names(x))) {
        par(new = TRUE)
        do.call(plot, get(control, "t", "args_plot", list(x = x[, param_t], ylim = ylim_t,
                                                          xaxt = "n", yaxt = "n", main = NA)))
        do.call(lines, get(control, "crest_t", "args_lines", list(x = x[, param_crest_t])))
        tsplot_add_legend("topright", control, param_t, param_crest_t)
    } else if (any(c(param_t, param_crest_t) %in% names(x))) {
        # Check which one has to be plotted
        param <- ifelse(param_t %in% names(x), param_t, param_crest_t)
        tmp   <- ifelse(param_t %in% names(x), "t", "crest_t")
        par(new = TRUE)
        do.call(plot, get(control, tmp, "args_plot", list(x = x[, param], ylim = ylim_t,
                                                          xaxt = "n", yaxt = "n", main = NA)))
    }

    # Adding legend for relative humidity
    if (all(c(param_rh, param_crest_rh) %in% names(x)))
        tsplot_add_legend("bottomright", control, param_rh, param_crest_rh)

}


#' @rdname tsplot_add
#' @author Reto Stauffer
tsplot_add_tempdiff <- function(tmp, control, prob_boxes) {
    get <- tsplot_get_control # For convenience
    # Temperature difference
    param <- get(control, "diff_t", "name")
    if (param %in% names(tmp)) {
        plot(tmp[,param], type = "n", xaxt = "n", bty = "n")
        tsplot_add_boxes(prob_boxes)
        tsplot_add_midnight_lines(tmp)
        # Call the line plot
        args <- get(control, "diff_t", "args_lines", list(x = tmp[, param]))
        do.call(lines, args)
        # Adding horizontal grid
        abline(h = seq(-20,20, by = 1), col = "gray80", lty = 3)
        abline(h = 0, col = 1)
        # Labeling
        mtext(side = 2, line = 3, get(control, "diff_t", "ylab"))
        box()
    }
}


#' @rdname tsplot_add
#' @author Reto Stauffer
tsplot_add_wind <- function(x, control, prob_boxes, crest = FALSE) {
    get <- tsplot_get_control # For convenience
    # Plot empty frame
    plot(NA, type = "n", xaxt = "n", ylab = "", xlim = range(index(x)),
             ylim = c(0, 360), yaxt = "n", bty = "n")

    # Adding windsector highlights if there are any.
    tsplot_add_boxes(prob_boxes)

    # Find the wind direction parameters in the data set
    var_dd         <- ifelse(crest, "crest_dd",  "dd")
    var_ff         <- ifelse(crest, "crest_ff",  "ff")
    var_ffx        <- ifelse(crest, "crest_ffx", "ffx")
    # Adding wind speed
    param_dd       <- get(control, var_dd,  "name")
    param_ff       <- get(control, var_ff,  "name")
    param_ffx      <- get(control, var_ffx, "name")

    # Plotting wind direction and wind sectors (if set).
    # Sectors are only plotted for the station (not crest station)
    if (param_dd %in% names(x)) {
        ws     <- get(control, "foehnix_windsector", "data")
        cex    <- get(control, var_dd, "cex")
        cex_dd <- rep(cex, nrow(x))
        if (!is.null(ws) & !crest) {
            for (i in seq_along(ws)) {
                cex_dd[x[, param_dd] >= ws[[i]][1L] & x[, param_dd] <= ws[[i]][2L]] <- cex * 2
                rect(min(index(x)), ws[[i]][1L], max(index(x)), ws[[i]][2L],
                     col    = sprintf("%s30", get(control, "foehnix_windsector", "col")),
                     border = get(control, "foehnix_windsector", "lty"))
                if (!is.null(names(ws)[i]))
                    text(min(index(x)) + 0.01 * diff(range(index(x))),
                         max(ws[[i]]), names(ws)[i], adj = c(0, 1.5))
            }
        }
        args <- get(control, var_dd, "args_lines", list(x = x[, param_dd], cex = cex_dd))
#        args$cex <- cex_dd
        do.call(lines, args)

        #do.call(lines, get(control, var_dd, "args_lines", list(x = x[, param_dd], cex = cex_dd)))
    }

    # Adding vertical lines
    tsplot_add_midnight_lines(x)

    # Adding wind direction
    if (param_dd %in% names(x)) {
        do.call(lines, get(control, var_dd, "args_lines", list(x = x[, param_dd])))
        axis(side = 2, at = seq(90, 360 - 90, by = 90))
        mtext(side = 2, line = 3, get(control, var_dd, "ylab"))
    }
    
    # For convenience
    if (any(c(param_ff, param_ffx) %in% names(x))) {

        # Get y limits
        idx <- grep(sprintf("^(%s)$", paste(param_ff, param_ffx, sep = "|")), names(x))
        ylim_ff <- max(x[, idx], na.rm = TRUE) * c(0, 1.05)

        # Valid limits?
        if (all(is.finite(ylim_ff))) {
            # y-axis labels
            ylab <- c()
            for (var in c(var_ff, var_ffx)) ylab <- append(ylab, get(control, var, "ylab"))
            ylab <- paste(ylab, collapse = "\n")
    
            # Plotting an empty sub-figure
            par(new = TRUE)
            plot(NA, type = "n", yaxs = "i", yaxt = "n", xaxt = "n",
                 xlim = range(index(x)), ylim = ylim_ff)
    
            # Adding horizontal lines
            abline(h = pretty(ylim_ff), lty = 3,
                   col = sprintf("%s50", get(control, var_ff, "col")))
    
            # Plotting wind gusts (first), and wind speed (second)
            if (param_ffx %in% names(x))
                do.call(lines,       get(control, var_ffx, "args_lines", list(x = x[, param_ffx])))
            if (param_ff %in% names(x))
                do.call(add_polygon, get(control, var_ff,  "args_polygon", list(x = x[, param_ff])))
    
            # Adding labels
            axis(side = 4, at = pretty(ylim_ff))
            mtext(side = 4, line = ifelse(grep("\n", ylab), 4, 3), ylab)
    
            box()
        } # End "is.finite"
    }
}


#' Add foehn probabilities to time series plot
tsplot_add_foehn <- function(x, control, prob_boxes, xtra = NULL, xtra_names = NULL, ...) {

    get <- tsplot_get_control # For convenience

    # Limits
    xlim <- range(index(x))
    plot(NA, type = "n", xlim = xlim, xaxt = "n",
         ylab = NA, ylim = c(-4,104), yaxs = "i") 
    axis(side = 1, at = pretty(xlim), strftime(pretty(xlim), "%Y-%m-%d %H:%M"))
    tsplot_add_boxes(prob_boxes)
    tsplot_add_midnight_lines(x)

    # Adding additional foehn probs
    if (! is.null(xtra)) {
        for (i in seq_along(xtra)) {
            tmp_xtra <- if (inherits(xtra[[i]], "foehnix")) xtra[[i]]$prob$prob else xtra[[i]]
            lines(100 * window(tmp_xtra, start = min(index(x)), end = max(index(x))),
                  col = "black", lty = i + 1)
        }
    }
    abline(h = seq(0, 100, by = 20), col = "gray", lty = 3)
    mtext(side = 2, line = 3, get(control, "prob", "ylab"))
    do.call(add_polygon, get(control, "prob", "args_polygon", list(x = x$prob * 100,
                                                                   lower.limit = -4)))

    # Adding RUG
    at <- index(x$prob)[which(x$prob >= .5)]
    if (length(at) > 0) axis(side = 1, at = at, labels = NA,
                               col = get(control, "prob", "col"))

    # Adding "missing data" RUG
    at <- index(x$prob)[which(is.na(x$prob))]
    if (length(at) > 0) axis(side = 1, at = at, labels = NA, col = "gray50")

    # Adding extra probabilities
    if (!is.null(xtra)) {
        if (is.null(xtra_names))
            xtra_names <- c("foehnix", sprintf("extra %d", seq(1, length(xtra))))
        cols <- c(get(control, "prob", "col"), rep("gray50", length(xtra)))
        ltys <- c(1, seq_along(xtra) + 1)
        legend("left", bg = "white", col = cols, lty = ltys, legend = xtra_names)
    }

    # Adding a title to the plot
    title <- sprintf("Foehn Diagnosis %s to %s", xlim[1L], xlim[2L])
    mtext(side = 3, outer = TRUE, title, font = 2, cex = 1.2, line = 0.5)
    box()
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
add_polygon <- function(x, col = "#ff0000", lower.limit = 0, ...) {
    # lower.limit is either a vector or a single numeric
    stopifnot(length(lower.limit) == 1 & is.finite(lower.limit))
    # Need hex color
    if (!grepl("^#[A-Za-z0-9]{6}$", col)) stop("Sorry, need hex color definition for polygon plots.")
    # All elements NA?
    if (all(is.na(x))) return(invisible(NULL))
    # Else find valid blocks and plot them. Start with 1
    i <- 1
    while (i <= length(x)) {
        if (all(is.na(x))) break
        i1 <- min( which( !is.na(x) ) )
        if (i1 > 1) { x <- x[-c(seq(1,i1-1))]; i1 <- 1 }
        # Else check first NA
        if (!any(is.na(x))) { i2 <- length(x) } else { i2 <- min( which( is.na(x) ) ) - 1 }
        # Create x/y coordinate vectors for the polygon function
        p_x <- as.numeric(zoo::index(x[i1:i2])); p_x <- c(p_x,max(p_x),min(p_x))
        p_y <- c(as.numeric(x[i1:i2]),lower.limit, lower.limit )
        # Plotting
        polygon(p_x, p_y, col = sprintf("%s20",col), border = NA)
        lines(x[i1:i2],   col = col, ...)
        # Remove plotted data from time series and continue
        x <- x[-c(i1:i2)]
    }

}

