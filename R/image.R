
#' foehnix Image Plot - Hovmoeller Diagram
#'
#' The default \code{\link{image}} plot of a \code{\link{foehnix}} object
#' is a Hovmoeller diagram.
#'
#' @param x object of class \code{\link{foehnix}}.
#' @param FUN character string or a custom aggregation function. See 'Details'
#'        section for more information.
#' @param deltat integer, interval in seconds for the time of day axis. Has to be
#'        a fraction of 86400 (24 hours in seconds). It \code{NuLL} (default) the
#'        interval of the time series object will be used.
#' @param deltad integer, similar to \code{deltat}, the interval in days for the
#'        grid on the x-axis. Default is \code{7L} (aggregate to weekly values).
#' @param col vector of colors forwarded to \code{image.default}. By default
#'        a gray scale color map is used.
#' @param contours logical \code{TRUE} or \code{FALSE}, whether or not the
#'        concours should be added.
#' @param contour.col color for the contour lines, only used if \code{contours = TRUE}.
#' @param ... additional arguments (see 'Details' section).
#'
#' @details Plotting a Hovmoeller diagram based on the \code{\link{zoo}} time
#' series object of the \code{\link{foehnix}} classification. Different plot
#' types are available. The default functions (see list below) use \code{na.rm = TRUE}.
#'
#' Input \code{FUN} can be one of the following character strings:
#' \itemize{
#'    \item \code{freq}: plotting frequencies (default).
#'    \item \code{mean}: mean probability.
#'    \item \code{occ}: plotting occurrence of foehn (where probability \code{>= 0.5}).
#'    \item \code{noocc}: contrary to \code{occ}: occurrence of no foehn (probability \code{< 0.5}).
#' }
#'
#' \code{FUN} can also be a custom function used for time series aggregation
#' (see \code{\link{aggregate.zoo}}).
#'
#' Additional arguments which can be set:
#' \itemize{
#'      \item \code{xlim}: limits of abscissa. Numeric vector of length 2 with
#'            Julian days (0 - 365). If the values are decreasing (e.g., \code{xlim = c(300, 100)})
#'            the abscissa will be adjusted to show continuous data around new years eve.
#'      \item \code{ylim}: limits of ordinate. Numeric vector of length 2 with
#'            seconds (seconds of the day; 0 = 00:00:00 UTC, 86400 = 24:00:00 UTC).
#'      \item \code{xlab}, \code{ylab}, \code{main}: axis labels and title of the plot.
#' }
#' 
#' @importFrom grDevices gray.colors
#' @name image
#' @export
image.foehnix <- function(x, FUN = "freq", deltat = NULL, deltad = 7L,
                          col = rev(gray.colors(20)), contours = FALSE,
                          contour.col = "black", ...) {

    stopifnot(inherits(x, "foehnix"))

    # Some arguments for the plot
    arg <- list(...)
    xlab = if (!"xlab" %in% names(arg)) arg$xlab <- "time of the year"
    ylab = if (!"ylab" %in% names(arg)) arg$ylab <- "time of the day"
    main = if (!"main" %in% names(arg)) arg$main <- "foehnix Hovmoeller Diagram"
    xlim = if (!"xlim" %in% names(arg)) arg$xlim <- c(0, 364)
    ylim = if (!"ylim" %in% names(arg)) arg$ylim <- c(0, 86400)

    # Checking x limits
    stopifnot(is.numeric(arg$xlim) & is.finite(arg$xlim) & length(arg$xlim) == 2L)
    if (any(arg$xlim < 0 | arg$xlim >= 365))
        stop(sprintf("\"xlim\" need to be within %d to %d", 0, 365))
    if (any(arg$ylim < 0 | arg$ylim > 86400))
        stop(sprintf("\"ylim\" need to be within %d to %d", 0, 86400))

    # If "reversed" (e.g., xlim = c(300, 100)) we fix this by
    # setting the limits to c(300 - 364, 100) (to the left)
    arg$xlim <- pmin(364, arg$xlim) # set 365 to 364
    if (diff(arg$xlim) < 0) arg$xlim <- c(arg$xlim[2L], arg$xlim[1L] - 364) # "Revert"

    # Extend zoo object if needed (inflation)
    x <- x$prob
    stopifnot(is.regular(x, strict = TRUE))
    index(x) <- as.POSIXct(index(x))

    # Checking deltat argument
    if (is.null(deltat)) {
        deltat <- as.numeric(diff(index(x)[1:2]), unit = "secs")
    } else {
        stopifnot(is.finite(deltat))
        stopifnot(deltat > 0)
    }
    if (!round(86400 / deltat) * deltat == 86400)
        stop(sprintf("deltat = %d is not a fraction of 86400 (one day in seconds).", deltat))

    # Checking deltad
    stopifnot(inherits(deltad, c("integer", "numeric")))
    stopifnot(deltad <= 365)
    deltad <- as.integer(deltad)
    if (deltad < 1) stop("\"deltad\" has to be a positive integer.")

    # Checking colors
    stopifnot(inherits(col, "character"))
    stopifnot(length(col) > 1)

    # Aggregation function
    FUN_allowed <- c("freq", "mean", "occ", "noocc")
    if (is.character(FUN)) {
        FUN <- match.arg(FUN, FUN_allowed)
        if ( FUN == "mean" ) {
            FUN <- function(x) mean(x, na.rm = TRUE)
        } else if (FUN == "occ") {
            FUN <- function(x) sum(x >= 0.5, na.rm = TRUE)
        } else if (FUN == "noocc") {
            FUN <- function(x) sum(x <  0.5, na.rm = TRUE)
        } else if (FUN == "freq") {
            FUN <- function(x) sum(x >= 0.5, na.rm = TRUE) / sum(!is.na(x))
        }
    } else if (!is.function(FUN)) {
        stop("input \"FUN\" has to be a function or a character, one of ",
             paste(FUN_allowed, collapse = ", "))
    }

    # Add information about time and day of the year
    to_longform <- function(x, breaks.time, breaks.date) {
        # Convert zoo index to POSIXlt once
        lt   <- as.POSIXlt(index(x))

        # Seconds of this day
        time <- as.numeric(index(x)) - as.numeric(as.Date(index(x))) * 86400
        time <- ifelse(time == 0, 86400, time) # Set 00:00:00 to 24:00:00

        # Zero-based Julian day
        yday <- ifelse(time == 86400, lt$yday - 1, lt$yday) # Move 24:00 the day before
        yday <- ifelse(yday < 365, yday, 0)                 # Fix 365 (leap year)
        yday <- ifelse(yday >=  0, yday, 364)   # If moved to the day bevore (see above): fix
        res <- data.frame(hash_time = cut(time, breaks.time, include.lowest = FALSE,
                                          ordered = TRUE, dig.lab = 7),
                          hash_date = cut(yday, breaks.date, include.lowest = TRUE,
                                          ordered = TRUE, dig.lab = 7))

        # Add combined hash
        res$hash <- sprintf("%s_%s", res$hash_date, res$hash_time)
        return(res)
    }


    breaks.time <- seq(0, 86400, by = deltat)
    breaks.date <- unique(c(seq(0, 364, by = deltad), 364))
    data <- cbind(as.data.frame(x), to_longform(x, breaks.time, breaks.date))
    data <- na.omit(data)
    stopifnot(nrow(data) > 0)


    # Aggregate information
    agg <- aggregate(data$prob, by = list(data$hash), FUN = FUN)
    names(agg) <- c("hash", "value")

    # Adding date and time hash after aggregation
    fun <- function(x) as.character(strsplit(x, "_")[[1L]])
    agg <- cbind(agg, structure(as.data.frame(t(sapply(agg$hash, fun)),
                                              stringsAsFactors = FALSE),
                                names = c("hash_date", "hash_time")))

    # Convert values to colors for the plot.
    get_color <- function(x, col, zlim = NULL) {
        # Calculate color ID
        if (is.null(zlim)) {
            cID <- (x - min(x, na.rm = TRUE)) / max(x - min(x, na.rm = TRUE), na.rm = TRUE)
        } else {
            cID <- (x - min(zlim, na.rm = TRUE)) / max(zlim, na.rm = TRUE)
        }
        cID <- as.integer(round(cID * (length(col) - 1)) + 1)
        cID[which(cID < 1 | cID > length(col))] <- NA
        # Return a vector of colors
        col[cID]
    }
    agg$color <- get_color(agg$value, col, zlim = arg$zlim)

    # Draw plot
    hold <- par(no.readonly = TRUE); on.exit(par(hold))

    # Using layout to draw the diagram plus a legend. First we 
    # need to know the dimension of the image such that we can
    # adjust our layout:
    imgsize = structure(as.list(par()$pin), names = c("width", "height"))

    # Setting the parameters for the plot
    if ( inherits(arg$main, "character") ) {
        oma_top <- length(strsplit(arg$main, "\n")[[1]]) + 2
    } else { oma_top <- 2; }
    par(mar = rep(.3, 4), oma = c(4.1, 4.1, 3, 3))
    layout(matrix(1:2, nrow = 1), widths = c(imgsize$width - 0.5, 0.5))

    # Create an empty plot with the extend we need, namely
    # 0 - 354 (0 based Julian day) along the x-axis, and
    # 0 - 86400 (one full day in seconds) along the y-axis.
    plot(NA, bty = "n",
         xlim = sort(arg$xlim), xaxt = "n", xaxs = "i",
         ylim = arg$ylim,       yaxt = "n", yaxs = "i",
         xlab = NA, ylab = NA, main = NA)
    mtext(side = 1, line = 3.0, arg$xlab)
    mtext(side = 2, line = 3.3, arg$ylab)

    # Convert levels of the time hash. Convert format.
    matrix_template <- matrix(NA, nrow = length(levels(data$hash_time)),
                              ncol = length(levels(data$hash_date)),
                              dimnames = list(levels(data$hash_time), levels(data$hash_date)))

    # Restructure 'agg' and add values to 'vmat'
    df_to_matrix <- function(x, mat, var) {
        x <- x[, grep(sprintf("^(hash_date|hash_time|%s)$", var), names(x))]
        x <- reshape(x, timevar = "hash_date", direction = "wide", idvar = "hash_time")
        x <- structure(as.matrix(x[, -1L]), dimnames = list(x$hash_time,
                       regmatches(names(x)[-1], regexpr("[\\[\\(].*$", names(x)[-1]))))
        mat[match(rownames(x), rownames(mat)), match(colnames(x), colnames(mat))] <- x
        return(mat)
    }

    # x: matrix with values
    # y: matrix with colors
    matrix_to_df <- function(x, y) {
        # Check that x/y match
        stopifnot(identical(dim(x), dim(y)))
        stopifnot(identical(rownames(x), rownames(y)))
        stopifnot(identical(colnames(x), colnames(y)))

        # Extract x/y limits from row and column names
        fun <- function(x) {
            x <- strsplit(regmatches(x, regexpr("[0-9]+\\,[0-9]+", x, perl = TRUE)), ",")[[1L]]
            data.frame(from = as.integer(x[1L]),
                       mid  = mean(as.integer(x)),
                       to   = as.integer(x[2L]))
        }
        rows <- do.call(rbind, lapply(rownames(x), fun))
        cols <- do.call(rbind, lapply(colnames(x), fun))

        # Generate final data.frame with coordinates and values
        res  <- data.frame(ymin  = rep(rows$from, times = ncol(x)),
                           ymid  = rep(rows$mid,  times = ncol(x)),
                           ymax  = rep(rows$to,   times = ncol(x)),
                           xmin  = rep(cols$from, each  = nrow(x)),
                           xmid  = rep(cols$mid,  each  = nrow(x)),
                           xmax  = rep(cols$to,   each  = nrow(x)),
                           value = as.vector(x),
                           color = as.vector(y),
                           stringsAsFactors = FALSE)
        # Return: "df" data.frame for rectangles, NA's will be removed.
        # "rows": row-interval information, "cols": column interval information.
        return(list(df = na.omit(res), rows = rows, cols = cols))
    }

    # Adding the data (rectangles), the aggregated data.
    vmat <- df_to_matrix(agg, matrix_template, "value")
    cmat <- df_to_matrix(agg, matrix_template, "color")
    x    <- matrix_to_df(vmat, cmat)

    rect(x$df$xmin, x$df$ymin, x$df$xmax, x$df$ymax, border = NA, col = x$df$color)
    if (min(arg$xlim) < 0)
        rect(x$df$xmin - 364, x$df$ymin, x$df$xmax - 364, x$df$ymax, border = NA, col = x$df$color)

    # Adding y-axis (time)
    yat <- seq(0, 86400, by = 3600)
    ylab <- strftime(as.POSIXct(yat, origin = "1970-01-01"), "%H:%M")
    axis(side = 2, at = yat, labels = ylab, las = 1)

    # Adding x-axis (date)
    get_xticks <- function() {
        # Ticks - first of month
        tick <- as.POSIXlt(sprintf("2016-%02d-01", 1:12))
        # Labels - mid month
        labs <- as.POSIXlt(sprintf("2016-%02d-15", 1:12))
        res  <- data.frame(tick   = rep(tick$yday, 3) + rep(c(-364, 0, 364), each = length(tick)) - 0.5,
                           labelx = rep(labs$yday, 3) + rep(c(-364, 0, 364), each = length(labs)) - 0.5,
                           label  = rep(strftime(labs, "%b"), 3))
        return(res)
    }
    xat <- get_xticks()
    axis(side = 1, at = xat$tick, labels = NA)
    axis(side = 1, at = xat$labelx, labels = xat$label, tick = FALSE,
         las = ifelse(par()$pin[1L] < 6, 2, 1))

    # If the user wants to have contour lines: draw contours.
    if (contours) {

        # Extend data and dimensions for countour plot
        tmpx <- c(x$cols$mid -   364, x$cols$mid, x$cols$mid +   364)
        tmpy <- c(x$rows$mid - 86400, x$rows$mid, x$rows$mid + 86400)

        # Replicates the matrix 'x' on a 3x3 extended tile.
        # Used later on, required to create cyclic boundaries for the
        # contour plot.
        extend_matrix <- function(x) {
            x <- do.call(rbind, replicate(3, x, simplify = FALSE))
            x <- do.call(cbind, replicate(3, x, simplify = FALSE))
            return(x)
        }

        # Adding contour plot
        contour(x = tmpx, y = tmpy, z = t(extend_matrix(vmat)),
                add = TRUE, col = contour.col, ...)
    }

    # Drawing the outline/box
    box()

    # Drawing the legend
    draw_legend <- function(x, col, zlim = NULL) {

        if ( is.null(zlim) ) {
            at <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length = length(col))
        } else {
            at <- seq(min(zlim, na.rm = TRUE), max(zlim, na.rm = TRUE), length = length(col))
        }
        # Loading colors
        colors <- get_color(at, col, zlim = NULL)
        # Draw the color map
        image(y = at,
              z = matrix(1:length(colors), nrow = 1), col = colors,
              xaxt = "n", xaxs = "i", xlim = c(0,1),
              yaxt = "n", yaxs = "i", ylim = range(at))

        # Adding legend
        axis(side = 4, las = 2, at = pretty(at))
        box()
    }
    draw_legend(agg$value, col, arg$zlim)

    # Adding title
    mtext(side = 3, line = .5, font = 2, cex=  1.2, outer = TRUE, arg$main)

    # That's the end, my friend ...
    # Return some properties (insisibile), mainly for testing.
    invisible(list(agg = agg,
                   vmat = vmat,
                   cmat = cmat,
                   xlab = xlab,
                   ylab = ylab,
                   zlim = arg$zlim,
                   breaks.time = breaks.time,
                   breaks.date = breaks.date,
                   control = list(FUN = FUN,
                                  deltat = deltat,
                                  deltad = deltad,
                                  contours = contours,
                                  contour.col = contour.col)))

}






