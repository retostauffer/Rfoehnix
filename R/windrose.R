# -------------------------------------------------------------------
# - NAME:        windrose.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-16
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-17 19:59 on marvin
# -------------------------------------------------------------------

windrose <- function(x, ...) UseMethod("windrose")
windrose.foehnix <- function(x, ...) {
    print("oh, a windrose for a foehnix object .... #TODO code it!")
}

#TODO: Very specific function (just a copy of the Luzern analysis).
# Usage needs to be changed drastically.
windrose.default <- function(x, interval = 30, ff = NULL, circle = seq(0,1,by=0.02),
    windsextor = NULL, main = NULL, draw.gusts = FALSE ) {
    if ( ! floor(360/interval)*interval == 360 ) stop("interval wrong")

    if ( draw.gusts ) {
        stopifnot( "ffx" %in% names(x) )
        x  <- na.omit(subset(x, select=c(ffx,dd)))
        names(x)[1L] <- "ff" #### FAKING NAME
    } else {
        x  <- na.omit(subset(x, select=c(ff,dd)))
    }

    # Remove dd==0 & ff==0 observations
    idx <- which(x$dd == 0 & x$ff == 0)
    if ( length(idx) > 0 ) x <- x[-idx,]

    # Breaks for classification
    if ( is.null(ff) ) ff <- quantile(x$ff, seq(0, 1, length = 5))
    if ( max(ff) < Inf ) ff <- c(ff,max(x$ff,na.rm=TRUE))
    if ( min(ff) > 0   ) ff <- c(0,ff)

    # Centering
    x$dd <- ifelse(x$dd > (360 - interval/2), x$dd - 360, x$dd)
    dd.breaks <- seq(-interval/2,360,by=interval)

    ff.breaks <- unique(ff)
    tab <- xtabs( ~ cut(x$dd, dd.breaks, include.lowest = TRUE) +
                    cut(x$ff, ff.breaks, include.lowest = TRUE) )
    tab <- t(apply(tab, 1, cumsum)) / nrow(x)

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

    # Transform windsextor, if not null
    if ( ! is.null(windsextor) ) {
        windsextor <- apply(matrix(c(10,10,windsextor), ncol = 2, byrow = FALSE),
                     1, function(x) rotate(x[1L], x[2L]))
    }


    # Title
    if ( is.null(main) ) main <- "Windrose"

    # Start plotting down here
    lim <- max(abs(tab), na.rm = TRUE)
    hold <- par(no.readonly = TRUE); on.exit(par(hold))
    par(mar = c(2,2,4,2) )
    plot(0, type = "n", xlim = c(-1,1)*lim*1.05, ylim = c(-1,1)*lim*1.05,
         xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
         xlab = NA, ylab = NA, main = NA, asp = 1)
    mtext(side = 3, line = 2, main, font = 2, cex = 1.2)

    # If secotr is set: colorize
    if ( ! is.null(windsextor) ) {
        polygon( c(0,windsextor[1,1],windsextor[2,1],0),
                 c(0,windsextor[1,2],windsextor[2,2],0), col = "gray90", border = NA)
    }


    # Adding wind direction labels
    lines(c(0,0), c(-lim,lim))
    lines(c(-lim,lim), c(0,0))
    axis(side = 1, at = 0, label = "S", las = 1)
    axis(side = 2, at = 0, label = "W", las = 1)
    axis(side = 3, at = 0, label = "N", las = 1)
    axis(side = 4, at = 0, label = "E", las = 1)

    require("colorspace")
    if ( ! draw.gusts ) h = c(265,80) else h <- c(-100,80)
    cols <- rev(heat_hcl(ncol(tab), h = h, c. = c(60, 10),
                         l = c(25, 95), power = c(0.7,2)))

    dd <- dd.breaks[-1] - diff(dd.breaks[1:2])/2
    for ( i in ncol(tab):1 ) { 
        tmp <- t(apply(cbind(tab[,i], dd), 1, function(x) rotate(x[1L], x[2L])))
        tmp <- rbind(tmp,tmp[1,])
        polygon(tmp[,1], tmp[,2], col = cols[i], border = NA)
    }

    # Draw circles
    for ( i in circle ) { if ( i == 0 ) next else drawCircle(i) }

    # If windsextor is set: draw windsextor
    if ( ! is.null(windsextor) ) {
        lines(c(0,windsextor[1,1]), c(0,windsextor[1,2]))
        lines(c(0,windsector[2,1]), c(0,windsector[2,2]))
    }
    legend("bottomleft", bty = "n", legend = sprintf("N = %d", nrow(x)))

    # Adding legend
    legend("topleft", fill = cols, legend = colnames(tab), bg = "white",
          title = "Wind speed [m/s]")
    box()

}
