

# TODO: Neither well coded, nor tested, nor documented.
#' @import zoo
shiny_tsplot <- function(output, x, start, end) {

    requireNamespace("shiny")
    tmp <- merge(window(x$prob, start = start, end = end),
                 window(x$data, start = start, end = end))
    names(tmp)[1L] <- "prob"

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
            p_x <- as.numeric(index(x[i1:i2])); p_x <- c(p_x,max(p_x),min(p_x))
            p_y <- c(as.numeric(x[i1:i2]),lower.limit, lower.limit )
            polygon( p_x, p_y, col = sprintf("%s20",col), border = NA )
            lines( x[i1:i2],   col = col, lwd = lwd )
            # Remove plotted data from time series and continue
            x <- x[-c(i1:i2)]
        }
    
    }
    add_boxes <- function(x, col = "gray90") {
        dx  <- as.numeric(diff(index(x)[1:2]), unit = "secs") / 2
        up   <- which(diff(x >= .5) == 1) + 1
        down <- which(diff(x >= .5) == -1)
        if ( length(up) == 0 | length(down) == 0 ) return()
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



    # -----------------------------------------------------------
    ts_trh_plot <- function(x, ...) {
        par(mar = c(2.1, 4.1, 0.1, 4.1))
        plot(x$t, type = "n", xlim = range(index(x)), xaxs = "i",
             xlab = NA, ylab = NA, main = NA)
        mtext(side = 2, line = 3, "temperature")
        add_boxes(x$prob)
        add_midnight_lines(x)
        lines(x$t, col = 2, lwd = 2)
        ##
        par(new = TRUE)
        plot(NA, xlim = range(index(x)), ylim = c(0, 180), xaxs = "i", yaxs = "i",
             xaxt = "n", yaxt = "n", xlab = NA, ylab = NA, main = NA)
        add_polygon(x$rh, col = "#003300")
        abline(h = seq(20, 100, by = 20), lty = 5, col = "#003300")
        axis(side = 4, at = seq(20, 100, by = 20))
        mtext(side = 4, line = 3, "rel humidity")
    }
    output$ts_trh <- shiny::renderPlot(ts_trh_plot(tmp), height = 300, width = 1400)

    # -----------------------------------------------------------
    ts_dd_plot <- function(x, ...) {
        par(mar = c(2.1, 4.1, 0.1, 4.1))
        plot(x$ff, type = "n", xlim = range(index(x)), xaxs = "i", yaxs = "i",
             xlab = NA, ylab = NA, main = NA, ylim = c(0, max(15, max(x$ff, na.rm = TRUE))) * 1.05)
        mtext(side = 2, line = 3, "wind speed")
        add_boxes(x$prob)
        add_polygon(x$ff, col = "#0033CC")
        add_midnight_lines(x)
        abline(h = seq(5, 25, by = 5), lty = 5, col = "#0033CC")
        ####
        par(new = TRUE)
        plot(NA, xlim = range(index(x)), xaxs = "i", yaxs = "i", ylim = c(0, 360),
             xaxt = "n", yaxt = "n", xlab = NA, ylab = NA, main = NA)
        add_midnight_lines(x)
        points(x$dd, col = 1, lwd = 2)
        axis(side = 4, at = seq(60, 300, by = 60)) 
        mtext(side = 4, line = 3, "wind dir")
    }
    output$ts_dd <- shiny::renderPlot(ts_dd_plot(tmp), height = 300, width = 1400)

    # -----------------------------------------------------------
    ts_prob_plot <- function(x, ...) {
        par(mar = c(2.1, 4.1, 0.1, 4.1))
        plot(NA, type = "n", xlim = range(index(x)), ylim = c(-2.5, 102.5),
             xaxs = "i", yaxs = "i", xlab = NA, ylab = NA, main = NA, xaxt = "n", yaxt = "n")
        axis(side = 2, at = seq(25, 75, by = 25))
        mtext(side = 2, line = 3, "foehn probability")
        add_polygon(x$prob * 100, col = "#CC3300", lower.limit = -5)
        abline(h = seq(25, 75, by = 25), lty = 5, col = "#CC3300")
        add_midnight_lines(x)
    }
    output$ts_prob <- shiny::renderPlot(ts_prob_plot(tmp), height = 300, width = 1400)
}


#' Shiny Application to Check foehnix Classification (Time Series Plots)
#'
#' TODO: Neither well coded, nor tested, nor documented.
#'
#' @param x a \code{\link{foehnix}} mixture model object.
#' @param ... additional arguments, not in use.
#'
#' @import zoo
#' @author Reto Stauffer
shiny_check <- function(x, ...) {

    requireNamespace("shiny")

    # Click string
    xy_str <- function(e) {
        if ( is.null(e) ) return("NULL\n")
        sprintf("x = %.1f, y = %.1f", e$x, e$y)
    }
    tsplot_click_event <- function(x, e) {
        # Find closest index
        closest <- index(x$prob)[which.min(abs(as.numeric(index(x$prob)) - e$x))]
        shiny::showNotification(sprintf("Closest is %s", as.character(closest)))
    }
    # User interface
    ui <- shiny::basicPage(
        shiny::tags$head(
            shiny::tags$style(shiny::HTML("
            div.shiny-plot-output {
                height: auto !important;
            }
            "))
        ),
        shiny::titlePanel("foehnix time series check"),
        shiny::plotOutput("ts_trh",   click = "tsplot_click", brush = "tsplot_brush"),
        shiny::plotOutput("ts_dd",    click = "tsplot_click", brush = "tsplot_brush"),
        shiny::plotOutput("ts_prob",  click = "tsplot_click", brush = "tsplot_brush"),
        shiny::verbatimTextOutput("info"),
        shiny::plotOutput("plot2")
    )

    
    # Shiny server function
    server <- function(input, output) {
        #output$tsplot <- shiny::renderPlot({
        #    shiny_tsplot(x, start = "2010-02-01", end = "2010-02-04")
        #}, height = 800, width = 1200)
        shiny_tsplot(output, x, start = index(x$prob)[1L], end = index(x$prob)[500L])
    
        output$info <- shiny::renderText({
            paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
        })

        shiny::observeEvent(input$tsplot_click, {
            tsplot_click_event(x, input$tsplot_click)
        })
        shiny::observeEvent(input$tsplot_brush, {
            shiny::showNotification("brush event")
        })
    }
    shiny::shinyApp(ui, server)
}



