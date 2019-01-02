# -------------------------------------------------------------------
# - NAME:        foehnix_filter.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-19
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-19, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-21 21:27 on marvin
# -------------------------------------------------------------------


#' Evaluates Data Filter Rules for foehnix Mixture Moel Calls.
#' 
#' \code{\link{foehnix}} models allow to specify an optional
#' \code{foehnix_filter}. If a filter is given only a subset
#' of the data set provided to \code{\link{foehnix}} is used
#' for the foehn classification.
#' 
#' A typical example is a wind direction filter such that
#' only observations (times) are used where the observed
#' wind direction was within a user defined wind sector
#' corresponding to the wind direction during foehn events
#' for a specific location.
#' 
#' However, the filter option allows to even implement complex
#' filter rules if required. The 'Details' section contains
#' further information and examples how this filter rules can
#' be used.
#' 
#' @param x object of class \code{zoo} or \code{data.frame} containing the
#'        observations.
#' @param filter can be \code{NULL} (no filter applied), a 
#'        function operating on \code{x}, or a named list with a simple
#'        filter rule (\code{numeric} of length two) or custom filter
#'        functions. Details provided in the 'Details' section.
#' @return Returns a vector of integers corresponding to those rows in
#' the data set \code{x} which fulfill all filte criteria. If input
#' \code{filter = NULL} an integer vector \code{1:nrow(x)} is returned.
#' 
#' @details Foehn winds often (not always) show a very specific wind direction
#' due to the canalization of the air flow trough the local topography. The
#' \code{\link{foehnix_filter}} option allows to subset the data according to a
#' user defined set of filters from simple filters to complex filters.
#'
#' These filters classify each observation (each row in \code{x}) as
#' good (within filter), bad (outside filter), and ugly (at least one
#' variable required to apply the filter was \code{NA}).
#' 
#' No filter: If \code{filter = NULL} no filter will be applied and the whole
#' data set provided is used to do the foehn classification (all observations
#' will be treated as 'good').
#' 
#' Simple filter rules: The filter is a named list containing one or several
#' numeric vectors of length 2 with finite numeric values.  The name of the
#' list element defines the column of the data set (input \code{x}), the
#' numeric vector of length 2 the range which should be used to filter the
#' data. This is the simplest option to apply the mentioned wind direction
#' filter. Examples:
#' 
#' \itemize{
#'     \item \code{filter = list(dd = c(43, 223))}: applies the filter to
#'         column \code{x$dd}. The filter classifies observations/rows
#'         as 'good' (within filter) if \code{x$dd >= 43 & x$dd <= 223}.
#'     \item \code{filter = list(dd = c(330, 30)}: similar to the filter
#'         rule above, allows to specify a wind sector going trough 0
#'         (if dd is wind direction in degrees between \code{[0, 360[}).
#'         The filter classifies observations/rows as 'good' (within filter)
#'         if \code{x$dd >= 330 | x$dd <= 30}.
#'     \item \code{filter = list(dd = c(43, 223), crest_dd = c(90, 270)}:
#'         two filter rules, one for \code{x$dd}, one for \code{x$crest_dd}.
#'         The filter classifies observations/rows as 'good' (within filter)
#'         if \code{x$dd >= 43 & x$dd <= 223} AND \code{x$crest_dd >= 330 | x$crest_dd <= 30}.
#'         If an observation/row does not fulfill one or the other rule
#'         the observation/row is classified as 'bad' (outside filter), if
#'         one of \code{x$dd} or \code{x$crest_dd} is \code{NA} the
#'         corresponding observation/row will be classified as 'ugly'.
#'     \item Filters are not restricted to wind direction (as shown in the
#'         examples above)!
#' }
#' 
#' 
#' Custom filter functions: Instead of only providing a segment/sector defined
#' by two finite numeric values (see 'Simple filter' above) a named list of #'
#' functions can be provided. These functions DO HAVE TO return a vector of #'
#' logical values (\code{TRUE} (good),\code{FALSE} (bad), or \code{NA} (ugly)) #'
#' of length \code{nrow{x}}. If not, an error will be thrown.  The function will
#' be applied to the column #' specified by the name of the list element. Some
#' examples:
#' 
#' \itemize{
#'     \item \code{filter = list(dd = function(x) x >= 43 & x <= 223)}:
#'         The function will be applied to \code{x$dd}.
#'         A vector with \code{TRUE}, \code{FALSE}, or \code{NA} is returned for each for
#'         each \code{1:nrow{x}} which takes \code{NA} if \code{is.na(x$dd)},
#'         \code{TRUE} if \code{x$dd >= 43 & x$dd <= 223} and \code{FALSE} else.
#'         Thus, this filter is the very same as the first example in the
#'         'Simple filter' section above.
#'     \item \code{filter = list(ff = function(x) x > 2.0)}:
#'         Custom filter applied to column \code{x$ff}.
#'         A vector with \code{TRUE}, \code{FALSE}, and \code{NA} is returned for each for
#'         each \code{1:nrow{x}} which takes \code{NA} if \code{is.na(x$ff)}, \code{TRUE} if
#'         \code{x$ff > 2.0}, and \code{FALSE} else.
#'     \item \code{filter = list(ff = function(x) \dots, dd = function(x) \dots)}:
#'         two filter functions, one applied to \code{x$ff}, one to \code{x$dd}.
#'         Note that observations/rows will be classified as 'ugly' if one of the
#'         two filters returns \code{NA}. If no \code{NA} is returned the
#'         observation is classified as 'good' if both return \code{TRUE}, and
#'         as 'bad' (outside filter) if at least one returns \code{FALSE}.
#' }
#' 
#' Complex filters: If \code{filter} is a function this filter function will be
#' applied to the full input object \code{x}. This allows to write functions of
#' any complexity. As an example:
#' 
#' \itemize{
#'     \item \code{filter = function(x) (x$dd >= 43 & x$dd <= 223) & x$ff >= 2.0}:
#'         Input \code{x} to the filter function is the object as provided
#'         to the \code{\link{foehnix_filter}} function (\code{x}). Thus,
#'         the different columns of the object can be accessed directly
#'         trough their names (e.g., \code{x$dd}, \code{x$ff}).
#'         A vector of length \code{nrow(x)} with \code{TRUE}, \code{FALSE},
#'         and \code{NA} has to be returned. Only those classified as 'good' (\code{TRUE})
#'         will be used for classification.
#' }
#'             
#' @examples
#' # Loading example data set and conver to zoo time series
#' # time series object.
#' data("ellboegen", package = "foehnix")
#' library("zoo")
#' ellboegen <- zoo(subset(ellboegen, select = -timestamp),
#'                  as.POSIXct(ellboegen$timestamp, origin = "1970-01-01", tz = "UTC"))
#' 
#' # Case 1:
#' # -----------------
#' # Filter for observations where the wind direction is
#' # within 100 - 260 (southerly flow):
#' idx_south <- foehnix_filter(ellboegen, list(dd = c(100, 260)))
#' print(idx_south)
#' 
#' # Same filter but for northerly flows, taking rows with
#' # wind direction observations (dd) smaller than 45 or
#' # larger than 315 degrees:
#' idx_north <- foehnix_filter(ellboegen, list(dd = c(315, 45)))
#' print(idx_north)
#' 
#' par(mfrow = c(1,3))
#' hist(ellboegen$dd,                 xlab = "dd", main = "all observations")
#' hist(ellboegen$dd[idx_south$good], xlab = "dd", main = "southerly winds")
#' hist(ellboegen$dd[idx_north$good], xlab = "dd", main = "northerly winds")
#' 
#' # Case 2:
#' # -----------------
#' # A second useful option is to add two filters:
#' # the wind direction at the target station (here Ellboegen)
#' # has to be within c(43, 223), the wind direction at the
#' # corresponding crest station (upstream, crest of the European Alps)
#' # has to show southerly flows with a wind direction from
#' # 90 degrees (East) to 270 degrees (West).
#' 
#' # Loading Sattelberg station data and convert to zoo
#' # time series object.
#' data("sattelberg", package = "foehnix")
#' require("zoo")
#' sattelberg <- zoo(subset(sattelberg, select = -timestamp),
#'                  as.POSIXct(sattelberg$timestamp, origin = "1970-01-01", tz = "UTC"))
#' names(sattelberg) <- paste0("crest_", names(sattelberg)) # Renaming variables
#' 
#' # Combine Ellboegen observations with Sattelberg observations
#' data <- merge(ellboegen, sattelberg)
#' print(head(data))
#' 
#' # Now apply a wind filter
#' my_filter  <- list(dd = c(43, 223), crest_dd = c(90, 270))
#' filter_obj <- foehnix_filter(data, my_filter)
#' print(filter_obj)
#' 
#' # Subsetting the 'good' rows
#' data <- data[filter_obj$good,]
#' 
#' summary(subset(data, select = c(dd, crest_dd)))
#' 
#' @author Reto Stauffer
#' @export
foehnix_filter <- function(x, filter) {

    # Wrong input 'x' 
    if ( ! inherits(x, c("zoo", "data.frame")) )
        stop("Input \"x\" to foehnix_filter has to be of class zoo or data.frame.")

    # ---------------
    # If NULL: return integer sequence corresponding to
    # the number of rows in 'x'.
    if ( is.null(filter) ) {
        tmp <- rep(TRUE, nrow(x))

    # ---------------
    # If filter is a function: execute function
    # and check if the return is what we have expected, a list
    # of integers in the range 1:nrow(x). If not, raise an error.
    } else if ( is.function(filter) ) {
        tmp <- apply_foehnix_filter(x, filter, NULL)

    # ---------------
    } else {
        # If filter was not a function nor NULL input filter has to be
        # a named list.
        if ( ! inherits(filter, "list") )
            stop("Input \"filter\" needs to be a list.")
        if ( length(names(filter)) != length(filter) )
            stop("Input \"filter\" needs to be a named list.")

        # Looping over the filter
        valid_fun <- function(x) {
            if ( is.function(x) ) return(TRUE)
            if ( length(x) == 2 & all(is.finite(x)) ) return(TRUE)
            return(FALSE)
        }

        # Check if all list elements are valid elements, either functions
        # or numeric vectors of length 2 with finite values.
        if ( ! all(sapply(filter, valid_fun)) )
            stop("Invalid specification of \"filter\". See ?foehnix_filter for details.")

        # Else check if we can find all names of the filter in
        # the original data set such that wind filtering can be 
        # performed.
        if ( ! all(names(filter) %in% names(x)) )
            stop(paste("Not all variables specified for wind direction filtering",
                       sprintf("found in the data set (%s).", paste(names(filter), collapse = ","))))

        # Else search for indizes where variables lie within 
        # the wind filter rule(s).
        fun <- function(name, filter, x) {
            # Picking data and wind filter rule
            if ( is.function(filter[[name]]) ) {
                apply_foehnix_filter(x, filter[[name]], name)
            } else {
                x <- as.numeric(x[,name])
                f <- filter[[name]]
                if ( f[1L] < f[2L] ) { x >= f[1L] & x <= f[2L] }
                else                 { x >= f[1L] | x <= f[2L] }
            }
        }

        # Apply all filters
        tmp <- do.call(cbind, lapply(names(filter), fun, filter = filter, x = x))
        # Aggregate possible multi-column object "tmp".
        # "all" does the following:
        # - If at least one element is NA -> set to NA
        # - If at least one element FALSE -> set to FALSE
        # - If all elements are TRUE      -> set to TRUE
        tmp <- apply(tmp, 1, all)
    }

    # Wrong length? Stop!
    if ( ! length(tmp) == nrow(x) )
        stop("Filter did not return expected length (not 'nrow(x)'). Stop.")

    # Else return a list with indizes corresponding to the filter
    rval <- list(good   = as.integer(which(tmp == TRUE)),
                 bad    = as.integer(which(tmp == FALSE)),
                 ugly   = as.integer(which(is.na(tmp))),
                 total  = length(tmp),
                 call   = match.call())
    class(rval) <- "foehnix.filter"
    return(rval)

}

#' @rdname foehnix_filter
#' @export
print.foehnix.filter <- function(x, ...) {
    cat("\nFoehnix Filter Object:\n")
    cat("Call: "); print(x$call)
    cat(sprintf("Total data set length:          %8d\n", x$total))
    cat(sprintf("The good (within filter):       %8d (%4.1f percent)\n",
                length(x$good), length(x$good)/x$total*100))
    cat(sprintf("The bad (outside filter):       %8d (%4.1f percent)\n",
                length(x$bad),  length(x$bad)/x$total*100))
    cat(sprintf("The ugly (NA; missing values):  %8d (%4.1f percent)\n",
                length(x$ugly), length(x$ugly)/x$total*100))
    invisible(list(good = length(x$good), bad = length(x$bad), ugly = length(x$ugly)))
}

# Helper function for foehnix_filter, executes the
# filter rules.
#' @importFrom zoo coredata
apply_foehnix_filter <- function(x, filter, name) {

    # Length of input data set
    N <- nrow(x)

    # If filter has to be applied to one specific column: subset
    if ( ! is.null(name) ) x <- x[,name]

    # Apply filter
    res <- filter(x)

    # Else check if we got what we expect
    if ( inherits(res, "zoo") ) {
        if ( ! inherits(zoo::coredata(res), "logical") )
            stop("Custom filter function returns unexpected results.")
    } else if ( ! inherits(zoo::coredata(res), "logical") ) {
        stop("Custom filter function returns unexpected results.")
    }
    if ( length(res) != N )
        stop(sprintf("Custom filter function returned result of wrong length (N != %d)\n", N))

    # Else return
    return(res)

}

