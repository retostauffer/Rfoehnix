# -------------------------------------------------------------------
# - NAME:        foehnix_filter.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-19
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-19, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-19 19:55 on marvin
# -------------------------------------------------------------------

apply_foehnix_filter <- function(x, filter, name) {

    # Length of input data set
    N <- nrow(x)

    # If filter has to be applied to one specific column: subset
    if ( ! is.null(name) ) x <- x[,name]

    # Apply filter
    res <- filter(x)

    # Else check if we got what we expect
    if ( inherits(res, "zoo") ) {
        if ( ! inherits(coredata(res), "logical") )
            stop("Custom filter function returns unexpected results.")
    } else if ( ! inherits(coredata(res), "logical") ) {
        stop("Custom filter function returns unexpected results.")
    }
    if ( length(res) != N )
        stop(sprintf("Custom filter function returned result of wrong length (N != %d)\n", N))

    # Contains NA values?
    if ( any(is.na(res)) )
        stop("Custom filter function returned NA values. Invalid filter.")

    # Else return
    return(res)

}

# -------------------------------------------------------------------
# Returns the indizes of all rows in x where the wind direction
# lies within the wind sector. Used to filter for wind directions.
# -------------------------------------------------------------------
foehnix_filter <- function(x, filter) {

    # Wrong input 'x' 
    if ( ! inherits(x, c("zoo", "data.frame")) )
        stop("Input \"x\" to foehnix_filter has to be of class zoo or data.frame.")

    # ---------------
    # If NULL: return NULL
    if ( is.null(filter) ) return(NULL)

    # ---------------
    # If windfilter is a function: execute function
    # and check if the return is what we have expected, a list
    # of integers in the range 1:nrow(x). If not, raise an error.
    if ( is.function(filter) )
        return(as.integer(which(apply_foehnix_filter(x, filter, NULL))))

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
    idx <- which(apply(tmp, 1, function(x) all(x == TRUE)))
    if ( length(idx) == 0 )
        stop("No data left after applying the wind filter rules!")

    return(as.integer(idx))

}


