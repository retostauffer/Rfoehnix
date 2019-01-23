# -------------------------------------------------------------------
# - NAME:        demodata.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2019-01-16
# -------------------------------------------------------------------
# - DESCRIPTION: A small function which returns the demo data set.
# -------------------------------------------------------------------
# - EDITORIAL:   2019-01-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2019-01-23 16:04 on marvin
# -------------------------------------------------------------------


#' foehnix Demo Data Set
#'
#' The foehnix package comes with two demo data sets containing
#' observations from two automated weather stations in Tyrol,
#' Austria. One station (Sattelberg) is located close to the main
#' alpine ridge and is used as 'crest' station. The second station
#' (Ellboegen) is located in the Wipp valley.
#' The package demos and examples will estimate automated foehn
#' classification models (\code{\link{foehnix}} models) for 
#' station Ellboegen using additional information from the
#' crest station as concomitant variables and for custom wind
#' filters (\code{\link{foehnix_filter}}).
#'
#' @param which either \code{"combined"} (returns a combined data set),
#'    \code{"sattelberg"} (only observations from station Sattelberg),
#'    or \code{"ellboegen"} (only observations rom station Ellboegen).
#'
#' @source Station operated by the Department of Atmospheric and Cryospheric
#' Sciences (\url{http://acinn.uibk.ac.at}) of the University of Innsbruck.
#' The data is available under the
#' \href{https://creativecommons.org/licenses/by-sa/4.0/}{CC BY-SA 4.0 license}.
#' 
#' @examples
#' # Loading the combined demo data set. Variables starting with
#' # "crest_" are observations from station Sattelberg.
#' x <- demodata()
#' print(head(x))
#'
#' # Sattelberg only
#' x <- demodata("sattelberg")
#' print(head(x))
#'
#' # Ellboegen only
#' x <- demodata("ellboegen")
#' print(head(x))
#'
#' @seealso \code{\link{ellboegen}}, \code{\link{sattelberg}}
#' @author Reto Stauffer
#' @export
demodata <- function(which = "combined") {

    which <- match.arg(tolower(which), c("combined", "ellboegen", "sattelberg"))
    # Loading observations (data.frame) from two CSV files
    # which come with the foehnix package.
    if ( any(c("ellboegen", "combined") %in% which) ) {
        data("ellboegen",  package = "foehnix")
        ellboegen  <- zoo::zoo(ellboegen[,-1],  as.POSIXct(ellboegen[,1],  origin = "1970-01-01"))
    }
    if ( any(c("sattelberg", "combined") %in% which) ) {
        data("sattelberg", package = "foehnix")
        sattelberg <- zoo::zoo(sattelberg[,-1], as.POSIXct(sattelberg[,1], origin = "1970-01-01"))
    }

    # Convert the data.frames into time series objects.
    if ( which == "ellboegen" )  return(ellboegen)
    if ( which == "sattelberg" ) return(sattelberg)
    
    # Else combine both data sets

    # Modify sattelberg variable names (crest_ identifies Sattelberg
    # observations, our crest station) and combine both data sets.
    names(sattelberg) <- paste0("crest_", names(sattelberg))
    data <- merge(ellboegen, sattelberg)
    
    # Dry adiabatic temperature difference between 
    # Sattelberg (data$crest_t) and Ellboegen (data$t) corrected by
    # 1027/10 degrees.
    data$diff_t <- data$crest_t + 10.27 - data$t

    return(data)

}

