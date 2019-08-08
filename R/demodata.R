
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
#' @param which the \code{foehnix} package comes with two demo data sets.
#'      Each consists of meteorological observations of two stations (one
#'      in the valley; target station) and one near the crest upstream of
#'      typical foehn direction (crest station).
#'      \code{which = "tyrol"} returns the tyrolean data set (A),
#'      \code{which = "california"} the californian data set (USA), a time series
#'      object which contains measurements from both stations (valley/crest).
#'      The function can also be used to get the demo data sets for specific sites
#'      (see 'Examples').
#'
#' @source
#' Ellboegen and Sattelberg:
#' Operated by the Department of Atmospheric and Cryospheric
#' Sciences (\url{http://acinn.uibk.ac.at}) of the University of Innsbruck.
#' The data is available under the
#' \href{https://creativecommons.org/licenses/by-sa/4.0/}{CC BY-SA 4.0 license}.
#'
#' Viejas and the Lucky Five Ranch:
#' Operated by #TODO add correct source (GM).
#' 
#' @examples
#' # Loading the combined demo data set for "Tyrol (A)".
#' # Stations: Ellboegen (valley station) and Sattelberg (crest station).
#' # Variables starting with "crest_" are observations from station Sattelberg,
#' x <- demodata("tyrol") # Default
#' print(head(x))
#'
#' # Loading the combined demo data set for "California (USA)".
#' # Stations: Viejas (valley station) and 'Lucky Five Ranch' (crest station).
#' # Variables starting with "crest_" are observations from station 'Lucky Five Ranch'.
#' x <- demodata("california")
#' print(head(x))
#'
#' # Sattelberg only
#' x <- demodata("sattelberg")
#' print(head(x))
#'
#' # Solely Ellboegen
#' x <- demodata("ellboegen")
#' print(head(x))
#'
#' # Viejas
#' x <- demodata("viejas")
#' print(head(x))
#'
#' # Lucky Five Ranch
#' x <- demodata("luckyfive")
#' print(head(x))
#'
#' @seealso \code{\link{ellboegen}}, \code{\link{sattelberg}},
#'          \code{\link{viejas}}, \code{\link{luckyfive}}
#'
#' @author Reto Stauffer
#' @export
demodata <- function(which = c("tyrol", "ellboegen", "sattelberg",
                               "california", "viejas", "luckyfive")) {

    # Match arg
    which <- match.arg(which)

    # For combined stations
    if (which == "tyrol") {
        stations <- list(valley = "ellboegen", crest = "sattelberg")
    } else if (which == "california") {
        stations <- list(valley = "viejas", crest = "luckyfive")
    } else {
        stations <- which # keep it as character
    }

    # Single station?
    if (is.character(stations)) {
        obj <- do.call(data, list(stations, package = "foehnix"))
        return(eval(parse(text = obj[1L])))
    }

    # Else load both stations, rename, and combine.
    stopifnot(identical(sort(names(stations)), c("crest", "valley")))

    # Load data sets
    obj  <- data(list = stations, package = "foehnix")
    data <- lapply(obj, function(x) return(eval(parse(text = x))))

    # Rename crest station variables
    names(data$crest) <- sprintf("crest_%s", names(data$crest))
    data <- do.call(merge, c(data, list(all = TRUE)))

    # Dry adiabatic temperature difference between  ...
    if (which == "tyrol") {
        # Sattelberg (data$crest_t) and Ellboegen (data$t) corrected by
        # 1027 / 100 degrees K (1027 meters of difference in altitude).
        data$diff_t <- data$crest_t + 10.27 - data$t
    } else if (which == "california") {
        # Lucky Five Ranch (data$crest_air_temp) and Viejas (data$air_temp) corrected by
        # 730 / 100 degrees K (730 meters of difference in altitude)
        data$diff_temp <- data$crest_air_temp + 7.30 - data$air_temp
    }

    return(data)
}

