
#' foehnix Demo Data Set
#'
#' The \code{\link{foehnix}} package comes with two sets of meteorological
#' observations one for Tyrol, Austria, and one for Southern California, USA.
#' Each region comes with observations from two stations, one valley station
#' (or target station) and one station further upstream of the main wind direction
#' (crest station) used to filter the data (see \code{\link{foehnix_filter}}).
#' For Tyrol, observations for station Ellbögen (valley) and station Sattelberg
#' (crest) are included, the Californian data set consists of the crest station
#' 'Lucky Five Ranch' and the valley station 'Viejas Casino and Resort'.
#' More details are provided in the 'Details' section.
#'
#' @param which \code{which = "tyrol"} returns the combined tyrolean data set (A) and
#'      \code{which = "california"} the combined californian data set (USA), a time series
#'      object which contains measurements from both stations (valley/crest).
#'      The function can also be used to get the demo data sets for specific sites
#'      (see 'Examples'/'Usage').
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
    obj  <- data(list = stations, package = "foehnix", envir = environment())
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

