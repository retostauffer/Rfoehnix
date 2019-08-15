
# @usage
# # Combined data set Tyrol (A).
# # Valley station: Ellbögen; crest station: Sattelberg
# demodata()
# demodata(which = "tyrol")
#
# # Combined data set California (CA, USA).
# # Valley station: Viejas; crest station: Lucky Five Ranch
# demodata(which = "california")
#

#' @details
#' The Tyrolean data set consists of station
#' Elbögen located in the Wipp valley, an area well known for south foehn
#' events. The second station called Sattelberg is
#' located 18km (11.5mi) south of Ellbögen close to the main
#' alpine ridge and is used as upstream crest station.
#'
#' \itemize{
#'     \item Ellbögen: 11.42889 E/47.18694 N, 1080 meters above mean sea level
#'     \item Sattelberg: 11.47889 E/47.01083 N, 2107 meters above mean sea level
#' }
#' 
#' Hourly time series objects (class `zoo`; UTC) including the following variables:
#' \itemize{
#'    \item \code{dd}: wind direction in degrees, meteorological format
#'        (0/360 correspond to 'wind from north', 90 to 'wind from east',
#'        180 'wind from south', and 270 'wind from west')
#'    \item \code{ff}: wind speed in meters per second
#'    \item \code{p}: station pressure in hectopascal
#'    \item \code{rh}: relative humidity in percent
#'    \item \code{t}: dry air temperature in degrees Celsius
#' }
#'
#' The Californian data sets consist of two stations located in the southern
#' part of the state.  Station 'Viejas' is installed next to the Viejas Casino
#' and Resort located in Alpine, CA (valley station), the station 'Lucky Five
#' Ranch' is located 22km (14mi) northwest of the Viejas Casino close to the
#' main ridge of the Sierra Nevada mountain range and is used as the upstream
#' crest station.
#'
#' \itemize{
#'     \item Viejas Casino and Resort: -116.70437 E/32.84559 N, 715 meters above mean sea level
#'     \item Lucky Five Ranch: -116.528 E/32.9331 N, 1445 meters above mean sea level
#' }
#'
#' Hourly time series objects (class `zoo`; UTC) including the following variables:
#' \itemize{
#'    \item \code{air_temp}: dry air temperature in degrees Celsius
#'    \item \code{relative_humidity}: relative humidity in percent
#'    \item \code{wind_speed}: wind speed in meters per second
#'    \item \code{wind_direction}: wind direction in degrees, meteorological format
#'        (0/360 correspond to 'wind from north', 90 to 'wind from east',
#'        180 'wind from south', and 270 'wind from west')
#'    \item \code{wind_gust}: wind gust speed in meters per second
#' }
#'
#' @name demodata
#' @docType data
#' @keywords data
#' @author Reto Stauffer, Deborah Detka
NULL

# @usage
# # Hourly observations station Ellögen (Tyrol, Austria).
# # Serves as "valley" or "target" station for the "tyrol" data set.
# data(which = "ellboegen")

#' @source Station Ellögen operated by the Department of Atmospheric and Cryospheric
#' Sciences (\url{http://acinn.uibk.ac.at}) of the University of Innsbruck.
#' The data is available under the
#' \href{https://creativecommons.org/licenses/by-sa/4.0/}{CC BY-SA 4.0 license}.
#' @name ellboegen
#' @rdname demodata
NULL

# @usage
# # Hourly observations station Sattelberg (Tyrol, Austria).
# # Serves as "crest" station for the "tyrol" data set.
# data(which = "sattelberg")

#' @source Station Sattelberg operated by the Department of Atmospheric and Cryospheric
#' Sciences (\url{http://acinn.uibk.ac.at}) of the University of Innsbruck.
#' The data is available under the
#' \href{https://creativecommons.org/licenses/by-sa/4.0/}{CC BY-SA 4.0 license}.
#' @name sattelberg
#' @rdname demodata
NULL

# @usage
# # Hourly observations station Viejas Casino and Resort (California, U.S.A.).
# # Serves as "valley" or "target" station for the "california data set.
# data(which = "viejas")

#' @source Station Viejas Casino and Resort operated by San Diego Gas and Electric and made freely
#' publicly available through Synoptic PBC at \url{https://synopticdata.com/}.
#' @name viejas
#' @rdname demodata
NULL

# @usage
# # Hourly observations station Lucky Five Ranch (California, U.S.A.).
# # Serves as "crest" station for the "california" data set.
# data(which = "luckyfive")

#' @source Station Lucky Five Ranch operated by San Diego Gas and Electric and made freely
#' publicly available through Synoptic PBC at \url{https://synopticdata.com/}.
#'
#' @name luckyfive
#' @rdname demodata
NULL

