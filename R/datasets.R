
#' Meteorological Observation Data, Station Ellboegen
#'
#' The \code{\link{foehnix}} package comes with two data sets of meteorological
#' observations for two sites in Tyrol, Austria.  One station is located near
#' Ellboegen, a small town close to Innsbruck, the second station (Sattelberg)
#' is located south of Ellboegen close to the border between Austria and Italy
#' on top of a mountain close to the crest of the European Alps and is used as
#' 'crest station' (see \code{\link{sattelberg}}).
#'
#' @usage data("ellboegen")
#'
#' @source Station operated by the Department of Atmospheric and Cryospheric
#' Sciences (\url{http://acinn.uibk.ac.at}) of the University of Innsbruck.
#' The data is available under the
#' \href{https://creativecommons.org/licenses/by-sa/4.0/}{CC BY-SA 4.0 license}.
#'
#' @details
#' Ellboegen:
#' \itemize{
#'     \item Location: 11.42889 E/47.18694 N
#'     \item Altitude: 1080 meters above mean sea level
#' }
#' 
#' \code{zoo} time series object with an hourly temporal resolution (time zone: UTC)
#' including the following variables:
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
#' @seealso \code{\link{sattelberg}}, \code{\link{demodata}},
#'          \code{\link{viejas}}, \code{\link{luckyfive}}.
#'
#' @name ellboegen
#' @docType data
#' @keywords data ellboegen
#' @author Reto Stauffer, Deborah Detka
NULL

#' Meteorological Observation Data, Station Sattelberg
#'
#' The \code{\link{foehnix}} package comes with two data sets of meteorological
#' observations for two sites in Tyrol, Austria.  One station is located near
#' Ellboegen (see \code{\link{ellboegen}}), a small town close to Innsbruck,
#' the second station (Sattelberg) is located south of Ellboegen close to the
#' border between Austria and Italy on top of a mountain close to the crest of
#' the European Alps and is used as 'crest station'.
#'
#' @usage data("sattelberg")
#'
#' @source Station operated by the Department of Atmospheric and Cryospheric
#' Sciences (\url{http://acinn.uibk.ac.at}) of the University of Innsbruck.
#' The data is available under the
#' \href{https://creativecommons.org/licenses/by-sa/4.0/}{CC BY-SA 4.0 license}.
#'
#' @details
#' Sattelberg:
#' \itemize{
#'     \item Location: 11.47889 E/47.01083 N
#'     \item Altitude: 2107 meters above mean sea level
#' }
#' 
#' \code{zoo} time series object with an hourly temporal resolution (time zone: UTC)
#' including the following variables:
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
#' @seealso \code{\link{sattelberg}}, \code{\link{demodata}},
#'          \code{\link{viejas}}, \code{\link{luckyfive}}.
#'
#' @name sattelberg
#' @docType data
#' @keywords data sattelberg
#' @author Reto Stauffer, Deborah Detka
NULL

#' Meteorological Observation Data, Station Viejas
#'
#' The \code{\link{foehnix}} package comes with two data sets of meteorological
#' observations for two sites in south California, USA. One station is located
#' next to the Viejas Casino and Resort located in Alpine, CA (valley station),
#' the second station called 'Lucky Five Ranch' is located 22km (14mi)
#' northwest of the Viejas Casino close to the main ridge of the Sierra Nevada
#' mountain range and is used as the crest station.
#'
#' @usage data("viejas")
#'
#' @source Station operated by San Diego Gas and Electric and made freely
#' publicly available through Synoptic PBC at \url{https://synopticdata.com/}.
#'
#' @details
#' Viejas Casino and Resort:
#' \itemize{
#'     \item Location: -116.70437 E/32.84559 N
#'     \item Altitude: 715 meters above mean sea level
#' }
#' 
#' \code{zoo} time series object with an hourly temporal resolution (time zone: UTC)
#' including the following variables:
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
#' @seealso \code{\link{sattelberg}}, \code{\link{demodata}},
#'          \code{\link{viejas}}, \code{\link{luckyfive}}.
#'
#' @name viejas
#' @docType data
#' @keywords data viejas
#' @author Reto Stauffer, Georg J. Mayr


#' Meteorological Observation Data, Lucky Five Ranch
#'
#' The \code{\link{foehnix}} package comes with two data sets of meteorological
#' observations for two sites in south California, USA. One station is located
#' next to the Viejas Casino and Resort located in Alpine, CA (valley station),
#' the second station called 'Lucky Five Ranch' is located 22km (14mi)
#' northwest of the Viejas Casino close to the main ridge of the Sierra Nevada
#' mountain range and is used as the crest station.
#'
#' @usage data("luckyfive")
#'
#' @source Station operated by San Diego Gas and Electric and made freely
#' publicly available through Synoptic PBC at \url{https://synopticdata.com/}.
#'
#' @details
#' Lucky Five Ranch:
#' \itemize{
#'     \item Location: -116.528 E/32.9331 N
#'     \item Altitude: 1445 meters above mean sea level
#' }
#' 
#' \code{zoo} time series object with an hourly temporal resolution (time zone: UTC)
#' including the following variables:
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
#' @seealso \code{\link{sattelberg}}, \code{\link{demodata}},
#'          \code{\link{viejas}}, \code{\link{luckyfive}}.
#'
#' @name luckyfive
NULL


