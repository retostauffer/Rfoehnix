
#' Meteorological Observation Data, Station Ellboegen
#'
#' The \code{\link{foehnix}} package comes with two data sets of meteorological
#' observations for two sites in Tyrol, Austria.  One station is located nearby
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
#' Variable description (columns):
#' \itemize{
#'    \item \code{timestamp}: UNIX time stamp, seconds since 1970-01-01 00:00:00
#'    \item \code{dd}: wind direction in degrees, meteorological format
#'        (0/360 correspond to 'wind from north', 90 to 'wind from east',
#'        180 'wind from south', and 270 'wind from west'
#'    \item \code{ff}: wind speed in meters per second
#'    \item \code{p}: station pressure in hectopascal
#'    \item \code{rh}: relative humidity in percent
#'    \item \code{t}: dry air temperature in degrees Celsius
#' }
#'
#' @seealso \code{\link{sattelberg}}, \code{\link{demodata}}.
#'
#' @name ellboegen
#' @docType data
#' @keywords data ellboegen
#' @author Reto Stauffer
NULL

#' Meteorological Observation Data, Station Sattelberg
#'
#' The \code{\link{foehnix}} package comes with two data sets of meteorological
#' observations for two sites in Tyrol, Austria.  One station is located nearby
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
#' Variable description (columns):
#' \itemize{
#'    \item \code{timestamp}: UNIX time stamp, seconds since 1970-01-01 00:00:00
#'    \item \code{dd}: wind direction in degrees, meteorological format
#'        (0/360 correspond to 'wind from north', 90 to 'wind from east',
#'        180 'wind from south', and 270 'wind from west'
#'    \item \code{ff}: wind speed in meters per second
#'    \item \code{p}: station pressure in hectopascal
#'    \item \code{rh}: relative humidity in percent
#'    \item \code{t}: dry air temperature in degrees Celsius
#' }
#'
#' @seealso \code{\link{ellboegen}}, \code{\link{demodata}}.
#'
#' @name sattelberg
#' @docType data
#' @keywords data sattelberg
#' @author Reto Stauffer
NULL

