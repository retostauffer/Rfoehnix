#' Meteorological Observation Data, Station Ellboegen
#'
#' The \code{\link{foehnix}} package comes with two data sets of meteorological
#' observations for two sites in Tyrol, Austria.  One station is located near
#' Ellboegen, a small town close to Innsbruck, the second station (Sattelberg)
#' is located south of Ellboegen close to the border between Austria and Italy
#' on top of a mountain close to the crest of the European Alps and is used as
#' 'crest station' (see \code{\link{sattelberg}}).
#'
#' #TODO migrate the descriptions (commented, see below) into this combined
#' manual page.
#'
#' @usage
#' # Combined data set Tyrol (A).
#' # Valley station: Ellbögen; crest station: Sattelberg
#' demodata("tyrol")
#'
#' # Combined data set California (CA, USA).
#' # Valley station: Viejas; crest station: Lucky Five Ranch
#' demodata("tyrol")
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
#' @name demodatasets
#' @docType data
#' @keywords data
#' @author Reto Stauffer, Deborah Detka
NULL

#' @usage
#' # Hourly observations station Ellögen (Tyrol, Austria).
#' # Serves as "valley" or "target" station for the "tyrol" data set.
#' data("ellboegen")
#' @source Station Ellögen operated by the Department of Atmospheric and Cryospheric
#' Sciences (\url{http://acinn.uibk.ac.at}) of the University of Innsbruck.
#' The data is available under the
#' \href{https://creativecommons.org/licenses/by-sa/4.0/}{CC BY-SA 4.0 license}.
#' @name ellboegen
#' @rdname demodatasets
NULL

#' @usage
#' # Hourly observations station Sattelberg (Tyrol, Austria).
#' # Serves as "crest" station for the "tyrol" data set.
#' data("sattelberg")
#' @source Station Sattelberg operated by the Department of Atmospheric and Cryospheric
#' Sciences (\url{http://acinn.uibk.ac.at}) of the University of Innsbruck.
#' The data is available under the
#' \href{https://creativecommons.org/licenses/by-sa/4.0/}{CC BY-SA 4.0 license}.
#' @name sattelberg
#' @rdname demodatasets
NULL

#' @usage
#' # Hourly observations station Viejas Casino and Resort (California, U.S.A.).
#' # Serves as "valley" or "target" station for the "california data set.
#' data("viejas")
#' @source Station Viejas Casino and Resort operated by San Diego Gas and Electric and made freely
#' publicly available through Synoptic PBC at \url{https://synopticdata.com/}.
#' @name viejas
#' @rdname demodatasets
NULL

#' @usage
#' # Hourly observations station Lucky Five Ranch (California, U.S.A.).
#' # Serves as "crest" station for the "california" data set.
#' data("luckyfive")
#' @source Station Lucky Five Ranch operated by San Diego Gas and Electric and made freely
#' publicly available through Synoptic PBC at \url{https://synopticdata.com/}.
#'
#' @name luckyfive
#' @rdname demodatasets
NULL

 ##  Meteorological Observation Data, Station Sattelberg
 ##  
 ##  The \code{\link{foehnix}} package comes with two data sets of meteorological
 ##  observations for two sites in Tyrol, Austria.  One station is located near
 ##  Ellboegen (see \code{\link{ellboegen}}), a small town close to Innsbruck,
 ##  the second station (Sattelberg) is located south of Ellboegen close to the
 ##  border between Austria and Italy on top of a mountain close to the crest of
 ##  the European Alps and is used as 'crest station'.
 ##  
 ##  @usage data("sattelberg")
 ##  
 ##  @source Station operated by the Department of Atmospheric and Cryospheric
 ##  Sciences (\url{http://acinn.uibk.ac.at}) of the University of Innsbruck.
 ##  The data is available under the
 ##  \href{https://creativecommons.org/licenses/by-sa/4.0/}{CC BY-SA 4.0 license}.
 ##  
 ##  @details
 ##  Sattelberg:
 ##  \itemize{
 ##      \item Location: 11.47889 E/47.01083 N
 ##      \item Altitude: 2107 meters above mean sea level
 ##  }
 ##  
 ##  \code{zoo} time series object with an hourly temporal resolution (time zone: UTC)
 ##  including the following variables:
 ##  \itemize{
 ##     \item \code{dd}: wind direction in degrees, meteorological format
 ##         (0/360 correspond to 'wind from north', 90 to 'wind from east',
 ##         180 'wind from south', and 270 'wind from west')
 ##     \item \code{ff}: wind speed in meters per second
 ##     \item \code{p}: station pressure in hectopascal
 ##     \item \code{rh}: relative humidity in percent
 ##     \item \code{t}: dry air temperature in degrees Celsius
 ##  }
 ##  
 ##  @seealso \code{\link{sattelberg}}, \code{\link{demodata}},
 ##           \code{\link{viejas}}, \code{\link{luckyfive}}.
 ##  
 ##  @name sattelberg
 ##  @docType data
 ##  @keywords data sattelberg
 ##  @author Reto Stauffer, Deborah Detka
 ##  NULL
 ##  
 ##  Meteorological Observation Data, Station Viejas
 ##  
 ##  The \code{\link{foehnix}} package comes with two data sets of meteorological
 ##  observations for two sites in south California, USA. One station is located
 ##  next to the Viejas Casino and Resort located in Alpine, CA (valley station),
 ##  the second station called 'Lucky Five Ranch' is located 22km (14mi)
 ##  northwest of the Viejas Casino close to the main ridge of the Sierra Nevada
 ##  mountain range and is used as the crest station.
 ##  
 ##  @usage data("viejas")
 ##  
 ##  @source Station operated by San Diego Gas and Electric and made freely
 ##  publicly available through Synoptic PBC at \url{https://synopticdata.com/}.
 ##  
 ##  @details
 ##  Viejas Casino and Resort:
 ##  \itemize{
 ##      \item Location: -116.70437 E/32.84559 N
 ##      \item Altitude: 715 meters above mean sea level
 ##  }
 ##  
 ##  \code{zoo} time series object with an hourly temporal resolution (time zone: UTC)
 ##  including the following variables:
 ##  \itemize{
 ##     \item \code{air_temp}: dry air temperature in degrees Celsius
 ##     \item \code{relative_humidity}: relative humidity in percent
 ##     \item \code{wind_speed}: wind speed in meters per second
 ##     \item \code{wind_direction}: wind direction in degrees, meteorological format
 ##         (0/360 correspond to 'wind from north', 90 to 'wind from east',
 ##         180 'wind from south', and 270 'wind from west')
 ##     \item \code{wind_gust}: wind gust speed in meters per second
 ##  }
 ##  
 ##  @seealso \code{\link{sattelberg}}, \code{\link{demodata}},
 ##           \code{\link{viejas}}, \code{\link{luckyfive}}.
 ##  
 ##  @name viejas
 ##  @rdname california
 ##  @docType data
 ##  @keywords data viejas
 ##  @author Reto Stauffer, Georg J. Mayr
 ##  ####NULL
 ##  ####
 ##  #####' @name luckyfive
 ##  #####' @rdname @california
 ##  ####NULL
