# -------------------------------------------------------------------
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-21
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-21, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2019-08-16 15:07 on marvin
# -------------------------------------------------------------------

#' Convert u and v Wind Components to Wind Speed and Wind Direction
#'
#' Takes u/v wind components (zonal and meridional wind components) as input
#' and returns wind speed and meteorological wind direction.
#'
#' @param u \code{numeric} vector with u components or a 
#'    \code{data.frame} or \code{zoo} object containing
#'    a column named \code{u} and a column named \code{v}.
#' @param v \code{NULL} (default) or \code{numeric} vector.
#'    If input \code{u} is a single vector \code{v} has to be specified.
#' @param rad \code{logical}, default is \code{FALSE}. Returns wind
#'    direction in radiant rather than degrees.
#'
#' @return Returns a \code{data.frame} or \code{zoo} object (depending on input
#' \code{u}) with two columns named \code{dd}
#' and \code{ff} containing wind speed (same physical unit as
#' input \code{u}/\code{v}) and wind direction. Wind direction
#' is either in meteorological degrees (0 from North, from 90 East,
#' 180 from South, and 270 from West) or in mathematical radiant
#' if input \code{rad = TRUE}.
#'
#' @author Reto Stauffer
#'
#' @details Note: if both, \code{u} and \code{v} are provided they
#' do have to be of the same length OR one of them has to be of length 1.
#' The one with length 1 will be recycled.
#'
#' @seealso ddff2uv
#' @examples
#' ## Generate data.frame with u/v components for all 4 main wind directions
#' data <- data.frame(name = c("N","E","S","W"),
#'                    u    = c( 0, -1 , 0,  1 ),
#'                    v    = c(-1,  0,  1,  0 ))
#' ## Use data.frame input
#' ddff <- uv2ddff(data)
#' cbind(data, ddff)
#' ## Use u/v components as two separate vector inputs
#' ddff <- uv2ddff(data$u, data$v)
#' cbind(data, ddff)
#' ## Radiant
#' ddff <- uv2ddff(data, rad = TRUE)
#' cbind(data, ddff)
#'
#' ## Use with zoo
#' library("zoo")
#' set.seed(100)
#' Sys.setenv("TZ" = "UTC")
#' data <- zoo(data.frame(u = rnorm(20, 2, 5), v = rnorm(20, 0, 4)),
#'             as.POSIXct("2019-01-01 12:00") + 0:19 * 3600)
#' head(data)
#' class(data)
#' ## Calculate dd/ff
#' ddff <- uv2ddff(data)
#' head(ddff)
#' class(ddff)
#'
#' @export
uv2ddff <- function(u, v = NULL, rad = FALSE){
   # if input u is zoo or data.frame
   zoo_index <- NULL # Default
   if (inherits(u, c("zoo", "data.frame"))) {
      # If input 'u' is zoo: keep index
      if (inherits(u, "zoo")) zoo_index <- index(u)
      if (!all(c("u", "v") %in% names(u)))
        stop("necessary colums \"u\" and/or \"v\" missing")
      # If "v" is set in addition: warn
      if (!is.null(v)) {
          warning(sprintf("input \"u\" to uv2ddff is \"%s\":", class(u)),
                          "\"v\" specified as well but will be ignored!")
      }
      v = as.numeric(u$v)
      u = as.numeric(u$u)
   # if u has 2 columns the second column is taken as v
   } else if (NCOL(u) == 2) {
      v <- u[,2]
      u <- u[,1]
   } else {
      if (is.null(v)) stop("input \"v\" missing")
      # If lenths do not match and none of them is of length 1: stop.
      if (!(length(u) == length(v)) & !any(c(length(u), length(v)) == 1L)) {
         stop("Length of \"u\" and \"v\" not identical")
      # Else recycle the one with length one to the length of the other one.
      # Damn it, so much one's in one sentence!
      } else if (length(u) == 1) {
         u <- rep(u, length(v))
      } else if (length(v) == 1) {
         v <- rep(v, length(u))
      }
   }
   # polar coordinates:
   ff <- sqrt(u^2 + v^2)
   dd <- atan(v/u) + (u < 0) * pi
   # Only non-na combis
   idx <- which(!is.na(dd) & !is.na(ff));   dd[idx] <- dd[idx] + 2 * pi
   # convert angle to meteorological convention
   dd <- 3 * pi / 2 - dd
   idx <- which(!is.na(dd) & !is.na(ff));   dd[idx] <- dd[idx] + 2 * pi
   # if rad (radiants) = F we have to convert to degrees.
   if (!rad) dd <- dd * 180 / pi
   res <- data.frame(dd, ff)
   if (is.null(zoo_index)) return(res) else return(zoo(res, zoo_index))
}

#' Convert Wind Speed and Wind Direction to u/v Wind Components
#'
#' Converts wind direction (\code{dd}) and wind speed (\code{ff})
#' information into \code{u}/\code{v} wind components (zonal and
#' meridional wind component).
#' 
#' @param ff \code{numeric}, \code{matrix}, \code{data.frame}, or
#'           \code{zoo} object (see 'Details' section).
#' @param dd only necessary if \code{ff} is a numeric vector.  Use NULL if
#'        input \code{ff} is containing both variables (\code{ff}, \code{dd}), else
#'        \code{dd} is a data vector containing the wind direction in degrees (0:
#'        north, 90: east, ...)
#'
#' @details Converts data from wind speed and direction into the zonal
#' and meridional wind components (\code{u}/\code{v}).
#'
#' Different inputs are allowed:
#' \itemize{
#'    \item if \code{ff} is a matrix: requires to contains at least the
#'          two columns \code{"ff"} and \code{"dd"}.
#'    \item if \code{ff} is a \code{data.frame} or \code{zoo} object:
#'          requires to contains at least the
#'          two variables \code{"ff"} and \code{"dd"}.
#'    \item if \code{ff} is \code{numeric}: \code{dd} has to be provided
#'          in advance. \code{ff} and \code{dd} have to be of the same length
#'          or one has to be of length \code{1} (will be recycled).
#' }
#'
#' @return Returns a \code{data.frame} or \code{zoo} object (depending on input \code{ff})
#' containing the \code{u} and \code{v} components
#' of the data. In addition, \code{rad} (mathematical representation
#' of wind direction in radiant) is returned.
#'
#' @author Reto Stauffer
#'
#' @seealso uv2ddff
#' @examples 
#' ## Generate dd and ff variable
#' set.seed(0)
#' ff <- floor(abs(rnorm(20))*10)
#' dd <- sample(seq(0,359),20)
#' df <- data.frame('ff'=ff,'dd'=dd)
#'
#' ## Using with vectors 
#' print(head(ddff2uv('dd' = dd, 'ff' = ff)))
#' 
#' ## Using with data.frame
#' print(head(ddff2uv(df)))
#' 
#' ## Using with matrix
#' print(head(ddff2uv(as.matrix(df))))
#'
#' ## Use with zoo
#' library("zoo")
#' set.seed(100)
#' Sys.setenv("TZ" = "UTC")
#' data <- zoo(data.frame(dd = runif(20, 0, 360), ff = rnorm(20, .2, 2)^2),
#'             as.POSIXct("2019-01-01 12:00") + 0:19 * 3600)
#' data <- round(data,2)
#' head(data)
#' class(data)
#' ## Calculate dd/ff
#' uv <- ddff2uv(data)
#' head(uv)
#' class(uv)
#' @export
ddff2uv <- function(dd, ff = NULL){
   # if ff is a data.frame or a zoo object we have to
   # search for necessary 'dd' and 'ff'
   zoo_index <- NULL # Default
   if (inherits(dd, c("zoo", "data.frame"))) {
     if (sum(! c('ff','dd') %in% names(dd) ) > 0)
       stop('necessary colums "ff" and/or "dd" missing')
     # If "ff" is set in addition: warn
     if (!is.null(ff)) {
         warning(sprintf("input \"dd\" to uv2ddff is \"%s\":", class(u)),
                         "\"ff\" specified as well but will be ignored!")
     }
     if (inherits(dd, "zoo")) zoo_index <- index(dd)
     ff = as.numeric(dd$ff)
     dd = as.numeric(dd$dd)
   # if ff has 2 columns the second column is taken as dd
   } else if(NCOL(dd) == 2) {
     ff <- dd[,2]
     dd <- dd[,1]
   } else {
      if (is.null(ff)) stop("input \"ff\" missing")
      # If lenths do not match and none of them is of length 1: stop.
      if (!length(dd) == length(ff) & !any(c(length(dd), length(ff)) == 1L)) {
         stop("Length of \"dd\" and \"ff\" not identical")
      # Else recycle the one with length one to the length of the other one.
      # Damn it, so much one's in one sentence!
      } else if (length(dd) == 1) {
         dd <- rep(dd, length(ff))
      } else if (length(ff) == 1) {
         ff <- rep(ff, length(dd))
      }
   }

   # Convert into polar coordinates
   metrad <- dd * pi / 180

   ## u and v
   u <- ff * (-sin(metrad))
   v <- ff * (-cos(metrad))

   # Mathematical metrad
   rad <- 2*pi - metrad - pi/2
   rad <- ifelse(rad >= 2*pi, rad - 2*pi, rad)

   res <- data.frame(u, v, rad)
   if (is.null(zoo_index)) return(res) else return(zoo(res, zoo_index))
}


