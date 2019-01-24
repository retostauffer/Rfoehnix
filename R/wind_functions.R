# -------------------------------------------------------------------
# - NAME:        wind_functions.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-21
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-21, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2019-01-24 13:56 on marvin
# -------------------------------------------------------------------

#' Convert U and V Wind Components to Wind Speed and Direction
#'
#' Takes U/V wind components as input and returns wind speed
#' and meteorological wind direction.
#'
#' @param u \code{numeric} vector with U components or a 
#'    \code{data.frame} or \code{zoo} object containing
#'    a column named \code{u} and a column named \code{v}.
#' @param v \code{NULL} (default) or \code{numeric} vector.
#'    Iv input \code{u} is a single vector \code{v} has to be given.
#' @param rad \code{logical}, default is \code{FALSE}. Returns wind
#'    direction in radiant rather than in degrees.
#'
#' @return Returns a data.frame with two columns named \code{dd}
#'    and \code{ff} containing wind speed (same physical unit as
#'    input \code{u}/\code{v}) and wind direction. Wind direction
#'    is either in meteorological degrees (0 from North, from 90 East,
#'    180 from South, and 270 from West) or in mathematical radiant
#'    if input \code{rad = TRUE}.
#'
#' @seealso ddff2uv
#' @examples
#' ## Generate data.frame with u/v components for all 4 main wind directions
#' data <- data.frame( name = c("N","E","S","W"),
#'                     u    = c( 0, -1 , 0,  1 ),
#'                     v    = c(-1,  0,  1,  0 ) )
#' ## Use data.frame input
#' ddff <- uv2ddff( data )
#' cbind( data, ddff )
#' ## Use u/v components as two separate vector inputs
#' ddff <- uv2ddff( data$u, data$v )
#' cbind( data, ddff )
#' ## Radiant
#' ddff <- uv2ddff( data, rad = TRUE )
#' cbind( data, ddff )
#' @export
uv2ddff <- function(u, v = NULL, rad = FALSE){
   ## if input u is zoo or data.frame
   if ( inherits(u, c("zoo", "data.frame")) ) {
      if ( ! all(c("u", "v") %in% names(u)) )
        stop("necessary colums \"u\" and/or \"v\" missing")
      v = as.numeric(u$v)
      u = as.numeric(u$u)
   ## if u has 2 columns the second column is taken as v
   } else if(NCOL(u) == 2) {
      v <- u[,2]
      u <- u[,1]
   } else {
      if ( is.null(v) ) stop("input \"v\" missing")
      if ( !identical(length(u), length(v)) )
          stop("Length of \"u\" and \"v\" not identical")
   }
   ## polar coordinates:
   ff <- sqrt(u^2 + v^2)
   dd <- atan(v/u) + (u < 0) * pi
   ## Only non-na combis
   idx <- which( !is.na(dd) & !is.na(ff) );   dd[idx] <- dd[idx] + 2 * pi
   ## convert angle to meteorological convention
   dd <- 3 * pi / 2 - dd
   idx <- which( !is.na(dd) & !is.na(ff) );   dd[idx] <- dd[idx] + 2 * pi
   ## if rad (radiants) = F we have to convert to degrees.
   if ( ! rad ) dd <- dd * 180 / pi
   data.frame(dd, ff)
}

#' Conver Wind Speed and Wind Direction In U/V Components
#'
#' Converting wind direction (dd) and wind speed (ff) into 
#' zonal (u) und meridional (v) wind components.
#' 
#' @param ff there are several options to use this input argument.
#'           Most simple: ff is a numeric vector, then dd is required as
#'           second input (numeric vector, too). But ff can also be
#'           a zoo object or a data.frame or a matrix.
#'           If it is a matrix: needs two columns, first one has to be
#'           ff, second dd. If it is a zoo object or a data.frame
#'           there have to be at least two columns (only those two will
#'           be used for computation) named 'ff' and 'dd'.
#' @param dd only necessary if 'ff' is a numeric vector.
#'           Use NULL if ff is containing both
#'           variables (ff/dd), else dd is a data vector containing
#'           the wind direction in degrees (0: north, 90: east, ...)
#'
#' @details Wind speed units can be what you want. You are getting back
#'          the two wind components in the same unit. Wind direction 'dd'
#'          has to be in meteorological degrees where 0 is North,
#'          90 is East and so on.
#'
#' @return Returns a data.frame containing the \code{u} and \code{v} components
#'         of the data. In addition, \code{rad} (mathematical representation
#'         of wind direction in radiant) is returned.
#'
#' @author Jakob Messner
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
#' @export
ddff2uv <- function(dd, ff = NULL){
   ## if ff is a data.frame or a zoo object we have to
   ## search for necessary 'dd' and 'ff'
   if ( inherits(dd, c("zoo", "data.frame")) ) {
     if ( sum(! c('ff','dd') %in% names(dd) ) > 0 )
       stop('necessary colums "ff" and/or "dd" missing')
     ff = as.numeric(dd$ff)
     dd = as.numeric(dd$dd)
   ## if ff has 2 columns the second column is taken as dd
   } else if(NCOL(dd) == 2) {
     ff <- dd[,2]
     dd <- dd[,1]
   } else {
      if ( is.null(ff) ) stop("input \"ff\" missing")
      if ( !identical(length(dd), length(ff)) )
          stop("Length of \"dd\" and \"ff\" not identical")
   }

   ## convert into polar coordinates
   metrad <- dd * pi / 180

   ## u and v
   u <- ff * (-sin(metrad))
   v <- ff * (-cos(metrad))

   ## Mathematical metrad
   rad <- 2*pi - metrad - pi/2
   rad <- ifelse( rad >= 2*pi, rad - 2*pi, rad )

   data.frame(u, v, rad )
}


