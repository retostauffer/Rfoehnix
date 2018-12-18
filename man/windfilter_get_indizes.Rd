\name{windfilter_get_indizes}
\alias{windfilter_get_indizes}
\title{Get Observations Within Wind Sector (Wind Filter)}
\usage{
    windfilter_get_indizes(x, windfilter)
}
\description{
    \code{\link{foehnix}} models allow to specify an optional
    \code{windfilter}. This filter is not restricted to wind
    direction, but typically a wind direction filter is used
    to restrict foehn classification to a specific wind sector
    from which foehn is expected.

    Multiple filter rules can be used, e.g., a wind sector
    for the target location and an additional wind sector filter
    for an upstream station. More details can be found in the
    'Details' section and the 'Examples' of this manual page.
}
\arguments{
    \item{x}{object of class \code{zoo} or \code{data.frame} containing
        the observations.}
    \item{windfilter}{a list of filter rules (or \code{NULL}), see 'Details'.}
}
\details{
    Foehn winds often (not always) show a very specific wind direction
    due to the canalization of the air flow trough the local
    topography. The \code{windfilter} option allows to subset the
    data according to a user defined wind sector using the observed
    wind direction to filter the observations.

    The \code{windfilter} object can be \code{NULL} (for convenience, no
    filtering), in this case the function simply returns \code{NULL}.
    Else \code{windfilter} has to be a named \code{list} where each
    list element contains a numeric vector with two numeric values.
    As an example: if we know that foehn wind for a specific location
    has to have a wind direction between 100 and 260 degrees and the
    observed wind direction in \code{x} is \code{dd} we can specify
    a wind filter as follows:

    \itemize{
        \item \code{windfilter = list(dd = c(100, 260))}
    }

    The following rules are applied (example for column \code{dd}:
    \itemize{
        \item If the first element is smaller than the second element:
            only rows of \code{x} where \code{x[,"dd"]} is within the
            defined sector will be used for classification.
        \item If the first element is larger than the second element:
            only rows of \code{x} where \code{x[,"dd"]} is outside the
            defined sector will be used for classification (allows to
            specify north wind sectors going trough 0 degrees).
    }
}
\examples{
    # Loading example data set and conver to zoo time series
    # time series object.
    data("ellboegen", package = "foehnix")
    library("zoo")
    ellboegen <- zoo(subset(ellboegen, select = -timestamp),
                     as.POSIXct(ellboegen$timestamp, origin = "1970-01-01", tz = "UTC"))

    # Case 1:
    # -----------------
    # Filter for observations where the wind direction is
    # within 100 - 260 (southerly flow):
    idx_south <- windfilter_get_indizes(ellboegen, list(dd = c(100, 260)))

    # Same filter but for northerly flows, taking rows with
    # wind direction observations (dd) smaller than 45 or
    # larger than 315 degrees:
    idx_north <- windfilter_get_indizes(ellboegen, list(dd = c(315, 45)))

    par(mfrow = c(1,3))
    hist(ellboegen$dd,            xlab = "dd", main = "all observations")
    hist(ellboegen$dd[idx_south], xlab = "dd", main = "southerly winds")
    hist(ellboegen$dd[idx_north], xlab = "dd", main = "northerly winds")

    # Case 2:
    # -----------------
    # A second useful option is to add two filters:
    # the wind direction at the target station (here Ellboegen)
    # has to be within c(43, 223), the wind direction at the
    # corresponding crest station (upstream, crest of the European Alps)
    # has to show southerly flows with a wind direction from
    # 90 degrees (East) to 270 degrees (West).

    # Loading Sattelberg station data and convert to zoo
    # time series object.
    data("sattelberg", package = "foehnix")
    require("zoo")
    sattelberg <- zoo(subset(sattelberg, select = -timestamp),
                     as.POSIXct(sattelberg$timestamp, origin = "1970-01-01", tz = "UTC"))
    names(sattelberg) <- sprintf("sat_\%s", names(sattelberg)) # Renaming variables

    # Combine Ellboegen observations with Sattelberg observations
    data <- merge(ellboegen, sattelberg)
    print(head(data))

    # Now apply a wind filter
    windfilter <- list(dd = c(43, 223), sat_dd = c(90, 270))
    idx <- windfilter_get_indizes(data, windfilter)
    data <- data[idx,]

    summary(subset(data, select = c(dd, sat_dd)))
}
