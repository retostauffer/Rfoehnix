\name{windrose}
\alias{windrose}
\title{Draw Windrose}
\usage{
    #TODO: Note that this is, at the moment, just a copy
    # of an existing method I've written some time ago and is not
    # ment to enter the release as it is.
    windrose(x, interval = 30, ff = NULL, circle = seq(0,1,by=0.02),
             windsector = NULL, main = NULL, draw.gusts = FALSE )
}
\description{
    Wind rose plotting function.
    #TODO extend manual page as soon as the function is implemented.
}
\arguments{
    \item{x}{zoo or data.frame object containing at least the two
        columns \code{dd} and \code{ff}.}
    \item{interval}{numeric or integer, interval in degrees for
        the binning of the data.}
    \item{ff}{default \code{NULL} \dots maybe name of the wind speed
        column in \code{x}? TODO: no idea.}
    \item{circle}{sequence where the circles should be drawn.}
    \item{windsector}{\code{NULL} by default, a wind sector
        can be provided (as for \code{\link{foehnix}}) to 
        highlight the sector from which foehn winds are observed.}
    \item{main}{\code{NULL} by default or a character containing
        the title for the windrose.}
    \item{draw.gusts}{logical. If \code{TRUE} the function expects
        to find \code{ffx} in the input data set \code{x}.}
}
\details{
    This function requires quite some work! As it is right now
    it is simply a copy of an existing method I've written a
    while ago.
}

