\name{foehnix}
\alias{foehnix}
\alias{plot.foehnix}
\alias{print.foehnix}
\alias{coef.foehnix}
\alias{summary.foehnix}
\alias{predict.foehnix}
\alias{fitted.foehnix}
\alias{logLik.foehnix}
\alias{AIC.foehnix}
\alias{BIC.foehnix}
\alias{edf.foehnix}
\title{Foehn Classification Based on a Two-Component Mixture Model}
\usage{
    ## TODO: extend/adjust usage
    ## Basic usage
    foehnix(formula, data)

    ## Advanced usage
    foehnix(formula, data, windfilter = NULL, family = "gaussian",
            control = foehnix.control(family, ...), ...)

    ## S3 methods for foehnix objects
    plot(x, ...)
    print(object, ...)
    coef(object, type = "parameter", ...)
    summary(object, ...)

    predict(x, newdata = NULL, type = "response")
    fitted(object, which = "probability", ...)

    logLik(object, ...)
    AIC(object, ...)
    BIC(object, ...)
    edf(object, ...)
}
\description{
    This is the main method of the foehnix package to estimate
    two-component mixture models for automated foehn classification.
}
\arguments{
    \item{formula}{an object of class \code{formula} (or one that can be coerced to
          that class): a symbolic description of the model to be
          fitted.  The details of model specification are given under
          'Details'.}
    \item{data}{a time series object of class \code{zoo} containing the
        variables for the two-part mixture model.}
    \item{switch}{logical. If set to \code{TRUE} the two estimated components
        will be switched. This is important if the covariate for the components
        (left hand side of input \code{formula} is smaller for cases with foehn
        than for cases without foehn, e.g., when using the temperature difference
        as main covariate.}
    \item{windfilter}{a named list can be provided to apply a custom (simple)
        filter to the observations on \code{data}. Can be used to e.g., 
        prespecify a specific wind sector for foehn winds. Please
        read the manual of the \code{\link{foehnix_filter}} method
        for more details and examples.} 
    \item{control}{additional control arguments, see \code{\link{foehnix.control}}.}
    \item{...}{forwarded to \code{\link{foehnix.control}}.}
}
\details{
    The two-component mixture model can be specified via formula object where the
    left hand side of the formula contains the variable explaining the two
    components (only one variable), the right hand side of the formula specifies
    the concomitant variables (multiple variables allowed).
    As an example: let's assume that our zoo object '\code{data}' contains the
    following columns:
    \itemize{
        \item \code{ff}: observed wind speed at target site
        \item \code{rh}: observed relative humidity at target site
        \item \code{diff_t}: temperature difference between target site and
            a station upstream of the foehn wind direction
    }
    The specification for \code{formula} could e.g. look as follows:
    \itemize{
        \item \code{ff ~ 1}: the two components of the mixture model will be
            based on the observed wind speed (\code{ff}), no concomitant
            variables are given (intercept only). 
        \item \code{ff ~ rh}: similar to the specification above, but using
            observed relative humidity (\code{rh}) as concomitant.
        \item \code{ff ~ rh + diff_t}: as above but with an additional second
            concomitant variable (observed temperature difference, \code{diff_t}).
        \item \code{diff_t ~ ff + rh}: using temperature difference as the main
            variable for the two components while \code{ff} and \code{rh} are
            used as concomitants.
    }
    Note that these are just examples and have to be adjusted given data
    availability, location, structure/names of the variables in the
    \code{data} object.


    An optional numeric vector of length \code{2} can be provided to filter
    the data. If set, only times (rows in \code{data}) where the observed wind
    direction \code{dd} lies within the wind sector specified.
    For the examples the wind direction \code{dd} is assumed to be in degrees,
    meteorological definition (\code{90} = from east, \code{180} = from south, \dots).
    The following condition is used:
    \itemize{
        \item If \code{windsector[1L] < windsector[2L]}: use \code{data} where
            \code{data$dd >= windsector[1L] & data$dd <= windsector[2L]}
            (e.g., \code{windsector = c(90, 270)} for south wind sector).
        \item If \code{windsector[1L] > windsector[2L]}: use \code{data} where
            \code{data$dd >= windsector[1L] | data$dd <= windsector[2L]}
            (e.g., \code{windsector = c(270, 90)} for north wind sector).
    }


    Details for lambda. If \code{"auto"} the default is \code{"loglik"} except
    that the correlation between two or more covariates used in \code{formula}
    exceeds \code{0.75} (both positive or negative). In this case
    \code{lambda.min = "AIC"} will be used automatically with
    additional regularization.
    If \code{lambda.min = "loglik"} an unpenalized estimate will be returned.
    If \code{lambda.min} is either AIC or BIC a ridge penalty (L2 penalty)
    is used. A fixed set of penalties will be tested. The one minimising
    either the AIC or BIC criteria will be used for the foehn classification
    model.
}
\value{
    Returns an object of class \code{foehnix}. A set of 
    default and custom methods is available for these objects
    such as \code{print}, \code{summary}, \code{coef}, \code{plot}, \dots
}
\references{
    Plavcan D, Mayr GJ, Zeileis A (2014).
        Automatic and Probabilistic Foehn Diagnosis with a Statistical Mixture Model.
        \emph{Journal of Applied Meteorology and Climatology}.
        \bold{53}(3), 652--659. doi:10.1175/JAMC-D-13-0267.1
    
    Gr\\"un B, Leisch F (2007).
        Fitting Finite Mixtures of Generalized Linear Regressions in \emph{R}.
        \emph{Computational Statistics \& Data Analysis}. \bold{51}(11), 5247--5252.
        doi:10.1016/j.csda.2006.08.014
    
    Gr\\"un B, Leisch F (2008).
        FlexMix Version 2: Finite Mixtures with Concomitant Variables and Varying and Constant Parameters.
        \emph{Journal of Statistical Software, Articles}.
        \bold{28}(4), 1--35. doi:10.18637/jss.v028.i04
    
    Fraley C, Raftery AE (2002).
        Model-Based Clustering, Discriminant Analysis, and Density Estimation.
        \emph{Journal of the American Statistical Association}.
        \bold{97}(458), 611--631. doi:10.1198/016214502760047131
}
\seealso{
    See \code{\link{windfilter}} for more information about the
    \code{windfilter} option.

    See also: \code{\link{tsplot}}, \code{\link{plot.foehnix}},
    \code{\link{windrose}}.
}










