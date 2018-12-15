\name{foehnix}
\alias{foehnix}
\title{Foehn Classification Based on a Two-Component Mixture Model}
\usage{
    ## TODO: extend/adjust usage
    ## Basic usage
    foehnix(formula, data)
    ## Advanced usage
    foehnix(formula, data, windsector = NULL, maxit = 100L, tol = 1e-8,
            lambda.min = "auto", standardize = TRUE, family = "gaussian", ...) 
}
\arguments{
    \item{formula}{an object of class \code{formula} (or one that can be coerced to
          that class): a symbolic description of the model to be
          fitted.  The details of model specification are given under
          'Details'.}
    \item{data}{a time series object of class \code{zoo} containing the
        variables for the two-part mixture model.}
    \item{windsector}{either \code{NULL} or a \code{numeric} vector of length \code{2}.
        Defines the foehn wind sector used for filtering the data.
        Details can be found in the 'Details' section.}
    \item{maxit}{control argument for the iterative solvers. Default is \code{100L},
        the maximum number of iterations for the EM algorithm and the IWLS backfitting
        algorithm for the concomitant model.
        If a vector of length two is provided the first value is used for the
        EM algorithm, the second for the IWLS backfitting.}
    \item{tol}{similar as for \code{maxit}. Used to identify convergence of the
        iterative solvers. Default is \code{1e-8}, if two values are given the
        first will be used for the EM algorithm, the second one for the IWLS backfitting
        procedure. If set to \code{NULL} the algorithm will run until \code{maxit} is reached.}
    \item{lambda.min}{one of \code{"auto"} (default), \code{"loglik"}, \code{"BIC"}, or \code{"AIC"}.
        See 'Details' for more information.}
    \item{standardize}{logical flag, default is \code{TRUE}. Defines whether or not
        the model matrix for the concomitant model should be standardized for 
        model estimation. Recommended.}
    \item{family}{string, distribution of the two components of the mixture model.
        One of \code{"gaussian"} (default) or \code{"logistic"}.}
    \item{...}{Currently unused additional arguments.}
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
    \bold{53}(3), 652--659. \doi{10.1175/JAMC-D-13-0267.1}

Gr\"un B, Leisch F (2007).
    Fitting Finite Mixtures of Generalized Linear Regressions in \emph{R}.
    \emph{Computational Statistics \& Data Analysis}. \bold{51}(11), 5247--5252.
    \doi{10.1016/j.csda.2006.08.014}

Gr\"un B, Leisch F (2008).
    FlexMix Version 2: Finite Mixtures with Concomitant Variables and Varying and Constant Parameters.
    \emph{Journal of Statistical Software, Articles}
    \bold{28}(4), 1--35. \doi{10.18637/jss.v028.i04}
}










