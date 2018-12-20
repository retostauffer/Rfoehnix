\name{plot.foehnix}
\title{foehnix Model Assessment Plots}
\description{
Visual representation \code{\link{foehnix}} model optimization.
}
\usage{
    ## Basic
    plot(x)
    ## Specific plot type
    plot(x, which = "loglik")
    plot(x, which = 1:2)
}
\arguments{
    \item{x}{object of type \code{\link{foehnix}}.}
    \item{which}{\code{NULL} (default), character, character string,
        integer, or numeric. Allowed characters: \code{loglik},
        \code{loglikcontribution}, and \code{coef}. If \code{which}
        is numeric/integer or a vector of numerics/integers the numbers
        correspond to \code{loglik} (\code{1}), \code{loglikpath} (\code{2}),
        or \code{coef} (\code{3}).}
}
\details{
    There are currently three different plot types.
    \itemize{
        \item \code{"loglik"} shows the log-likelihood sum path trough
            the iterations of the EM algorithm for parameter estimation.
        \item \code{"loglikcontribution"} shows the log-likelihood
            contribution (initial value subtracted; all paths start
            with \code{0}).
        \item \code{coef} shows the development of the (standardized)
            coefficients during EM optimization. Parameters of the
            components are shown on the real scale, the coefficients
            of the concomitant model (if used) are shown on the
            standardized scale.
    }
}

