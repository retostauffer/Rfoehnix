\name{iwls_logis}
\alias{iwls_logis}
\title{IWLS Solver for Binary Logistic Regression Model}
\usage{
    iwls_solver(X, y)
}
\arguments{
    \item{X}{model matrix including intercept (if required).}
    \item{y}{response, either binary (containing only \code{0}s and \code{1}s)
        or values within \code{[0-1]}.}
    \item{beta}{initial regression coefficients. If not set all coefficients
        will be initialized with \code{0}.}
    \item{maxit}{integer, maximum number of iterations.}
    \item{tol}{float, tolerance for the log-likelihood comparison. If the
        improvement of the last step is smaller than \code{tol} the algorithm
        stops due to convergence.}
    \item{...}{currently unused.}
}
\value{
    Returns a matrix size \code{p x 1} where \code{p} is the
    number of coefficients (equal to the number of columns in
    the model matrix \code{X}.
}
\details{
    Solves the following problem:

    \code{\beta = ((X * W^\frac{1}{2})^top (X * W^\frac{1}{2}))^{-1} * (X * W^\frac{1}{2})^\top * W^\frac{1}{2} * z}

    ... where ...

    \code{z = \eta + (y - \mu) \frac{d \eta}{d \mu}}

}
\examples{
# Example data set
data("airquality")
airquality <- na.omit(airquality)
airquality$Ozone <- as.numeric(airquality$Ozone > 50)

# glm model
m1 <- glm(Ozone ~ ., data = airquality, family = binomial(link = "logit"))

# iwls solver
mf <- model.frame(Ozone ~ ., data = airquality)
m2 <- iwls_logit(model.matrix(Ozone ~ ., data = airquality), model.response(mf))

# Comparing coefficients
print(cbind(coef(m1), m2))
}
