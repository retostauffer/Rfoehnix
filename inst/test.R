# -------------------------------------------------------------------
# - NAME:        test.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-11-28
# -------------------------------------------------------------------
# - DESCRIPTION: Development test script for foehton, simple method.
# -------------------------------------------------------------------
# - EDITORIAL:   2018-11-28, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-15 15:32 on marvin
# -------------------------------------------------------------------

library("foehnix")

# Loading the two data sets from the package
data("ellboegen", package = "foehnix")
data("sattelberg", package = "foehnix")

sattelberg  <- zoo(sattelberg[,-1], as.POSIXct(sattelberg[,1], origin = "1970-01-01", tz = "UTC"))
ellboegen   <- zoo(ellboegen[,-1],  as.POSIXct(ellboegen[,1],  origin = "1970-01-01", tz = "UTC"))
ellboegen$diff_t <- sattelberg$t + 10.27 - ellboegen$t
ellboegen$minus_diff_t <- -ellboegen$diff_t

## Testing one model with an intercept only concomitant model (typically not to useful)
load_all("foehnix"); x1 <- foehnix(ff ~ 1, data = ellboegen, windsector = c(43, 223),
                                     standardize = TRUE, family = "gaussian")
load_all("foehnix"); x2 <- foehnix(ff ~ rh + diff_t, data = ellboegen, windsector = c(43, 223),
                                     standardize = TRUE, family = "gaussian", maxit = 200)
load_all("foehnix"); x3 <- foehnix(minus_diff_t ~ rh + ff, data = ellboegen, windsector = c(43, 223),
                                     standardize = TRUE, family = "gaussian", maxit = 200)
load_all("foehnix"); x4 <- foehnix(ff ~ rh + diff_t, data = ellboegen, windsector = c(43, 223),
                                     standardize = TRUE, family = "cgaussian", maxit = 200)
load_all("foehnix"); x5 <- foehnix(ff ~ rh + diff_t, data = ellboegen, windsector = c(43, 223),
                                     standardize = TRUE, family = "tgaussian", maxit = 200)
load_all("foehnix"); x6 <- foehnix(ff ~ rh + diff_t, data = ellboegen, windsector = c(43, 223),
                                     standardize = TRUE, family = "clogistic", maxit = 200)

# Coefficients
lapply(list(x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, x6=x6), function(x) tail(x$optimizer$coefpath,1))
sapply(list(x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, x6=x6), function(x) x$optimizer$loglik)
which.max(sapply(list(x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, x6=x6), function(x) x$optimizer$loglik))

plot(x5, xtra = x2$prob)


























##load_all("foehnix"); xgauss <- foehnix(ff ~ 1, data = ellboegen, windsector = c(43, 223),
##                                     standardize = TRUE, family = "tgaussian")
##load_all("foehnix"); xgauss <- foehnix(ff ~ 1, data = ellboegen, windsector = c(43, 223),
##                                     standardize = TRUE, family = "logistic")
##load_all("foehnix"); xgauss <- foehnix(ff ~ 1, data = ellboegen, windsector = c(43, 223),
##                                     standardize = TRUE, family = "clogistic")
##load_all("foehnix"); xgauss <- foehnix(ff ~ 1, data = ellboegen, windsector = c(43, 223),
##                                     standardize = TRUE, family = "tlogistic")

# density tests # library("devtools")
# density tests # load_all("foehnix")
# density tests # prob <- c(0.3, 0.5, 0.3)
# density tests # post <- c(0.2, 0.3, 0.2)
# density tests # y <- c(5,3,5)
# density tests # 
# density tests # #load_all("foehnix"); f1 <- foehnix_gaussian(); f2 <- foehnix_cgaussian(); f3 <- foehnix_tgaussian()
# density tests # load_all("foehnix"); f1 <- foehnix_logistic(); f2 <- foehnix_clogistic(); f3 <- foehnix_tlogistic()
# density tests # theta <- list(mu1 = 3, mu2 = 8, logsd1 = 1, logsd2 = 2)
# density tests # load_all("foehnix"); f1$d(y, theta$mu1, exp(theta$logsd1), log = FALSE); f2$d(y, theta$mu1, exp(theta$logsd1), log = FALSE); f3$d(y, theta$mu1, exp(theta$logsd1), log = FALSE)
# density tests # load_all("foehnix"); f1$d(y, theta$mu1, exp(theta$logsd1), log = TRUE);  f2$d(y, theta$mu1, exp(theta$logsd1), log = TRUE);  f3$d(y, theta$mu1, exp(theta$logsd1), log = TRUE)
# density tests # load_all("foehnix"); f1$p(y, theta$mu1, exp(theta$logsd1), log = FALSE); f2$p(y, theta$mu1, exp(theta$logsd1), log = FALSE); f3$p(y, theta$mu1, exp(theta$logsd1), log = FALSE)
# density tests # load_all("foehnix"); f1$p(y, theta$mu1, exp(theta$logsd1), log = TRUE);  f2$p(y, theta$mu1, exp(theta$logsd1), log = TRUE);  f3$p(y, theta$mu1, exp(theta$logsd1), log = TRUE)
# density tests # load_all("foehnix"); f1$loglik(y, post, prob, theta); f2$loglik(y, post, prob, theta); f3$loglik(y, post, prob, theta)
# density tests # load_all("foehnix"); f1$posterior(y, prob, theta);    f2$posterior(y, prob, theta);    f3$posterior(y, prob, theta)
# density tests # # TODO: Check truncated and censored values against CRCH, there seem to be some problems
# density tests # #       in the logistic t/c source files (optimization looks rubbish).
# density tests # 
# density tests # library("crch")
# density tests # x <- sort(c(0,seq(-2, 10, length = 100)))
# density tests # plot(NA, xlim = c(-2,10), ylim = c(0,.4))
# density tests #     lines(x, f1$d(x, 2, 1, log = FALSE), col = 1)
# density tests #     lines(x, f2$d(x, 2, 1, log = FALSE), col = 2)
# density tests #     lines(x, f3$d(x, 2, 1, log = FALSE), col = 3)
# density tests #     lines(x, dcnorm(x, 2, 1, left = 0, right = 4), lty = 3)
# density tests #     lines(x, dtnorm(x, 2, 1, left = 0, right = 4), lty = 3)
# density tests # library("crch")
# density tests # x <- sort(c(0,seq(-2, 10, length = 100)))
# density tests # plot(NA, xlim = c(-2,10), ylim = c(0,1))
# density tests #     lines(x, f1$p(x, 2, 1, log = FALSE), col = 1)
# density tests #     lines(x, f2$p(x, 2, 1, log = FALSE), col = 2)
# density tests #     lines(x, f3$p(x, 2, 1, log = FALSE), col = 3)
# density tests #     lines(x, pcnorm(x, 2, 1, left = 0, right = 4), lty = 3)
# density tests #     lines(x, ptnorm(x, 2, 1, left = 0, right = 4), lty = 3)


##  Sollte folgendes ausspucken:
##          Comp.1   Comp.2 
##  mu    2.470216 8.648086
##  sigma 1.472877 3.181229
##  (Intercept)  9.148858
##  rh          -0.091941
##  diff_t      -1.009777
##  EM step  78/100, log-likelihood sum: -72917.36046
load_all("foehnix"); xgauss <- foehnix(ff ~ rh + diff_t, data = ellboegen, windsector = c(43, 223), maxit = 1000,
                                     standardize = TRUE, family = "gaussian", verbose = TRUE)
load_all("foehnix"); bgauss <- advanced_foehnix(ff ~ rh + diff_t, data = ellboegen, windsector = c(43, 223), maxit = 1000,
                                     standardize = TRUE, family = "gaussian")
load_all("foehnix"); xgaussS <- foehnix(ff ~ 1, data = ellboegen, windsector = c(43, 223), maxit = 1000,
                                     standardize = TRUE, family = "gaussian")
load_all("foehnix"); bgaussS <- advanced_foehnix(ff ~ 1, data = ellboegen, windsector = c(43, 223), maxit = 1000,
                                     standardize = TRUE, family = "gaussian")
print(summary(xgauss))
print(summary(bgauss))
structure(cbind(coef(xgauss), coef(bgauss)), dimnames = list(names(coef(xgauss)), c("xgauss", "bgauss")))

#par(mfrow = c(1,2))
#plot(xgauss$prob, bgauss$prob)
#plot(xgaussS$prob, bgaussS$prob)

par(mfrow = c(2,2))
plot_llpath(xgaussS)
plot_llpath(bgaussS)
plot_coefpath(xgaussS)
plot_coefpath(bgaussS)
par(mfrow = c(2,2), mar = rep(1,4))
plot_llpath(xgauss)
plot_llpath(bgauss)
plot_coefpath(xgauss, diff = FALSE)
plot_coefpath(bgauss, diff = FALSE)
par(mfrow = c(2,2), mar = rep(1,4))
plot_llpath(xgaussS, diff = FALSE)
plot_llpath(bgaussS, diff = FALSE)
plot_coefpath(xgaussS, diff = FALSE)
plot_coefpath(bgaussS, diff = FALSE)
cat(sprintf("Gaussian mixture model logllik:   %10.2f (in %5.2f min)\n", xgauss$optimizer$loglik$full, xgauss$time))
cat(sprintf("Gaussian bfgs based model:        %10.2f (in %5.2f min)\n", bgauss$optimizer$loglik$full, bgauss$time))

#load_all("foehnix"); xx <- foehndiag(sqrt(ff) ~ diff_t + rh, data = ellboegen, windsector = c(43, 223))
load_all("foehnix"); x1 <- foehnix(ff ~ 1, data = ellboegen, windsector = c(43, 223),
                                 standardize = TRUE, family = "gaussian", lambda.min = "AIC", nlambda = 100)
load_all("foehnix"); x1 <- foehnix(ff ~ 1, data = ellboegen, windsector = c(43, 223),
                                 standardize = TRUE, family = "logistic", lambda.min = "AIC", nlambda = 100)
load_all("foehnix"); x2 <- foehnix(ff ~ rh + diff_t, data = ellboegen, windsector = c(43, 223),
                                 standardize = TRUE, family = "gaussian")
load_all("foehnix"); x3 <- foehnix(ff ~ rh + diff_t, data = ellboegen, windsector = c(43, 223),
                                     standardize = TRUE, family = "gaussian", alpha = 1)





load_all("foehnix"); xlogis <- foehnix(ff ~ rh + diff_t, data = ellboegen, windsector = c(43, 223),
                                     standardize = TRUE, family = "logistic", lambda.min = "AIC", nlambda = 10)

load_all("foehnix"); bgauss <- advanced_foehnix(ff ~ rh + diff_t, data = ellboegen, windsector = c(43, 223),
                                     standardize = TRUE, family = "gaussian", lambda.min = "AIC", nlambda = 10)
load_all("foehnix"); blogis <- advanced_foehnix(ff ~ rh + diff_t, data = ellboegen, windsector = c(43, 223),
                                     standardize = TRUE, family = "logistic", lambda.min = "AIC", nlambda = 10)

ccmodel <- blogis$optimizer$ccmodel

# Coefficients
structure(cbind(coef(xgauss), coef(bgauss), coef(xlogis), coef(blogis)),
          dimnames = list(names(coef(xgauss)), c("xgauss", "bgauss", "xlogis", "blogis")))
stop()

# Logliks
cat(sprintf("Gaussian mixture model logllik:   %10.2f (in %5.2f min)\n", xgauss$optimizer$loglik$full, xgauss$time))
cat(sprintf("Gaussian bfgs based model:        %10.2f (in %5.2f min)\n", bgauss$optimizer$loglik$full, bgauss$time))
cat(sprintf("Logistic mixture model logllik:   %10.2f (in %5.2f min)\n", blogis$optimizer$loglik$full, xlogis$time))
cat(sprintf("Logistic bfgs based model:        %10.2f (in %5.2f min)\n", blogis$optimizer$loglik$full, blogis$time))
xx <- xgauss

stop('----- dev stop after xgauss -----')

coef(xx)
summary(xx)

par(mfrow=c(2,3))
plot(NA, xlim = c(0,10), ylim = c(0,1))
x <- seq(-3,13, length = 1000)
lines(x, dnorm(x, xx$coef$mu1, xx$coef$sd1), col = 1)
lines(x, dnorm(x, xx$coef$mu2, xx$coef$sd2), col = 2)

matplot(xx$optimizer$loglikpath, lty = 1, col = c(5,3,2), type = "b")
legend("left", legend = colnames(xx$optimizer$loglikpath), col = c(5,3,2), lty = 1)

set.seed(NULL)
plot(ellboegen$ff, xx$prob, main = "Probs", col = sample(1:10,1),
     ylim = c(0,1), yaxs = "i")
abline(h = seq(.1, .9, by = 0.1), col = "gray", lty = 5)


idx <- as.integer(which.max(xx$prob[201:(length(xx$prob)-201)]))[1] + 201 + seq(-200,200)
#idx <- as.integer(which.max(xx$prob)[1]) + seq(-200,200)
x <- merge(ellboegen, xx$prob)
names(x)[ncol(x)] <- "prob"
plot(x$ff[idx])
par(new = TRUE);
plot(x$prob[idx], col = 2, yaxt = "n", ylim = c(0,1), yaxs = "i")


load_all("foehnix"); plot(xx)#, start = start, end = end, xtra = cls)
start <- as.POSIXct(c("2016-01-01", "2017-01-01"))
end   <- start + 86400 * 10
xx <- bgauss
load_all("foehnix"); plot(xx, ndays = 50, xtra = blogis$prob)#cls)

xx <- bgauss
load_all("foehnix"); plot(xx, start = "2016-01-01", end = "2016-05-20", xtra = blogis$prob)

par(mfrow = c(1,2))
plot(bgauss$prob, xgauss$prob)
plot(blogis$prob, xlogis$prob)

cat("-----------------------------\n")
print(coef(cls))
cat("-----------------------------\n")
print(summary(xx))




