# -------------------------------------------------------------------
# - NAME:        images.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-11-30
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-11-30, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-11-30 17:16 on marvin
# -------------------------------------------------------------------

library("latex2exp")

theta <- list(mu1 = 3, sd1 = 1, mu2 = 6, sd2 = 1.4)

x <- seq(-2, 20, length = 1000)
d1 <- dnorm(x, theta$mu1, theta$sd1)
d2 <- dnorm(x, theta$mu2, theta$sd2)

c1 <- "#000000"
c2 <- "#9ea3b6"

# -------------------------------------------------------------
# -------------------------------------------------------------
pdf(file = "draft_schematic_density.pdf", width = 8, height = 2.3)
    par(mfrow = c(1,3), cex = 1, mar = c(2,2,.5,1))
    
    # --------------------------------------
    plot(NA, xlim = c(0, 10), ylim = c(0, max(d1, d2))*1.05, yaxs = "i",
         xaxt = "n", yaxt = "n", bty = "l", xlab = NA, ylab = NA, main = NA)
    mtext(side = 1, line = 3, "x")
    
    polygon(x, 0.5 * d1 + 0.5 * d2, col = "gray90", border = NA)
    lines(x, 0.5 * d1 + 0.5 * d2, col = 2, lty = 3, lwd = 2)
    lines(x, 0.5 * d1, col = c1, lwd = 2)
    lines(x, 0.5 * d2, col = c2, lwd = 2)
    
    axis(side = 1, col.axis = c1, col = c1, at = theta$mu1, expression(mu[1]))
    axis(side = 1, col.axis = c2, col = c2, at = theta$mu2, expression(mu[2]))
    axis(side = 1, lwd = 0, at = 9, expression("x"%->%""))
    axis(side = 2, line = -.50, lwd = 0, at = .2, "density") #expression("density"%->%""))
    
    text(theta$mu1, 0.5 * dnorm(0, 0, theta$sd1), pos = 3, col = c1, TeX(sprintf("\\tilde{p}_1 = %.1f", 0.5)))
    text(theta$mu2, 0.5 * dnorm(0, 0, theta$sd2), pos = 3, col = c2, TeX(sprintf("\\tilde{p}_2 = %.1f", 0.5)))
    
    # --------------------------------------
    plot(NA, xlim = c(0, 10), ylim = c(0, max(d1, d2))*1.05, yaxs = "i",
         xaxt = "n", yaxt = "n", bty = "l", xlab = NA, ylab = NA, main = NA)
    mtext(side = 1, line = 3, "x")
    
    polygon(x, 0.9 * d1 + 0.1 * d2, col = "gray90", border = NA)
    lines(x, 0.9 * d1 + 0.1 * d2, col = 2, lty = 3, lwd = 2)
    lines(x, 0.5 * d1, col = c1, lty = 2)
    lines(x, 0.5 * d2, col = c2, lty = 2)
    lines(x, 0.9 * d1, col = c1, lwd = 2)
    lines(x, 0.1 * d2, col = c2, lwd = 2)
    
    axis(side = 1, col.axis = c1, col = c1, at = theta$mu1, expression(mu[1]))
    axis(side = 1, col.axis = c2, col = c2, at = theta$mu2, expression(mu[2]))
    axis(side = 1, lwd = 0, at = 9, expression("x"%->%""))
    axis(side = 2, line = -.50, lwd = 0, at = .2, "density") #expression("density"%->%""))
    
    text(theta$mu1, 0.9 * dnorm(0, 0, theta$sd1), pos = 3, col = c1, TeX(sprintf("\\tilde{p}_1 = %.1f", 0.9)))
    text(theta$mu2, 0.1 * dnorm(0, 0, theta$sd2), pos = 3, col = c2, TeX(sprintf("\\tilde{p}_2 = %.1f", 0.1)))
    
    # --------------------------------------
    plot(NA, xlim = c(0, 10), ylim = c(0, max(d1, d2))*1.05, yaxs = "i",
         xaxt = "n", yaxt = "n", bty = "l", xlab = NA, ylab = NA, main = NA)
    mtext(side = 1, line = 3, "x")
    
    polygon(x, 0.1 * d1 + 0.9 * d2, col = "gray90", border = NA)
    lines(x, 0.1 * d1 + 0.9 * d2, col = 2, lty = 3, lwd = 2)
    lines(x, 0.5 * d1, col = c1, lty = 2)
    lines(x, 0.5 * d2, col = c2, lty = 2)
    lines(x, 0.1 * d1, col = c1, lwd = 2)
    lines(x, 0.9 * d2, col = c2, lwd = 2)
    
    axis(side = 1, col.axis = c1, col = c1, at = theta$mu1, expression(mu[1]))
    axis(side = 1, col.axis = c2, col = c2, at = theta$mu2, expression(mu[2]))
    axis(side = 1, lwd = 0, at = 9, expression("x"%->%""))
    axis(side = 2, line = -.50, lwd = 0, at = .2, "density") #expression("density"%->%""))
    
    text(theta$mu1, 0.1 * dnorm(0, 0, theta$sd1), pos = 3, col = c1, TeX(sprintf("\\tilde{p}_1 = %.1f", 0.1)))
    text(theta$mu2, 0.9 * dnorm(0, 0, theta$sd2), pos = 3, col = c2, TeX(sprintf("\\tilde{p}_2 = %.1f", 0.9)))
    
dev.off()
    
    
    
