x <- seq(-3, 3, by = .005)
# Choose the mean as 2.5 and standard deviation as 0.5.
y <- dnorm(x, mean = 0, sd = 1)

# Give the chart file a name.
# png(file = "dnorm.png")

png(file = "C:/R/script/graph/dnorm.png", width = 12, height = 5, units = "in")
png(file = "C:/R/script/graph/dnorm.png", width = 12, height = 5)
png(file = "C:/temp/dnorm.png", width = 12, height = 5, units = "in")

par(mfrow=c(1,2))
# Symetric
plot(x,y, type="n", xlab="", ylab="")
lines(x, y, lwd=1)
title("80% Confidence Interval")

abline(h=0, lwd=2); abline(v=0, lwd=2)
grid()

x10 <- qnorm(0.1)
y10 <- dnorm(x10)

plot.new()
segments( x10, 0, x1 = x10, y1 = dnorm(x10), col = 2, lwd=0.1)
segments(-x10, 0, x1 =-x10, y1 = dnorm(x10), col = 2, lwd=0.1)

text(-1.7, 0.03, "10%" )
text( 1.7, 0.03, "10%" )
text( 0.0, 0.1, "80%" )

### Asymetric
plot(x,y, type="n", xlab="", ylab="")
lines(x, y, lwd=1)
title("(20%, 60%) Confidence Interval")

abline(h=0, lwd=2); abline(v=0, lwd=2)
grid()

x20 <- qnorm(0.2)
y20 <- dnorm(x20)
x60 <- qnorm(0.6)
y60 <- dnorm(x60)

plot.new()
segments( x20, 0, x1 = x20, y1 = dnorm(x20), col = 2, lwd=0.1)
segments( x60, 0, x1 = x60, y1 = dnorm(x60), col = 2, lwd=0.1)

text(-1.4, 0.08, "20%" )
text(-0.2, 0.1, "40%" )
text( 1.2, 0.07, "40%" )

dev.off()  
  
### plot.new has not been called yet

