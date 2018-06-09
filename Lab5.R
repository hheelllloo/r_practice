rm(list=ls())
curve
curve(dt(x,1), add=T, col="red")
curve(dt(x,5), add=T, col="red")
curve(dt(x,15), add=T, col="red")
curve(dt(x,30), add=T, col="red")
pt(1, 6) - pt(-1, 6)
1 - pt(1, 6)
1 - pnorm(1)
t = -qnorm(0.025)
a = t*14/sqrt(5)
80 - a
80 + a
80 - qnorm(0.99)*14/sqrt(5)
b = qt(0.995, 4)*14/sqrt(5)
qt(0.005, 120, lower.tail=F)

