library(ggplot2)
library(UsingR)
data(galton) 

# Normal regression
y = galton$child
x = galton$parent
beta1 <- cor(y,x)*(sd(y)/sd(x))
beta0 <- mean(y) - beta1*mean(x)
Manual_Regression <- c(beta0, beta1)

# Using R lm()
Lm_R <- coef(lm(y~x))

# results
rbind(Manual_Regression,Lm_R )


## Statement 1 : Slope of regression through the origin =  Slope linear regression for mean centered values

# Regression through the origin
xc = x - mean(x)
yc = y - mean(y)
beta1 <- sum(yc *xc) / sum(xc^2)

# Or just beta1 <- coef(lm(yc~xc-1))

# results
c(beta1, coef(lm(yc~xc))[2])

' Therefore, slope of regression through the center = slope for mean centered values of x&y'










































