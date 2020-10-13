library(swirl)
swirl()

plot(child ~ parent, galton)
plot(jitter(child,4)~ parent, galton)
regrline <- lm(child ~ parent, galton)
abline(regrline,lwd =3, col="red")

summary(regrline)

# C2 

fit <- lm(child ~ parent, galton)
summary(fit)
mean(fit$residuals)

cov(fit$residuals, galton$parent)

'R fn deviance : Gives the differences between the original response (y) var & estimated (y) variable using the model. 

sqe calculates the sum of the squared residuals.
est calculates childs height (y) using given slope & intercept
 
 
sqe(ols.slope+sl,ols.intercept+ic) == deviance(fit) + sum(est(sl,ic)^2 )
'

ols.ic <- fit$coef[1]
ols.slope <- fit$coef[2]

varChild <- var(galton$child)
sum(varRes <- var(fit$residuals),varEst <- var(est(ols.slope, ols.ic)))

# Eg. 

' Dataset attenu which gives data for 23 earthquakes in California. Accelerations are estimated based on two predictors, distance and magnitude
Generate the regression line for this data.
'
efit <- lm(accel ~ mag +dist, attenu)          

mean(efit$residuals)
cov(efit$residuals, attenu$mag)
cov(efit$residuals, attenu$dist)

l_nor <- lm(gch_nor~ gpa_nor)

# residual variation

fit <- lm(child ~ parent, galton)

sqrt(sum(fit$residuals^2)/ (n-2))
sqrt(deviance(fit)/ (n-2))
summary(fit)$sigma


mu <- mean(galton$child)
sTot <-  sum((galton$child - mu)^2)
sRes <-  deviance(fit)
      
1 - sRes/ sTot
      
cor(galton$child, galton$parent)^2














