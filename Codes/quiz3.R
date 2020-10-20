# Q1
data("mtcars")
fit <- lm(mpg ~ factor(cyl) + wt , mtcars)
summary(fit)

      # -6.07

# Q2
fit1 <- lm(mpg ~ factor(cyl), mtcars)
fit2 <- update(fit1, lm(mpg ~ factor(cyl) + wt, mtcars))

summary(fit1)$coef; summary(fit2)$coef
anova(fit1, fit2)

      # Holding wt const. cyl appears to have less impact on mpg than if wt is disregarded. 
'It is both true and sensible that including weight would attenuate/reduce the effect of number of cylinders on mpg.'

# Q3
fit <- lm(mpg ~ factor(cyl) + wt , mtcars)
fit1 <- lm(mpg ~ factor(cyl) + wt + factor(cyl)*wt , mtcars)
anova(fit, fit1)
      # P val > 0.05. Fail to reject interaction terms not necessary

# Q4
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
      # Est. expected change in mpg / tonne  increase in wt for specific

# Q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit <- lm(y~x)
fitno <- lm(y[-5]~x[-5])
resno <- y[5] - predict(fitno, data.frame(x = x[5]))

1 - (resid(fit)[5] / resno)
hatvalues(fit) # Or influence(lm(y ~ x))$hat
      #0.9946

# Q6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y~x)
hatvalues(fit)
dfbeta(fit)

      # Not -0.378. -0.00134 0.673

# Q7 Possible for 


# Practice Test

# Q1

devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)

devtools::install_github("jhudsl/matahari")
library(matahari)











