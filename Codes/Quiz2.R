#q1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

summary(lm(y~x))

#q2 : Residual std. var
summary(lm(y~x))$sigma

#q3 : 
data("mtcars")
y = mtcars$mpg; x = mtcars$wt
fit <- lm(y~x)
predict(fit, newdata = data.frame(x= mean(x)) ,interval = "confidence")
# 18.99

#q4 : Est change in mpg per 1000lb increase in wt.

#q5 :
predict(fit, newdata = data.frame(x = 3), interval = "prediction")


#q6 : 
fit1 <- lm(y~I(x*2))
summary(fit1)
summary(fit)
predict(fit,newdata=data.frame(x=2),interval = "confidence")
# -12


fit <- lm(mpg ~ I(wt * 0.5), data = mtcars)
confint(fit)[2, ]

# 4.2

#q7 : multiplied by 100

#q8 : New intercept beta0 + c * beta1

#q9 : Not 0.75. 0.5

#q10 : If intercept included then sum = 0

