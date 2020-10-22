# Load data

data("mtcars")

mtcars$am_X <- 1 * mtcars$am ==0


fit <- lm(mpg~factor(am)-1, mtcars)
summary(fit)
anova(fit)

plot(fit)
