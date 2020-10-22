# Q1
library(MASS)
fit <- glm(use ~ wind, shuttle, family = binomial)
exp(coef(fit))
      # 0.969

shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind, data = shuttle, family = binomial)
exp(coef(fit))


# Q2

Ad_Y <- exp(resid(glm(use ~ magn , shuttle, family = binomial)))
Ad_Y <- Ad_Y / (1 + Ad_Y )

Ad_X <- exp(resid(glm(wind ~ magn , shuttle, family = binomial)))
Ad_X <- Ad_X / (1 + Ad_X)
      
fit1 <- glm(Ad_Y ~ Ad_X, family = binomial)
exp(coef(fit1))
 # or

fit <- glm(auto ~ headwind + magn, data = shuttle, family = binomial)
exp(coef(fit))


# Q3 not Coeff get inverted. They reverse sign

# Q4
data("InsectSprays")
head(InsectSprays)
fit <- glm(count ~ spray, InsectSprays, family = poisson)
summary(fit) 
exp(coef(fit))

      # 0.9457

# Q5 not Coef is unchanged, divided by 10, not subtracted by log10

# Q6

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

fit <- lm(y~x)
summary(fit)
abline(fit, lwd = 3)

knots = seq(-5,5, 11)
splineTerms <- sapply(knots, function(knot) {(x > knot) * (x - knot)})
xMat <- cbind(1, x, z)
mdl <- (lm(y ~ xMat - 1))

abline(mdl, lwd = 3)
#  1.013

z <- (x > 0) * x

plot(z,y)
