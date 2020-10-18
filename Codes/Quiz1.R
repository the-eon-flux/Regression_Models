# Q1 : 

x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

mean(x)
mean(x*w)
sum(c(w*(x - round(mean(x),1))^2 ))
sum(c(w*(x - round(mean(x*w),1))^2 ))
sum(c(w*(x - round(mean(rep(x ,w)),1))^2 ))



# Q2 : Fit the regression through the origin and get the slope treating y

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

coef(lm(y~x -1))

# Q3 : Fit the regression model with mpg as the outcome and weight as the predictor. 

data(mtcars)
coef(lm(mpg~wt, data = mtcars))[2]

# Q4 : 

sd(X) = 0.5*sd(Y)

cor(X,Y) = 0.5

beta1 = 0.5 * (sd(y)/ sd(x))
beta1 = 0.5 * (sd(y)/ 0.5*sd(Y))

'Hence,  slope coefficient for the regression model = 1 '

# Q5 :
0.4 * 1.5

# Q6 : 

x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
((x - mean(x)) / sd(x))[1]

# Q7 : 

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

coef(lm(y~x))[1]

# Q8 : 
'Intercept must be identically 0'

# Q9 : 

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

# Q10 : 

beta1 = cor(X,Y) * sd(y) / sd(x)
gama1 = cor(x,y) * sd(x) / sd(y)




























