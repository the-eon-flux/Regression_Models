# Introduction to Multivariable Regression.

'Remember we eiminate the intercept by subtracting the means from data points
       A regression in one variable done by : lm(child ~ parent, galton) to get
       2 coefficients, a slope and an intercept.

The intercept is really the coefficient of a special regressor which has the same value, 1, at every sample. The function, lm, includes this regressor by
default. The default intercept can be excluded by using -1 in the formula.

We will demonstrate by substituting an all-ones regressor of our own. This regressor must have the same number of samples as galton (928.) 

'
ones <- rep(1, nrow(galton))

#  Perform a regression which substitutes our regressor, ones for the default

lm(child ~ ones + parent -1, galton)
      # The coefficient of ones is 23.9415.
lm(child ~ parent, galton)
      # Both the intercepts have same values.
      # This regression as we have seen involves actually 2 regressors, parent & a regressor of all ones.

'We have seen that,
      The regression line given by lm(child ~ parent, galton) goes through the point x=mean(parent), y=mean(child).
      If we subtract the mean from each variable, the regression line goes through the origin, x=0, y=0, hence its intercept is zero.
            The idea behind this is that by subtracting the means, we eliminate one of the 2 regressors, the constant; leaving just one regressor, "parent" with a coeff. slope.
            
      Subtracting the means to eliminate intercept is a special technique called Gaussian Elimination.
      
Generalising this idea for multivariate regression : 
      Pick one regressor and to replace all other variables by the residuals of their regressions against that one.

In the above special case, it would be the residual of a regression against the constant 1.

Explanation in detail [Skip if understood]:

When doing a regression of var(Y) by against itself /against constant 1 you get the coeff. as the mean of var (Y).
Thus if you subtract the mean of Y from all Y values it equals to say that you are replacing the effect var Y by the residual (Variation unexplained by Y) of regression against constant 1.

coeff(lm(child ~ 1, galton)) == mean(child)

'

# To illustrate the general case we'll use the trees data from the datasets package. 

'
Idea is to predict the Volume of timber which a tree might produce from measurements of its Height and Girth.
 To avoid treating the intercept as a special case, we have added a column of 1s to the data which we shall use in its place.

'
head(trees)

# Refer file elimination.R to look at the code which eliminates 1 regressor from the data

fit <- lm(Volume ~ Girth + Height + Constant - 1, trees)

# let's eliminate Girth from the data set

trees2 <- eliminate("Girth", trees)

head(trees2)

# In trees2, the Constant column is not constant coz The constant, 1, has been replaced by its residual when regressed against Girth.

# Now create a model, called fit2, using the reduced data set.
fit2 <- lm(Volume ~ Height + Constant -1, trees2)

# Let's look at the coefficients from both fits

lapply(list(fit, fit2), coef)
# Thus we have eliminated a regressor by accounting for it and got the same result.


# We have illustrated that regression in many variables amounts to a series of regressions in one.

# We just have to pick any regressor and replace the outcome and all other regressors by their residuals against the chosen one.

## MultiVar Examples 2.

# Swiss data
'
It was gathered in 1888, a time of demographic change in Switzerland, and measured six quantities in 47 French-speaking provinces of Switzerland.

Plot shows here a 6 by 6 array of scatterplots for pairwise relationships between the variables.
'

# Plot

# . denotes all 5 variables
all <- lm(Fertility ~ . , swiss)
summary(all)

summary(lm(Fertility ~ Agriculture, swiss))

# Code swissLMs.r shows how & when the agriculture var changes its sign from +ve to -ve

# Run function from the other script
makelms()

# Now we'll show what happens when we add a variable that provides no new linear information to a model

ec <- swiss$Examination + swiss$Catholic
efit <- lm(Fertility ~ . + ec, swiss)

# We'll see that R ignores this new term since it doesn't add any information to the model.

coef(efit) - coef(all) #or 
all$coefficients-efit$coefficients

## Regression models with more than one independent variables.
#  InsectSprays data

# Objectives : 
'
Multilevel factor levels and learning to interpret linear models of data with more than 2 factors.

'
sapply(InsectSprays,class)

#  Data has 6 factors so we need 6 dummy variables. Each will indicate if a particular outcome (a count) is associated with  specific factor or category (insect spray).

# linear model in which count is the dependent variable and spray is the independent
   fit <- lm(count ~. ,InsectSprays)
   est <- summary(fit)$coef[,1]

   'R returns a 6 by 4 array. sprayA does not appear explicitly in the list of Estimates. It is there, however, as the first entry in the Estimate column; labeled as "(Intercept).
   Because sprayA is the first in the alphabetical list of the levels of the factor
    by default uses the first level as the reference against which the other levels or groups are compared when doing its t-tests (shown in the third column).
'

# Interpret linear model coefficients
   "
   The intercept is the mean of the reference group, in this case sprayA.
   The other estimates are the distance of other groups' mean from the ref mean
   "
   
# Proof
   mean(sA)
   mean(sB) # Equal to est of sA + est of sB (as the est other than that of sA are distances from sA's estimate)
   
# To not let any var to be a reference, we omit the intercept. Generate another model of this data, this time omitting the intercept.
   # the tests are whether the expected counts (the groups means) are different from zero.

   nfit <- lm(count ~ spray - 1, InsectSprays)
   summary(nfit)$coef
   # The means of all the groups are now explicitly shown

# Changing the reference group by using relevel to re-order the levels of a factor.
   spray2 <- relevel(InsectSprays$spray, "C")   
   fit2 <- lm(count ~ spray2, InsectSprays)
   summary(fit2)$coef
   
   mean(sC)

# Computing test statistic from the 3rd column in coef table. Eg. spray2B t value
   (fit$coef[2] - fit$coef[3]) / 1.60110
   
      
## MultiVar Examples 3.
# WHO hunger data concerns young children from around the world and rates of hunger among them which the organization compiled over a number of years.
   dim(hunger)
      # 948*13
   names(hunger)
   
   '
   The Numeric column for a particular row tells us the percentage of children under age 5 who were underweight when sample was taken. This is our outcome for following models. This is our rate of hunger.
   '

   # Looking at the rate of hunger over time
   fit <- lm(Numeric ~ Year, hunger)
   summary(fit)$coef

   # Rate of hunger according to sex
   lmF <- lm(hunger$Numeric[hunger$Sex == "Female"] ~ hunger$Year[hunger$Sex== "Female"] , hunger)
   lmM <- lm(hunger$Numeric[hunger$Sex == "Male"] ~ hunger$Year[hunger$Sex== "Male"] , hunger)

   '
   We can see from the slopes that males have higher -ve values.
   '

   # Now let's add the gender as another predictor to create the linear model lmBoth.
   
   lmBoth <- lm(Numeric ~ Year + Sex, hunger)
   summary(lmBoth)

   ' 3 estimates are given; the intercept, one for Year and one for Male.
   Since sex variables are categorical in value hence they are factors in this model.
   Thus in this case Female is our ref variable and its estimate, the intercept which represents the percentage of hungry females at year 0.
   '
   
   # After giving sex as predictor variable the slope has changed. Now it is constant for both male and female. This might be due to the interaction between year and gender. 

   # we can now add an interaction term to this model and see how that affects changes in rates of hunger.
   lmInter <- lm(Numeric ~ Year + Sex + Year * Sex, hunger)   
   summary(lmInter)

   # Male has the steeper slope here.
   # The percentage of hungry females at year 0 is given by the intercept
   # The percentage of hungry males  at year 0 is given by the intercept + coef(SexMale)
   
   # The annual change in percentage of hungry females is given by the Year est.
   # The annual change in percentage of hungry males is given by the 
      # Year:SexMale est + Year est 
   # ( As the year:sexMale est represents the distance of the annual change in percent of males from that of females.)


## Summary
   
   " Suppose we have two interacting predictors and one of them is held constant.
         The expected change in the outcome for a unit change in the other predictor is the coefficient of that changing predictor + the coefficient of the interaction * the value of the predictor held constant. 
   
   Suppose the linear model is Hi = b0 + (b1*Ii) + (b2*Yi)+ (b3*Ii*Yi) + ei.
      Suppose H's represent the outcomes, the I's and Y's the predictors neither of which is a category, 
      the b's represent the estimated coefficients of the predictors.
   
      This equation models a continuous interaction since neither I nor Y is a category or factor.
      We can ignore the e's which represent the residuals of the model.
   
   Suppose we fix I at value 5 and let Y vary. Then the expr becomes : 
      b2+b3*5
   
   "
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   

