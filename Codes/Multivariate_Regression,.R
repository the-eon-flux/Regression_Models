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
   
## Residual diagnostics and variation  
   library(swirl)
   swirl()
      
   # Outliers may or may not belong in the data.
   '
    In order to spot them, R provides various diagnostic plots and measures of influence.
    The basic technique is to examine the effects of leaving one sample out, as we did in comparing the black and orange
   '
   # The influential outlier is in a data frame named out2. 2 cols (y & x)
   fit <- lm(y ~ x, out2)
   
   # 1.) The simplest diagnostic plot displays residuals versus fitted values.
         '
         Residuals should be uncorrelated with the fit, independent & (almost) identically distributed with mean 0.   
         '
         plot(fit, which=1)
         # We see because of the outlier a linear trend. Now we remove that point & re-fit the model
         
         fitno <- lm(y ~ x, out2[-1,])
         plot(fitno, which = 1)   
         # Linear trend from the above plot has gone.   
         
         # And the measure of influence it had, can simply be calculated by
         coef(fit) - coef(fitno)
         
   # 2.) The function, dfbeta, does the equivalent calculation for every sample in the data.
   
         View(dfbeta(fit))
         '
          When a sample is included in a model, it pulls the regression line closer to itself than that of the model which excludes it. Its residual, the difference between its actual y value and that of a regression line, is thus smaller in magnitude when it is included than when it is omitted.
          The ratio of these two residuals is therefore small in magnitude for an influential sample. 
          For a sample which is not influential the ratio would be close to 1. Hence, 
          1 minus the ratio is a measure of influence, 
          near 0 for points which are not influential, and 
          near 1 for points which are.
          This measure is called influence, sometimes leverage, and sometimes hat value.
          
          The ratio"s numerator is the residual of the first sample of the model with outlier.
          Denominator has to be calculated as when we exclude the outlier, it"s residual is also not calculated.
          
          We can easily doing this by predicting value of y for fitno model and subtract it from the actual value.
          
         '   
         
         resno <- out2[1,"y"] - predict(fitno, out2[1,])
         # Influence
         1- resid(fit)[1]/resno
   
   # 3.) hatvalues: The function, hatvalues, performs for every sample a calculation equivalent to the one you've just done. 
         head(hatvalues(fit))
   
      '
      Residuals of individual samples are sometimes treated as having the same variance, which is estimated as the sample variance of the entire set of residuals.
      Theoretically, however, residuals of individual samples have different variances and these differences can become large in the presence of outliers.
      Standardized and Studentized residuals attempt to compensate for this effect in two slightly different ways. Both use hat values.
      '
   
   # 4.) Standardised residuals: 
      # calculate the sample standard deviation of fit's residual, by dividing fit's deviance, i.e., its residual sum of squares, by the residual degrees of freedom and taking the square root.
      # deviance is sum(resid^2)
      # degrees of freedom would be n - 2
      sigma <- sqrt(deviance(fit)/df.residual(fit))
   
      "
      Ordinarily we would just divide fit's residual (which has mean 0) by sigma. In the present case we 
         multiply sigma times sqrt(1-hatvalues(fit)) to estimate standard deviations of individual samples.  
         Result is called is called the standardized residual.
      "
      rstd <- resid(fit)/(sigma * sqrt( 1 - hatvalues(fit)))
      # rstandard: The function, rstandard, computes the standardized residual which we have just computed.
      head(cbind(rstd, rstandard(fit)))
      # A Scale-Location plot shows the square root of standardized residuals against fitted values.
      plot(fit, which=3)
      "
      Most of the diagnostic statistics under discussion were developed because of perceived shortcomings of other diagnostics and because their distributions under a null hypothesis could be characterized. e assumption that residuals are approximately normal is implicit in such characterizations. 
      Since standardized residuals adjust for individual residual variances, a QQ plot of standardized residuals against normal with constant variance is of interest. Following plot shows that.
            "
      plot(fit, which=2)
      # Outliers are few sd's away from mean
   
   # 5.) Studentized residuals, (sometimes called externally Studentized residuals,) estimate the standard deviations of individual residuals using, in addition to individual hat values, the deviance of a model which leaves the associated sample out. We'll illustrate using the outlier. 
      
      "
      calculate the sample standard deviation of fitno's residual by dividing its deviance, by its residual degrees of freedom and taking the square root.
      "
      sigma1 <- sqrt(deviance(fitno)/ df.residual(fitno))
      # Calculate the Studentized residual for the outlier sample by dividing resid(fit)[1] by the product of sigma1 and sqrt(1-hatvalues(fit)[1]).
      resid(fit)[1] / (sigma1 * sqrt(1-hatvalues(fit)[1]) )

   # rstudent: The function, rstudent, calculates Studentized residuals for each sample using a procedure equivalent to that which we just used for the outlier.     
      rstudent(fit)[1]
      
      
   # 6.) Cook's distance is the last influence measure. Essentially tells how much a given sample changes a model. 
      "
      It's essentially the sum of squared differences between values fitted with and without a particular sample. 
      It's normalised (divided by) residual sample variance times the number of predictors which is 2 in our case (intercept & x)
      "
      
      # calculating the difference in predicted values between fit and fitno
      
      dy <- predict(fitno, out2) - predict(fit, out2)
      sum(dy^2) / (2 * sigma^2)      
      
      # The function, cooks.distance, will calculate Cook's distance for each sample. 
      plot(fit, which=5)
      
      
      
      
      
      
      
      
      
      
      
      
      
      