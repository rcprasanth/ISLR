library(ISLR)
library(GGally)

attach(Auto)
str(Auto)

# Question 8

Model1 = lm(data = Auto, formula = mpg~horsepower)

summary(Model1)

# i. Is there a relationship between the predictor and the response?

# ii. How strong is the relationship between the predictor and the response?

# iii. Is the relationship between the predictor and the response positive or negative?

# iv. What is the predicted mpg associated with a horsepower of 98? What are the associated 95% confidence and prediction intervals?
predict(object = Model1, data.frame(horsepower = c(98)), interval = "confidence")
predict(object = Model1, data.frame(horsepower = c(98)), interval = "prediction")


# (b) Plot the response and the predictor. Use the abline() function to display the least squares regression line.
plot(horsepower, mpg)
abline(Model1)

# (c) Use the plot() function to produce diagnostic plots of the least squares regression fit. Comment on any problems you see with the fit.
opar = par()
par(mfrow = c(2,2))
plot(Model1)
par(opar)

# 9. This question involves the use of multiple linear regression on the Auto data set.
# (a) Produce a scatterplot matrix which includes all of the variables in the data set.
pairs(Auto)

# (b) Compute the matrix of correlations between the variables using the function cor(). You will need to exclude the name variable, cor() which is qualitative.
str(Auto)
cor(Auto[,1:8])

# (c) Use the lm() function to perform a multiple linear regression with mpg as the response and all other variables except name as the predictors. 
MulLinear = lm(formula = mpg~.-name, data = Auto)

# Use the summary() function to print the results.Comment on the output. 
summary(MulLinear)

# For instance:
# i. Is there a relationship between the predictors and the response?
# ii. Which predictors appear to have a statistically significant relationship to the response?
# iii. What does the coefficient for the year variable suggest?

# (d) Use the plot() function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit.
# Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?
opar = par()
par(mfrow = c(2,2))
plot(MulLinear)
par(opar)

# (e) Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions appear to be statistically significant?
step(MulLinear,~.^2)

# (f) Try a few different transformations of the variables, such as log(X),???X, X2. Comment on your findings.



