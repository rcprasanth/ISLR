# By Prasanth Regupathy
# 6/1/2016

library(ISLR)
library(MASS)

# 10. This question should be answered using the Weekly data set, which
# is part of the ISLR package. This data is similar in nature to the
# Smarket data from this chapter's lab, except that it contains 1, 089
# weekly returns for 21 years, from the beginning of 1990 to the end of
# 2010.
# (a) Produce some numerical and graphical summaries of the Weekly
# data. Do there appear to be any patterns?
str(Smarket)
pairs(Smarket)
summary(Smarket)
cor(Smarket[,1:7])


# (b) Use the full data set to perform a logistic regression with
# Direction as the response and the five lag variables plus Volume
# as predictors. Use the summary function to print the results. Do
# any of the predictors appear to be statistically significant? If so,
# which ones?
ClassificationModel = glm(formula = Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = binomial, data = Smarket)
summary(ClassificationModel)
plot(ClassificationModel)

Smarket$Predicted = predict(object = ClassificationModel, newdata = Smarket, type = "response")
Smarket$Predicted = ifelse(Smarket$Predicted<=0.5, "Down", "Up")

# (c) Compute the confusion matrix and overall fraction of correct
# predictions. Explain what the confusion matrix is telling you
# about the types of mistakes made by logistic regression.
table(Smarket$Direction, Smarket$Predicted)

# (d) Now fit the logistic regression model using a training data period
# from 1990 to 2008, with Lag2 as the only predictor. Compute the
# confusion matrix and the overall fraction of correct predictions
# for the held out data (that is, the data from 2009 and 2010).
attach(Smarket)
traincond = (Year<=2004)
train = Smarket[traincond,]
test = Smarket[!traincond,]

Lag2Logistic = glm(formula = Direction~Lag2, family = binomial, data = train)
summary(Lag2Logistic)

test$Predicted = predict(object = Lag2Logistic, newdata = test, type = "response")
test$Predicted = ifelse(test$Predicted>=0.5, "Up", "Down")

table(test$Direction, test$Predicted)

# (e) Repeat (d) using LDA.

Lag2_lda = lda(Direction~Lag2, data = train)
Lag2_lda
plot(Lag2_lda)

lda_pred = predict(object = Lag2_lda, newdata = test)
ctable = table(test$Direction, lda_pred$class)
ctable = round(ctable/sum(ctable)*100)
ctable

fourfoldplot(ctable, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix, LDA")

# (f) Repeat (d) using QDA.
Lag2_qda = qda(Direction~Lag2, data = train)
Lag2_qda
qda_pred = predict(Lag2_qda, newdata = test)
ctable = table(test$Direction, qda_pred$class)
ctable = round(ctable/sum(ctable)*100)
ctable

fourfoldplot(ctable, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix, QDA")

# (g) Repeat (d) using KNN with K = 1.

# (h) Which of these methods appears to provide the best results on this data?

# (i) Experiment with different combinations of predictors, including
# possible transformations and interactions, for each of the
# methods. Report the variables, method, and associated confusion
# matrix that appears to provide the best results on the held
# out data. Note that you should also experiment with values for
# K in the KNN classifier.

 
# 11. In this problem, you will develop a model to predict whether a given
# car gets high or low gas mileage based on the Auto data set.
# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains
# a value above its median, and a 0 if mpg contains a value below
# its median. You can compute the median using the median()
# function. Note you may find it helpful to use the data.frame()
# function to create a single data set containing both mpg01 and
# the other Auto variables.
 
 
head(Auto)
attach(Auto)

Auto$mpg01 = ifelse(mpg>median(mpg), 1, 0)
attach(Auto)

# (b) Explore the data graphically in order to investigate the association
# between mpg01 and the other features. Which of the other
# features seem most likely to be useful in predicting mpg01? Scatterplots
# and boxplots may be useful tools to answer this question.
# Describe your findings.
str(Auto)
plot(factor(mpg01), displacement)
plot(factor(mpg01), horsepower)
plot(factor(mpg01), weight)
plot(factor(mpg01), acceleration)
plot(factor(mpg01), year)
plot(factor(mpg01), origin)

barplot(table(mpg01, cylinders), legend.text = T)
plot(density(displacement))

# (c) Split the data into a training set and a test set.
RowNum = seq(1:nrow(Auto))
trainRow = sample(x = RowNum, size = nrow(Auto)*0.75)

train = Auto[trainRow, ]
test = Auto[-trainRow, ]

# (d) Perform LDA on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in
# (b). What is the test error of the model obtained?
lda.mpg01 = lda(mpg01~displacement+horsepower+weight+acceleration+year+origin, data = train)
lda.test.results = predict(lda.mpg01, newdata = test)
lda.test.results$class
test$mpg01

conttable = table(lda.test.results$class, test$mpg01)
fourfoldplot(x = conttable, conf.level = F, color = c("#CC6666", "#99CC99"), margin = 1, "LDA")

# (e) Perform QDA on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in
# (b). What is the test error of the model obtained?
qda.model = qda(mpg01~displacement+horsepower+weight+acceleration+year+origin, data = train)
qda.results = predict(object = qda.model, newdata = test)
qda.table = table(qda.results$class,test$mpg01)
fourfoldplot(x = qda.table, conf.level = F, color = c("#CC6666", "#99CC99"), margin = 1, main = "QDA")


# (f) Perform logistic regression on the training data in order to predict
# mpg01 using the variables that seemed most associated with
# mpg01 in (b). What is the test error of the model obtained?
logistic.model = glm(mpg01~displacement+horsepower+weight+acceleration+year+origin, data = train, family = binomial)
test$logistic.predictions = predict(logistic.model, test, type = "response")
test$logistic.predictions = ifelse(test$logistic.predictions>0.5, 1, 0)
logistic.contigency = table(test$logistic.predictions, test$mpg01)
fourfoldplot(x = logistic.contigency, conf.level = F, color = c("#CC6666", "#99CC99"), margin = 1, main = "Logistic")

# (g) Perform KNN on the training data, with several values of K, in
# order to predict mpg01. Use only the variables that seemed most
# associated with mpg01 in (b). What test errors do you obtain?
# Which value of K seems to perform the best on this data set?
library(class)
knn.pred = knn(train[,1:8], test[,1:8], train[,10], k=6)
knn.contingency = table(knn.pred, test$mpg01)
fourfoldplot(x = knn.contingency, conf.level = F, color = c("#CC6666", "#99CC99"), margin = 1, main = "KNN")

# 13. Using the Boston data set, fit classification models in order to predict
# whether a given suburb has a crime rate above or below the median.
# Explore logistic regression, LDA, and KNN models using various subsets
# of the predictors. Describe your findings.
Boston$crim01 = ifelse(Boston$crim>median(Boston$crim),1,0)

trainrowID = sample(x = seq(1:nrow(Boston)), size = round(0.70*nrow(Boston)))
BosTrain = Boston[trainrowID,]
BosTest = Boston[-trainrowID,]

#LDA
lda.model = lda(crim01~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv, BosTrain)
lda.results = predict(lda.model, BosTest)
table(lda.results$class, BosTest$crim01)

#QDA
qda.model = qda(crim01~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv, BosTrain)
qda.results = predict(qda.model, BosTest)
table(qda.results$class, BosTest$crim01)

#Logistic regression
Logistic.model = glm(crim01~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv, BosTrain, family = binomial)
Logistic.predictions = predict(Logistic.model, BosTest, type = "response")
Logistic.predictions = ifelse(Logistic.predictions>0.5,1,0)
table(Logistic.predictions, BosTest$crim01)

#KNN
str(BosTrain)
BosTrain[,2:13]
knn.results = knn(BosTrain[,2:14], BosTest[,2:14], BosTrain[,15], k = 6)
table(knn.results, BosTest$crim01)












