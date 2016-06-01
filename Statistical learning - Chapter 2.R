# Prasanth Regupathy
# May 29, 2016
# Load library
library(ISLR)

# Attach College dataset
attach(College)

###################     Question 8       #########################

# Summary of dataset
summary(College)

# Scatterplot matrix for first 10 variables
pairs(College[,1:10])

# Boxplot for outstate by Private
boxplot(Outstate~Private)

# Create qualitative variable - Elite.
# If top 10 percent is greater than 50, then Elite is "Yes". Otherwise, it is a "No".
College$Elite = factor(ifelse(Top10perc>50, "Yes", "No"))
attach(College)

# Check summary of newly created variable
summary(Elite)

# Boxplot for outstate by Elite
boxplot(Outstate~Elite)

par(mfrow=c(2,2))

VarNames = colnames(College)

for (i in 2:(length(College)-1))
{
  hist(College[,i], col = "lightgreen", main = VarNames[i], xlab = VarNames[i])
}

detach(College)

###################     Question 9       #########################
Auto = na.omit(Auto)

attach(Auto)

Autonames = names(Auto)


for (i in 1:length(Auto))
{
  if (class(Auto[,i]) == "numeric")
  {
    print(Autonames[i])
    print("Range")
    print(range(Auto[,i]))
    print(paste("Mean:",mean(Auto[,i])))
    print(paste("variance:", var(Auto[,i])))
    print(" ")
  }
}


pairs(Auto)


###################     Question 10       #########################
library(MASS)
?Boston
dim(Boston)

pairs(Boston)

table(Boston$chas)

median(Boston$ptratio)

Boston[which.min(x = Boston$medv),]

nrow(Boston[Boston$rm>7,])
nrow(Boston[Boston$rm>8,])

summary(Boston[Boston$rm>8,])
pairs(Boston[Boston$rm>8,])

View(Boston)






















