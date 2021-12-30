library(ggplot2)
library(car)
library(caret)
library(corrplot)
install.packages('car')
install.packages('corrplot')

#Loading data
data(mtcars) 

View(mtcars)
# Looking at variables
str(mtcars)
dim(mtcars)
head(mtcars)
summary(mtcars)
names(mtcars)

#------------------------
# Data Preparation
# converting variables to factors.
mtcars$am   = as.factor(mtcars$am)
mtcars$cyl  = as.factor(mtcars$cyl)
mtcars$vs   = as.factor(mtcars$vs)
mtcars$gear = as.factor(mtcars$gear)

#Dropping dependent variable for calculating Multicollinearity

mtcars_a = subset(mtcars, select = -c(mpg))


#Identifying numeric variables
numericData <- mtcars_a[sapply(mtcars_a, is.numeric)]

#Calculating Correlation
descrCor <- cor(numericData)
descrCor

# Print correlation matrix and look at max correlation
print(descrCor)









