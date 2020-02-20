# Lab Feb. 20th

# ----------------------------------------------------------------------------
# Regression Tree example using msleep
library(ggplot2)
library(rpart) 
library(rpart.plot)

data('msleep')
str(msleep)
# help('msleep')
str(data)
mSleepDF1 = msleep[, c(3,6,10,11)] # 3 = vore ,6=sleep_total, 10=brainwt, 11=bodywt
str(mSleepDF1)
head(mSleepDF1)

# Building Regression Decision Tree that #predicts the total sleeping
# hours of the mamals based on the other 
# variables available on the dataset

sleepModel_1 = rpart(sleep_total ~., data = mSleepDF1, method = 'anova')
sleepModel_1

rpart.plot(sleepModel_1, type = 3, fallen.leaves = TRUE)
# type = 3, Draw separate split labels for the left and right directions.See the documentation
# fallen.leaves = TRUE, Default TRUE to position the leaf nodes at the bottom of the graph.
rpart.plot(sleepModel_1, type = 3, fallen.leaves = FALSE)

rpart.plot(sleepModel_1, type = 3, digits = 3, fallen.leaves = TRUE)
rpart.plot(sleepModel_1, type = 3, digits = 4, fallen.leaves = TRUE)


# ----------------------------------------------------------------------------
# Classification tree using ctree()
install.packages('C50')
require(C50)

data('iris')
head(iris)
str(iris)
table(iris$Species)

set.seed(9850)
grn = runif(nrow(iris)) # generate random numbers 
irisrand = iris[order(grn), ]

str(irisrand)
classModel_1 = C5.0(irisrand[1:100, -5], irisrand[1:100, 5])
# Fit classification tree models or rule-based models using Quinlan's C5.0 algorithm
classModel_1
summary(classModel_1)

predict_1 = predict(classModel_1, irisrand[101:150, ])
predict_1

table(irisrand[101:150, 5], predict_1)

plot(classModel_1)

# ----------------------------------------------------------------------------
# NaiveBayes Classifier 
# install.packages('e1071')
library('e1071')
classifier = naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[, -5]))
dnn = list('predicted', 'actual')
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col = 'red', main = 'Petal length distribution for the 3 different species')
curve(dnorm(x, 4.260, 0.4699110), add = TRUE, col = 'blue')
curve(dnorm(x, 5.552, 0.5518947), add = TRUE, col = 'green')
