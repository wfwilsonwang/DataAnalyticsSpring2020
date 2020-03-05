# Lab Mar.5th 
# Trees, Hierarchical Clustering, Heatmaps

# creating a matrix data with random numbers
# and plotting the matrix using the image() function
# you will see there, it does not have a real pattern in the plot.
set.seed(12345)
# Set the seed of R‘s random number generator, which is useful 
# for creating simulations or random objects that can be reproduced.
help(par)
# par can be used to set or query graphical parameters. 
# Parameters can be set by specifying them as arguments to 
# par in tag = value form, or by passing them as a list of 
# tagged values.
par(mar = rep(0.2, 4))
data_Matrix = matrix(rnorm(400), nrow = 40)
# random generation for the normal distribution with mean 
# equal to mean and standard deviation equal to sd.
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])
# t: matrix transpose 

help(heatmap)
help(rep) # rep replicates the values in x. 

par(mar = rep(0.2, 4))
heatmap(data_Matrix)
# When we run the heatmap() here, we get the dendrograms printed on 
# the both columns and the rows and still there is no real immerging 
# pattern that is interesting to us, it is because there is no real 
# interesting pattern underlying in the data we generated.

help(rbinom) # random generation for the binomial distribution
set.seed(678910) 
for(i in 1:40){
  # flipping a coin and getting the data
  coin_Flip = rbinom(1, size = 1, prob = 0.5)
  # if the coin is "Heads", add a common pattern to that row, 
  if(coin_Flip){
  data_Matrix[i, ] = data_Matrix[i, ] + rep(c(0,3), each = 5) }
}
# repeat 0, 3, each for 3 times 
par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

par(mar=rep(0.2, 4)) 
heatmap(data_Matrix)

hh = hclust(dist(data_Matrix))
help(hclust) # Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.
data_Matrix_Ordered = data_Matrix[hh$order, ]
par(mfrow = c(1, 3))
image(t(data_Matrix_Ordered)[, nrow(data_Matrix_Ordered): 1])
plot(rowMeans(data_Matrix_Ordered), 40:1, xlab = 'The row mean', ylab = 'Row', pch = 19)
plot(colMeans(data_Matrix_Ordered), xlab = 'Column', ylab = 'Column Mean', pch = 19)


# Random Forest
library(randomForest)
data1 = read.csv(file.choose(), header = TRUE) 
head(data1)                 
colnames(data1) = c('BuyingPrice', 'Maintenance', 'NumDoors', 'NumPersons', 'BootSpace', 'Safety', 'Condition')                 
head(data1)                 
str(data1)                 
levels(data1$Condition)                 
summary(data1)                 

set.seed(100)
train = sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet = data1[train, ]
ValidSet = data1[-train, ]

help("randomForest")
# randomForest implements Breiman's random forest algorithm 
model1 = randomForest(Condition~., data = TrainSet, importance = TRUE)
model1

model2 = randomForest(Condition~., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model2 

predTrain = predict(model2, TrainSet, type = 'class')
table(predTrain, TrainSet$Condition)
predValid = predict(model2, ValidSet, type = 'class')
table(predValid, ValidSet$Condition)

importance(model2)
















