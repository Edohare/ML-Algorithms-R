install.packages("randomForest")
library(MASS)
library(randomForest)
install.packages("caret")
str(Boston)
train.index = sample(1:nrow(Boston),nrow(Boston)*0.7, replace = FALSE)
train = Boston[train.index,]
test = Boston[-train.index,]

reg.model = randomForest(medv~., data = train)
reg.model
#tells us our problem is a regression or classification problem
#uses boostrap method to create 500 trees
#regression, total number of predictors /3
#classification, the sqroot of the number of predictors
#4 total variables for each tree

#12 is MSE, 3.5 RMSE
#rsquared is 87.23%

plot(reg.model)
# shows us its performance depending on the number of trees, says how well we perform
#100 trees is about the same as 500 trees in terms of performance

#MODEL TUNING............................................................................

reg.model.100 =  randomForest(medv~.,data = train,ntree=100,mtry=6,importance=TRUE)
#force the model to stop at 100 trees instead of 500, helps with computation time for huge data sets
#mtry uses different numbers of variables instead of the norm
#importance determines the importance of the variables in the models
reg.model.100
#our MSE is slightly better, var explained is about the same
#..........................................................................................
importance(reg.model.100)
#purity is the split that gives us the minimized rss
#if rm is in the decision tree, it does 19% better, etc

varImpPlot(reg.model.100)
#plots the list of numbers from importance

pred.y = predict(reg.model.100,test)
pred.y
#prints out the predictions for our model on the test set data

error = test$medv-pred.y
error
sq.err = error^2
mse = mean(sq.err)
rmse = sqrt(mse)
rmse


#CLASSICATION.............................................................................
train$medv = ifelse(train$medv>20,"high","low")
test$medv = ifelse(test$medv>20,"high","low")
table(as.factor(test$medv)) # checks out the distribution
cl.model = randomForest(as.factor(medv)~.,data = train,importance=TRUE)
cl.model

pred.cl = predict(cl.model,test)
table(pred.cl)

confusionmatrix()
