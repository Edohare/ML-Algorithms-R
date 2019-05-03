library(MASS)
dim(Boston)
#500 rows, 14 columns

x=1:nrow(Boston)
#chooses all the rows
size = nrow(Boston)*.7
#chooses 70 percent of the boston data
train.index = sample(x,size,replace=FALSE)
#assigns our x and size to our created training set
train.index

train = Boston[train.index,]
#take our full boston data set, and select our training index
test= Boston[-train.index,]
#this chooses the remainder of our
model=lm(medv~age, data = train)
#creates a model of median home value against age on our training set
train.pred.y = predict(model, train)
train.pred.y
# this is a vector of all our predictions

#RMSE = root mean squared error
error = train.pred.y-train$medv
# find the error by taking our model against our training value models of the actual median housing value
#predicted value - actual value
sq.error= error^2
#this is how we sqaure the error, to remove the negatives
mse = mean(sq.error)
#then we take the mean of the squared error
rmse = sqrt(mse)
# take the square root of the mean squared error
rmse
# we have a value of 9, typically on average our predictions are off by about $9000

test.pred.y=predict(model, test)
#test
test.pred.y
#estimates how much the test set 
rmse = sqrt(mean((test.pred.y-test$medv)^2))
rmse
#Our answer comes back as above 9, which means our error is very slim

#END OF THE HOLD OUT METHOD____________________________________________________________________________________

#START OF K FOLD VALIDATION METHOD_____________________________________________________________________________
k=5
fold=  sample(1:k,nrow(Boston), replace = TRUE)
#size is 1-k, assigning each row a randomly picked value of 1-5, true means you can pick the same row twice
fold

set.seed(1)

kfold.rmse = 1:k
#a spaceholder that creates k rooms for the for loop
for (i in 1:k) {
  test = Boston[fold==i,]
  train = Boston[fold!=i,]
  model = lm(medv~age, data = train)
  test.pred.y = predict(model, test)
  rmse = sqrt(mean((test.pred.y-test$medv)^2))
  
  kfold.rmse[i] = rmse
  #this puts the rmse into the kfold rooms
}
#on the first loop, it says i = 1, for values that are in group 1 use as test set, and use the rest of the groups for the train set
#groups 2-5
#on the second loop, it says i=2, for values that are in group 2 use as test set, use rest of groups as train set 1,3-5
#etc
kfold.rmse
# k means we get to pick what k equals
#rmse, says overall, how off are we with our predictions
mean(kfold.rmse)
#this is our average test error
#END OF KFOLD--------------------------------------------------------------------------------------------------------

#START OF LOOCV - LEAVE ONE OUT CROSS VALIDATION----------------------------------------------------------------------
#take out one row as the test set, and use all the rest for the training set, and do this nrow number of times
loocv.rmse = 1:nrow(Boston)
#creates the rooms for all the rmse values
for (i in 1:nrow(Boston)) {
  test = Boston[i,]
  train = Boston[-i,]
  #creates on test on i, and uses everything else as training set
  model = lm(medv~age, data = train)
  test.pred.y = predict(model, test)
  rmse = sqrt(mean((test.pred.y-test$medv)^2))
  
  loocv.rmse[i]= rmse
}
loocv.rmse
mean(loocv.rmse)

#END OF LOOCV----------------------------------------------------------------------------------------------

#START OF BOOTSTRAP-------------------------------------------------------------------------------
boot.index = sample(1:nrow(Boston),nrow(Boston), replace = TRUE)
boot.index
#goes through the entire test set, picking it nrow number of times, but allows the same data to be chosen multiple times
#this usually allows around 2/3 data to be chosen

train = Boston[boot.index,]
#View(train)
test =  Boston[-boot.index,]

model = lm(medv~age, data = train)
test.pred.y = predict(model, test)
rmse = sqrt(mean((test.pred.y-test$medv)^2))
rmse
