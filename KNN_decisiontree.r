install.packages('tree')
setwd("C:/Users/Ed")
data = read.csv("telemarketing.csv")
library(tree)
library(class)
data
set.seed(1)
train.index = sample(1:nrow(data),nrow(data)*0.8, replace = FALSE)
train = data[train.index,]
test = data[-train.index,]
#1C
table(as.factor(train$y))
#Our proportion is 3206 no's and 410 yes's
#Given this we would expect our model to predict around 13% yes's

#2A
model = tree(y~.,data = train)
plot(model)
text(model)
#2B
best.tree = cv.tree(model, K=10)
best.tree
plot(best.tree$size,best.tree$dev, type = "b", col="red", main = "Find Best Tree")
pruned.tree = prune.tree(model, best=4)

plot(pruned.tree)
text(pruned.tree)

predi.y = predict(pruned.tree, test)
predi.y

model.pruned = prune.tree(model,best=4)

#2C
pred.class = predict(model.pruned,test,type="class") 
c.matrix = table(test$y,pred.class); c.matrix
acc = mean(test$y==pred.class)
sens.high = c.matrix[1]/(c.matrix[1]+c.matrix[3])
prec.high = c.matrix[1]/(c.matrix[1]+c.matrix[2])

data.frame(acc,sens.high,prec.high)

#KNN>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
str(data)

normalize = function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
norm.age = normalize(data$age)
norm.balance = normalize(data$balance)
norm.duration = normalize(data$duration)
norm.campaign = normalize(data$campaign)
norm.data = cbind(data[,c(3:4,7)],norm.age,norm.balance,norm.duration,norm.campaign)
norm.data$housing = as.character(norm.data$housing)

norm.data$housing[norm.data$housing=="no"] = 0 
norm.data$housing[norm.data$housing=="yes"] = 1 

str(norm.data$housing)

norm.data$contact = as.character(data$contact)
norm.data$contact.cellular[data$contact=="cellular"]=1
norm.data$contact.cellular[!(data$contact=="cellular")]=0
norm.data$contact.telephone[data$contact=="telephone"]=1
norm.data$contact.telephone[!(data$contact=="telephone")]=0
norm.data$contact.unknown[data$contact=="unknown"]=1
norm.data$contact.unknown[!(data$contact=="unknown")]=0
str(norm.data)

norm.data$contact=NULL
str(norm.data)
set.seed(1)
train.index = sample(1:nrow(norm.data),nrow(norm.data)*0.80)

train = norm.data[train.index,]
test = norm.data[-train.index,]

#3b
rep = seq(1,5,1) 
rep.acc = rep
rep.sens = rep
rep.prec = rep


k=5
fold = sample(1:k,nrow(train.x),replace=TRUE)


iter=1 
for (K in rep) {
  
  
  kfold.acc = 1:k
  kfold.sens = 1:k
  kfold.prec = 1:k
  
  for (i in 1:k) {
    
    test.kfold = train.x[fold==i,]
    train.kfold = train.x[fold!=i,]
    
    test.cl.actual = train.cl[fold==i]
    train.cl.actual = train.cl[fold!=i]

    pred.class = knn(train.kfold,test.kfold,train.cl.actual,k=K)

    c.matrix = table(test.cl.actual,pred.class)
    acc = mean(pred.class==test.cl.actual)
    sens.yes = c.matrix[4]/(c.matrix[2]+c.matrix[4])
    prec.yes = c.matrix[4]/(c.matrix[3]+c.matrix[4])

    kfold.acc[i] = acc
    kfold.sens[i] = sens.yes
    kfold.prec[i] = prec.yes
  }

  rep.acc[iter] = mean(kfold.acc)
  rep.sens[iter] = mean(kfold.sens)
  rep.prec[iter] = mean(kfold.prec)
  iter=iter+1
}

par(mfrow=c(1,3))
metric = as.data.frame(cbind(rep.acc,rep.sens,rep.prec))
color = c("blue","red","gold")
title = c("Accuracy","Sensitivity","Precision")

for (p in 1:3) {
  plot(metric[,p],type="b",col=color[p],pch=20,
       ylab="",xlab="K",main=title[p],xaxt="n")
  axis(1,at=1:5,labels=rep,las=1)
}

results = as.data.frame(cbind(rep,rep.acc,rep.sens,rep.prec))
names(results) = c("K","accuracy","sensitivity","precision")
results

#3D
train.x = train[,c(1,3:9)]
test.x = test[,c(1,3:9)] 
train.cl = train[,2] 
K = 3 

pred.class = knn(train.x,test.x,train.cl,k=K)
c.matrix = table(test$y,pred.class)
c.matrix
acc = mean(pred.class==test$y)
acc
sens.yes = c.matrix[4]/(c.matrix[2]+c.matrix[4])
sens.yes
prec.yes = c.matrix[4]/(c.matrix[3]+c.matrix[4])
prec.yes

train.x = train[,c(1,3:9)]
test.x = test[,c(1,3:9)] 
train.cl = train[,2] 
K = 5 

pred.class2 = knn(train.x,test.x,train.cl,k=K)
c.matrix = table(test$y,pred.class2)
c.matrix
acc2 = mean(pred.class==test$y)
acc2
sens.yes2 = c.matrix[4]/(c.matrix[2]+c.matrix[4])
sens.yes2
prec.yes2 = c.matrix[4]/(c.matrix[3]+c.matrix[4])
prec.yes2
