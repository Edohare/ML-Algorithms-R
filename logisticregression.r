url = url("http://guides.newman.baruch.cuny.edu/ld.php?content_id=40679293")
Default = read.csv(url)
str(Default)
set.seed(1)
train.index = sample(1:nrow(Default),nrow(Default)*0.8)
train = Default[train.index,]
test = Default[-train.index,]

# Number of observations in training and test set:
data.frame(full.size=nrow(Default),train.size=nrow(train),test.size=nrow(test))

model1 = glm(default~., data = train, family = "binomial")
#binomial is two humps, because it can only be 1 or 0
summary(model1)
# the coefficient estimates are the probability of the y happening

exp(coef(model1))
#extracts the coeffiecients, and exponeniate them, if we exponeniate our log odds, we have regular odds/probability
#for every unit increase in balance, we would expect a .005% increase in our odds of defaulting
#e-01 means .5
#lets says baseline odds of defaulting are 66%, so if they are a student, your odds are defaulting are cut in half
#values that are negative, it has a negative(downward) impact on our outcome
#positive signage, we see a positive relationship between the two

pred.prob =  predict(model1, test)
#this will show us the log odds by default
pred.prob
#lets convert it to probability
pred.prob2 = predict(model1, test, type = "response")
pred.prob2

pred.cl =  ifelse(pred.prob2>.5, "yes","no")
pred.cl
table(pred.cl)

#compare our predictions against the actual values of the test set
c.matrix = table(test$default,pred.cl)
c.matrix

acc = (c.matrix[1]+c.matrix[4])/sum(c.matrix)
acc
#we have a 97.5% accuracy
#our model is doing better than the original table results

sens.yes = (c.matrix[4])/(c.matrix[2]+c.matrix[4]); sens.yes
#we only predicted 31% of the defaulters, not good
#lowering the .5 threshold would make us more lenient

prec.yes = (c.matrix[4])/(c.matrix[3]+c.matrix[4]);prec.yes
#76% of the time when we predict someone will default, they do

pred.cl2 =  ifelse(pred.prob2>.2, "yes","no")
pred.cl2
table(pred.cl)

c.matrix2 = table(test$default,pred.cl2)
c.matrix2

acc2 = (c.matrix2[1]+c.matrix2[4])/sum(c.matrix)
acc2

sens.yes2 = (c.matrix2[4])/(c.matrix2[2]+c.matrix2[4]); sens.yes

prec.yes2 = (c.matrix2[4])/(c.matrix2[3]+c.matrix2[4]);prec.yes
