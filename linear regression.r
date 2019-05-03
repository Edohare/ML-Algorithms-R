install.packages("ISLR")
install.packages("leaps")
library(ISLR)
library(leaps)
?College
str(College)
model = lm(Grad.Rate~S.F.Ratio, data=College)
summary(model)
plot(College$S.F.Ratio, College$Grad.Rate, main = "Grad Rate VS S.F.Ratio")
#B
abline(model, col="blue")
#C
#as student faculty ratio increases, we can expect grad rates to fall
#D
model2 = lm(Grad.Rate~S.F.Ratio+I(S.F.Ratio^2), data=College)
summary(model2)
plot(model2)
#sfratio squared is not significant, meaning no relationship
#E
model3= lm(Grad.Rate~S.F.Ratio*Apps, data=College)
summary(model3)
confint(model3)
#it has a negative interaction effect, meaning the more s.f.ratio and the more apps, the worse the grad rate is

#2

model4= lm(Grad.Rate~Private+Top25perc+Outstate+Room.Board, data=College)
summary(model4)
# A
# We expect the private institutions to do better because it is a positive coefficient, it would do about 4% better
#B our rsquared is around 40% so our model is only showing about 40% of our variance
#Our RSE is showing that we tend to be off by about 13%
par(mfrow=c(2,2))
plot(model4)

#C
confint(model4)


Private= "No"
Top25perc = 55
Outstate =  25000
Room.Board = 4000
new_data=data.frame(Private, Top25perc,Outstate,Room.Board)
new_data
summary(new_data)
predict(model4,new_data)
predict(model4,new_data,interval = "prediction")

#Our model gives us a wide margin
#D
#tells us there is a very wide margin, so it may not be a reliable figure.
par(mfrow=c(2,2))
plot(model4)
#E
# If regression was violated it would present fanning or funneling, qq plot woul be off

#3
full_model=lm(Grad.Rate~., data=College)
summary(full_model)
#A adjusted rsquard for full model is 0.4495
model_fwd = regsubsets(Grad.Rate~., data=College,
                       nvmax=NULL, method="forward")

summary(model_fwd)
model_fwd_summary = summary(model_fwd) # Store summary output
which.max(model_fwd_summary$adjr2) # Display best subset by adjr2
summary(model_fwd)$which[13,]
#B
#There 13 variables in this model, with 4 left out: Enroll, Books, Accept, and S.F.Ratio
#C
best_model_fwd = lm(Grad.Rate~Private+Apps+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+Personal+PhD+Terminal+perc.alumni+Expend,data=College)
summary(best_model_fwd)
#Adjusted R sqaured for new model is .45. There is almost no difference between the two models, meaning that our smaller model is not much different than our full model
plot(model_fwd, scale="adjr2", main="Forward Selection: AdjR2")
#8th model has the lowest rsquared
summary(model_fwd)$which[8,]
#8th model has Apps, Top25Perc, p.undergrad, outstate, Room.board, personal, perc.alumni, and expend
