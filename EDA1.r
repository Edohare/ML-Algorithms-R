install.packages("tidyverse")
install.packages("car")
library(car)
hw2 = read.csv(url("http://guides.newman.baruch.cuny.edu/ld.php?content_id=39953204"), sep =";")
str(hw2)
#Section I-2; we have 500 obs with 19 variables
for (col in names(hw2)[3:7]) {
  hw2[,col] = as.factor(hw2[,col])
}
str(hw2)
str(hw2$Paid)
#Section I - 3
str(hw2$Category)
levels(hw2$Category)
levels(hw2$Category) = c("action","product","inspiration")
#Section I - 4
levels(hw2$Paid) = c("Non-Paid", "Paid") # converts Paid from 0 and 1, to non-paid and paid
levels(hw2$Paid)
#Section I - 5
colSums(is.na(hw2))
#we have a total of 5 NA's
hw = na.omit(hw2)
#Section II - 1
summary(hw$share)
sd(hw$share)
#This says that we can get anywhere from 0 to 790 shares, but 790 is a huge outlier
#We normally get shares around 10-30, but since we have such a large SD we could easily be getting a normal of 0-80 shares

#Section II-2 A and B
hist(hw$share,main="Share Histogram",xlab = "Number of Shares", ylab = "Number of Posts", xlim=c(0,200),breaks = 100)
#Histogram tells us that our posts get anywhere from 0-60 shares

# section II-3 A B And C
plothw=table(hw$Category, hw$Post.Month)
barplot(plothw, main="Category Vs Month",
        xlab="Month",ylab = "Number of Posts per Category", col=c("darkblue","red","green"),
        legend = rownames(plothw),args.legend = list(x ='topleft', bty='n'))
summary(hw$Paid)
prop.table(summary(hw$Paid))



#Section III - 1
scatterplot(share~like, data = hw, xlab = "Likes", ylab = "Shares", main="Likes Compared to Shares")
#this relationship is direct, more shares=more likes

#Section III-2
x=hw$Post.Month
y=hw$share
plot(x,y,xlab = "Month", ylab = "Shares", main = "Shares V Month", ylim = c(0,200))
#Key insight would be that we have a pretty steady amount of shares when comparing it to months
#Shares are not really affected by month, so there is no point in focusing on a certain month

#Section III-3A
z = hw$Paid
aggregate(y~z, data = hw, summary)
#Paid leads to a slightly higher mean, and a larger outlier, data would be skewed to right
xtabs(~z+y,data = hw)
#Our xtabs shows that our non-paid posts are almost always more consistent that our paid posts. Paid may not be worth it

#Section III-3B
plot(z,y,xlab = "Non-paid V Paid", ylab = "Shares", main= "Shares V Non-Paid or Paid", ylim=c(0,200))
#Our boxplot shows that the difference between non-paid and paid is negligible. Meaning we should stop paying.

#Section III-3C
g1=hw$share[hw$Paid=="Paid"]
g2=hw$share[hw$Paid=="Non-Paid"]
t.test(g1,g2)
#our t test also shows we have a confidence interval that goes between 0 and its greater than .05 which makes it invalid.
#We should stop paying for advertising 

#Section IV 1A
par(mfrow=c(1,3))
x1 = hw$Paid[hw$Category=="action"]
y1 = hw$share[hw$Category=="action"]
plot(x1,y1, main="Action",ylab= "Shares", ylim=c(0,200))

x2 = hw$Paid[hw$Category=="product"]
y2 = hw$share[hw$Category=="product"]
plot(x2,y2, main="Product",ylab= "Shares", ylim=c(0,200))

x3 = hw$Paid[hw$Category=="inspiration"]
y3 = hw$share[hw$Category=="inspiration"]
plot(x3,y3,  main="Inspiration",ylab= "Shares", ylim=c(0,200))
#compares x an y in a box plot
#Insight
#Our nonpaid posts do almost as good as our paid posts for every category
#If we wanted to keep paying for one type of post, product would be the only post that may be worth paying for

#Section IV - 1B
v1=hw$share[hw$Category=="action"][hw$Paid=="Paid"]
v2=hw$share[hw$Category=="product"][hw$Paid=="Paid"]
v3=hw$share[hw$Category=="inspiration"][hw$Paid=="Paid"]
v4=hw$share[hw$Category=="action"][hw$Paid=="Non-Paid"]
v5=hw$share[hw$Category=="product"][hw$Paid=="Non-Paid"]
v6=hw$share[hw$Category=="inspiration"][hw$Paid=="Non-Paid"]
#ACTION T TEST
t.test(v4,v1)
#PRODUCT T TEST
t.test(v5,v2)
#INSPIRATION T TEST
t.test(v6,v3)
