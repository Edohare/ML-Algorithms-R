data_1 = read.csv("2014.csv")
data_1$year = "2014"
data_2 = read.csv("2015.csv")
data_2$year = "2015"
data_3 = read.csv("2016.csv")
data_3$year = "2016"
df_1 = data_1[,c("id", "sale", "units", "rating", "product", "industry", "country", "return.client", "year")]
df_2 = data_2[,c("id", "sale", "units", "rating", "product", "industry", "country", "return.client", "year")]
df_3 = data_3[,c("id", "sale", "units", "rating", "product", "industry", "country", "return.client", "year")]
df = rbind(df_1,df_2,df_3)
str(df)
#I figured out this for loop later.
#data = data.frame()
#for (i in 2014:2016) {
 # filename = paste(i,".csv",sep="")
  #temp = read.csv(filename)
  #temp = temp[,"id"]
  #temp$year =  i
  #if (nrow(data)==0){
   # data=temp
 # } else{
  #  data = rbind(data, temp)
  #}
#}
# We have 1673 observations, and 9 variables
# We sell 7 different products, to 4 different industries, in 17 different countries

levels(df$country)
df$country = as.character(df$country) # converts the factor type to a character type
df$country[df$country=="Switzerland, Switzerland"] = "Switzerland" # replaces switzerland
keep_country = c("Switzerland", "United States", "United Kingdom")
df$country[!(df$country %in% keep_country)] ="other"
df$country = as.factor(df$country)
levels(df$country)
df[1:100,"country"]
levels(df$product)
df$product = as.character(df$product)
df$product[df$product=="Series A13"] = "Series A"
df$product[df$product=="Series A2"] = "Series A"
df$product[df$product=="Series B55"] = "Series B"
df$product = as.factor(df$product)# renames product
levels(df$product)
levels(df$industry)
df$industry = as.character(df$industry)
df$industry[df$industry=="999"] = NA
df$industry = as.factor(df$industry)#recodes indusstry
levels(df$industry)
colSums(is.na(df))#finds how many NA's in each col
df$product = as.character(df$product)
df$product[is.na(df$product)]="Delta"#recordes NA to delta
df$product = as.factor(df$product)#recodes product
colSums(is.na(df))
levels(df$industry)
str(df)
df_new = na.omit(df)#removes NA's
str(df_new)
df_new$sale.per.unit = df_new$sale / df_new$units #new col for SPU
str(df$rating)

df_new$rating.level= 1 #Rating level for loop
for (i in 1:nrow(df_new)) {
  if (df_new$rating[i]>=5){
    df_new$rating.level[i] = "Excellent"
  }
else if (df_new$rating[i]>4 & df_new$rating[i]<5){
  df_new$rating.level[i]="Satisfactory"
}else{
    df_new$rating.level[i]="Poor"
}
}

df_new$priority = 1 #Priorty for loop
for (i in 1:nrow(df_new)) {
  if(df_new$return.client[i]==1 & df_new$rating.level[i]=="Poor"){
    df_new$priority[i] = 1 #sets it to a 1 if both are true
}
else{
  df_new$priority[i]=0
}
}
