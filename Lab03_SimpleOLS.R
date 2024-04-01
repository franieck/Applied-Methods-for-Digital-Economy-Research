setwd("C:\\Users\\lucas\\Documents\\NOVA SBE\\PhD\\Methodology\\Applied Methods for Digital Economy Research\\Lab 3")
list.files()
sales <- data.table(read.csv("C:\\Users\\lucas\\Documents\\NOVA SBE\\PhD\\Methodology\\Applied Methods for Digital Economy Research\\Lab 3\\sales-data.csv")) #load CSV file
dt.sales <- data.table(sales)
rm(sales)
ncol(dt.sales)
nrow(dt.sales)
head(dt.sales)

stargazer(dt.sales, type = "text") #show descriptives
summary(dt.sales)

qplot(data=dt.sales              #scatter plot
      ,x=advertising
      ,y=sales
      ,geom="point")

dt.sales[,cor(sales,advertising)]  #correlation between sales and advertising
install.packages(Hmisc)
library(Hmisc)
dt.sales[,rcorr(sales,advertising)] #tests significance of the correlation > needs Hmisc installation (bug)

dt.sales[,cor.test(sales,advertising)] #tests significance of the correlation

lm.sales <- lm(sales ~ advertising,data=dt.sales) #create a linear model with x= advertising and y= sales based on the dataset dt.sales
summary(lm.sales) # "summary" function gives us the coefficient estimates, sig levels and Rsquared.Can also be done with Stargazer
stargazer(lm.sales, type="text")

coeffs <= coefficients(lm.sales)  #gives us the coefficients (the constant b0 and the slope b1)
coeffs

qplot(data=dt.sales
      ,x=advertising
      ,y=sales
      ,geom=c("point","smooth")
      ,method=lm)+
  theme_bw()+
  labs(x="advertising dollars", y="sales dollars")

#predicting values:
advertising=100
sales=coeffs(1)+coeffs(2)*advertising
sales=147.59047+13.40054*advertising
sales
#alternatively:
my.budget= data.table(advertising=100)
predict(lm.sales,my.budget)
#setting interval to "predict" allows us to show the 95% confidence interval of the prediction
predict(lm.sales, my.budget, interval="predict")
