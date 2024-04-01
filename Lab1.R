setwd("C:\\Users\\lucas\\Documents\\R\\Lab1")
load("ceosal2.RData")
install.packages("data.table")
library(data.table)
dt.ceo.salaries <- data.table(data)
rm(data)
names (dt.ceo.salaries)
ncol (dt.ceo.salaries)
nrow (dt.ceo.salaries)
head (dt.ceo.salaries)
tail (dt.ceo.salaries)
View (dt.ceo.salaries)
dt.ceo.salaries [1, ]
dt.ceo.salaries [ , salary]
dt.ceo.salaries [1:10, list(salary,age)]
dt.ceo.salaries [order(age)]
dt.ceo.salaries [age<=45 & grad==1,]
dt.ceo.salaries [, log_salary:=log(salary)]
dt.ceo.salaries [, age_squared:=(age)^2]

install.packages("stargazer")
library(stargazer)
