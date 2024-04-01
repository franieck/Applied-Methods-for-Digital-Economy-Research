load("C:\\Users\\lucas\\Documents\\NOVA SBE\\PhD\\Methodology\\Applied Methods for Digital Economy Research\\Prep 1 Lab Basics\\ceosal2.RData")
dt.ceo.salaries <- data.table(data)
library(data.table)
library(ggplot2)
library(stargazer)
rm(data)
nrow(dt.ceo.salaries)
dt.ceo.salaries[,sum(grad)]
nrow(dt.ceo.salaries[grad==1,]) 

dt.ceo.salaries[,mean(grad)]  
dt.ceo.salaries[,mean(salary)]
dt.ceo.salaries[grad==1, mean(salary)] # mean CEO salary among those with graduate degree
dt.ceo.salaries[grad==0, mean(salary)] # mean CEO salary among those without graduate degree

nrow(dt.ceo.salaries[college==0,]) # number of CEOs without college degree
dt.ceo.salaries[,list(n_ceo=.N), by = college] # number of CEOs without college degree
t.test(dt.ceo.salaries[,salary], mu = 800) # test whether mean salary is different than 800

t.test(dt.ceo.salaries[ , salary] ~dt.ceo.salaries[,grad]) # test whether mean salary is different between those with grad and those wihout grad degree.
dt.ceo.salaries[,t.test (salary~grad)] # test whether mean salary is different between those with grad and those wihout grad degree.

dt.ceo.salaries[,list( mean_salary=mean(salary)
                       , sd_salary=sd(salary)
                       , min_salary=min(salary) 
                       , max_salary=max(salary)),by=list(grad,college)] # computing summary statistics for different groups
                       

                
stargazer(dt.ceo.salaries, type="text")               
stargazer(dt.ceo.salaries[grad==1, list(age,salary)], type="text") #look at descriptive statistics of age and salary among those with a grad degree                    

qplot( data=dt.ceo.salaries
       ,x=salary
       ,geom="histogram")          
                      
qplot( data=dt.ceo.salaries
       ,x=age
       ,geom="histogram")   #histogram                     

qplot( data=dt.ceo.salaries
       ,x=sales
       ,y=profits
       ,geom="point") #scatterplot

qplot( data=dt.ceo.salaries
       ,x=factor(grad)
       ,geom="bar") #bar graph
      
qplot( data=dt.ceo.salaries
       ,x=sales
       ,y=profits
       ,geom="line") #linegraph

qplot( data=dt.ceo.salaries
       ,x=salary
       ,geom="histogram")+facet_wrap(~ grad) #bugging out



