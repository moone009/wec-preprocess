

##_____________________________________________________________________________________________________________________________
# setup test data
mtcars$carb = as.factor(mtcars$carb)
mtcars$am = as.character(mtcars$am)

##_____________________________________________________________________________________________________________________________
# Execute function
df = DummyCode(mtcars,c('carb','am'))



library(lubridate)
##_____________________________________________________________________________________________________________________________
# Sample Data

data=as.data.frame(list(ID=1:55,
                        variable=rnorm(55,50,15)))

#This function will generate a uniform sample of dates from 
#within a designated start and end date:
rand.date=function(start.day,end.day,data){   
  size=dim(data)[1]    
  days=seq.Date(as.Date(start.day),as.Date(end.day),by="day")  
  pick.day=runif(size,1,length(days))  
  date=days[pick.day]  
}

#This will create a new column within your data frame called date:
data$date=rand.date("2013-01-01","2014-02-28",data)

##_____________________________________________________________________________________________________________________________
# Sample Data
data <- date_engineer(data,'date',F)






data <- cbind(prodNA(iris[c(1,2,3,4)], noNA = 0.1),iris[,c(5)])
colnames(data)[5] <- 'Species'
target = 'Species'
columns = c(1:4)
df <- preprocess(data,target,columns,F)










