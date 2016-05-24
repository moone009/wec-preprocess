

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

##_____________________________________________________________________________________________________________________________
# Kfold
mtcars$folds = 5
mtcars <- kfold(mtcars,3)
rm(mtcars)
mtcars <- kfold(mtcars,3)

##_____________________________________________________________________________________________________________________________
# Static Variables

mtcars$Id <- 1
mtcars$Idd <- 1
mtcars <- Static_Missing_Vars(mtcars)

##_____________________________________________________________________________________________________________________________
# parallel process
p.func <- function(x){
  if(x > 2){"big"
  }else if(x == 1){"Thats random"
  }else{"Hello"}
}

m.func <- function(x,y){
  if(x > 20 & y > 200){"big"
  }else if(x < 20 & y > 188){"Thats random"
  }else{"Hello"}
}

df <- data.frame(id = rnorm(10000))
df <- parallelApply(df,1,p.func,1)
table(df$ParRow)

df <- data.frame(id = rnorm(10000,mean = 18,sd = 10),x =rnorm(10000), y = rnorm(10000,mean = 200,sd = 60))
df <- parallelApply(df,c(1,3),m.func,2)
table(df$ParRow)



##_____________________________________________________________________________________________________________________________
# pre process
data <- preprocess(mtcars,'vs',c(1:7),T,F)
head(data)
data <- preprocess(mtcars,'vs',c(1:7),F,F)
head(data)

data <- df_stats(mtcars)
data 
data <- df_stats(iris)
data

##_____________________________________________________________________________________________________________________________
# changeclass
df = data.frame(point1 = rnorm(1000,1,0),point2 = rnorm(1000,1,100),point3 = rnorm(1000,1,100),point4 = rnorm(1000,1,100))
df$point1 = as.character(df$point1)
df$point2 = as.numeric(df$point2)
df$point3 = as.factor(df$point3)
df$point4 = as.character(df$point4)

str(df)
head(df)

df <- changeclass(df)

head(df)
str(df)

##_____________________________________________________________________________________________________________________________
# 
data <- cbind(prodNA(iris[c(1,2,3,4)], noNA = 0.1),iris[,c(5)])
colnames(data)[5] <- 'Species'
target = 'Species'
columns = c(1:4)
df <- preprocess(data,target,columns,F)










