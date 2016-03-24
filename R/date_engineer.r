


date_engineer <- function(data,col,holiday){
  require(lubridate)
  data[paste(col,'_mon',sep='')] = month(data[[col]])
  data[paste(col,'_year',sep='')] = year(data[[col]])
  data[paste(col,'_day',sep='')] = day(data[[col]])
  data[paste(col,'_quarter',sep='')] = quarter(data[[col]])
  data[paste(col,'_week',sep='')] = week(data[[col]])
  
  # Major Holidays
  # within 7 days of Christmas, Easter, 4th July, Memorial Day, Labor Day, New Years Eve, Thanks Giving
  
  if(holiday == T){
    Holiday <- function(x,month = 12,day = 25){ 
      Y = as.numeric(x - as.Date(paste(year(data[[col]]),'-',month,'-',day,sep='')))   
      if(Y <= 0 & Y >= -7){1}else{0}
    }
    
    data[paste(col,'_Christmas',sep='')] =  sapply(data[[col]],Holiday,month=12,day=25)       
    data[paste(col,'_Independence',sep='')] =  sapply(data[[col]],Holiday,month=07,day=04)       
    #data[paste(col,'_Easter',sep='')] =  sapply(data[[col]],Holiday,month=05,day=04)       
    #data[paste(col,'_Memorial',sep='')] =  sapply(data[[col]],Holiday,month=07,day=04)       
    #data[paste(col,'_NYE',sep='')] =  sapply(data[[col]],Holiday,month=12,day=31{})       
    #data[paste(col,'_ThanksGiving',sep='')] =  sapply(data[[col]],Holiday,month=07,day=04)       
  }
  return(data)
}
