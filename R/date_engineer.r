###############################################################################################################################
# Name             :  date_engineer 
# Date             :  2016-06-03 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed to feature engineer dates 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160603               initial
############################################################################################################################### 

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
      if(Y <= 0 & Y >= -1){1}else{0}
    }
    thanksgiving <- c('23-Nov-2000','22-Nov-2001','28-Nov-2002','27-Nov-2003','25-Nov-2004','24-Nov-2005','23-Nov-2006',
                     '22-Nov-2007','27-Nov-2008','26-Nov-2009','25-Nov-2010','24-Nov-2011','22-Nov-2012','28-Nov-2013',
                     '27-Nov-2014','26-Nov-2015','24-Nov-2016','23-Nov-2017','22-Nov-2018','28-Nov-2019','26-Nov-2020')
    
    data[paste(col,'_Christmas',sep='')] =  sapply(data[[col]],Holiday,month=12,day=25)  
    data[paste(col,'_ChristmasEve',sep='')] =  sapply(data[[col]],Holiday,month=12,day=24)       
    data[paste(col,'_Independence',sep='')] =  sapply(data[[col]],Holiday,month=07,day=04)  
    
    #data[paste(col,'_Easter',sep='')] =  sapply(data[[col]],Holiday,month=05,day=04)       
    #data[paste(col,'_Memorial',sep='')] =  sapply(data[[col]],Holiday,month=07,day=04)       
    #data[paste(col,'_NYE',sep='')] =  sapply(data[[col]],Holiday,month=12,day=31{})       
    #data[paste(col,'_ThanksGiving',sep='')] =  sapply(data[[col]],Holiday,month=07,day=04)       
  }
  return(data)
}
