###############################################################################################################################
# Name             :  css_date 
# Date             :  2016-06-03 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed to manipulate dates from css 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160603               initial
############################################################################################################################### 

css_date <- function(x,date){
  require(stringr)
  mons <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  
  if(date == T){
    as.Date(substr(x,1,11),format="%b %d %Y")
  }else{
    if(substr(x,25,26)=="PM"){add = 12}else{add = 0}
    mon  <- str_pad(which(substr(x,1,3)==mons), 2, pad = "0")
    year <- substr(x,8,11)
    day  <- str_pad(substr(x,5,6), 2, pad = "0")
    hour  <- as.numeric(str_pad(substr(x,13,14), 2, pad = "0"))+add
    min  <- str_pad(substr(x,16,17), 2, pad = "0")
    sec  <- str_pad(substr(x,19,20), 2, pad = "0")
    ms  <- str_pad(substr(x,22,24), 3, pad = "0")
    
    return(paste(year,mon,day,hour,min,sec,ms,sep=''))
  }
  
}



x = 'Jul 24 2015 09:24:23:403AM'
x = 'Jul 24 2015 01:14:55:803PM'

css_date(x,T)
css_date(x,F)




md <- mtcars[,c(1,2,3,4,5,6)]
mod <- lm(mpg~.,md)

n = 32
p = 4
RSS = 176.62
RSSP = RSS * p

AIC <- n*(log(RSS/n))+(2*p)
6 + 32 * log(RSS/n)


AIC(lm(mpg ~ cyl + hp + wt,md))











