css_date <- function(x,date){
  require(stringr)
  mons <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  
  if(date == T){
  as.Date(substr(x,1,11),format="%b %d %Y")
  }else{
    mon  <- str_pad(which(substr(x,1,3)==mons), 2, pad = "0")
    year <- substr(x,8,11)
    day  <- str_pad(substr(x,5,6), 2, pad = "0")
    return(paste(year,mon,day,sep=''))
  }
  
}


x = 'Mar 22 2014 12:00:00:000AM'
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











