###############################################################################################################################
# Name             :  kfold 
# Date             :  2016-06-03 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed to create kfold groupings 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160603               initial
############################################################################################################################### 

kfold <- function(df,k){
  require(sqldf)
  if( length(which(colnames(df)=='folds'))>0){
    stop(print('Rename column(s) named folds'))
    break
    
  }
  #shuffle the data
  df<-sqldf("SELECT * FROM df order by random()")
  
  #Create 10 equally size folds
  df$folds <- cut(seq(1,nrow(df)),breaks=k,labels=FALSE)
  
  return(df)
  
}

