
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

