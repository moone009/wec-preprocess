
parallelApply <- function(df,cols,func,parameters){
    
  source('F:/R_Project_wec-preprocess/R/Kfold.r')
  require(doSNOW)
  require(foreach)
  require(data.table)
  registerDoSNOW(makeCluster(5, type = "SOCK"))
  
  ## Create ID to compute in parallel
  df <- kfold(df,5)
  
  if(parameters == 1){
    
    system.time(NewDataFrame <- foreach(i = 1:5) %dopar% {
      print(i)
      apply(subset(df, folds == i)[cols],1,func)
      
    })
  }else{
    
    system.time(NewDataFrame <- foreach(i = 1:5) %dopar% {
      tmpdata <- subset(df, folds == i)
      mapply(func,tmpdata[,cols[1]],tmpdata[,cols[2]])
      
    })
    
  }
  
  NewDataFrame <- rbindlist(Map(as.data.frame, NewDataFrame))
  colnames(NewDataFrame) <- c('ParRow')
  df <- cbind(df,NewDataFrame)
  return(df)

}




























