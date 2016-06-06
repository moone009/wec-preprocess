###############################################################################################################################
# Name             :  ParallelApply 
# Date             :  2016-06-03 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed as a wrapper to parallel apply functions 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160603               initial
############################################################################################################################### 

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




























