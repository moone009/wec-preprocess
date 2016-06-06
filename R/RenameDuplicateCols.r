###############################################################################################################################
# Name             :  RenameDuplicateCols 
# Date             :  2016-06-03 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed to rename duplicate columns to prevent sqldf errors 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160603               initial
############################################################################################################################### 

RenameDuplicateCols <- function(df){
  
  li <- which(colnames(df) %in% names(which(table(colnames(df))> 1)))
  while(length(li)>1){
    i = 1
    update.list <- which(colnames(df) %in% colnames(df)[li[i]])
    
    len <- length(update.list)
    
    # Remove records that are being renamed
    li <- li[-which(li %in% update.list)]
    colnames(df)[update.list] <- paste(colnames(df)[update.list],'_',1:len,sep='')
    # break operation once li is 0
    if(length(li) ==0){break}
  }
  
  return(df)
}
