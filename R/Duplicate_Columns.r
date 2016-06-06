###############################################################################################################################
# Name             :  Duplicate Columns 
# Date             :  2016-06-06 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed to remove idenctical columns 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160606               initial
############################################################################################################################### 


duplicate_names <- function(x){
  
  cols <- which(table(colnames(x))>1)
  removal <- c()
  for(i in 1:length(cols)){
    idx <-  which(colnames(x) %in% names(cols[i]))
    if(unique(x[,idx[1]]==x[,idx[2]])==TRUE){
    removal <- c(removal,max(idx))
    }
  }
  x <- x[,-c(removal)]
  return(x)
}


