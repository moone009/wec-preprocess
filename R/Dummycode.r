###############################################################################################################################
# Name             :  DummyCode 
# Date             :  2016-06-03 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed to dummy code non numeric features 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160603               initial
############################################################################################################################### 

DummyCode <- function (df,DummyColumns,dropcols) {
  
  # Create a copy of original dataset
  FileToDummyCode = df
  # greate a unique id for this dataset to group on
  FileToDummyCode$ByVarID = 1:nrow(FileToDummyCode)
  
  for( i in 1:length(DummyColumns)){
    
    print(DummyColumns[i])
    
    # Switch data to character
    Table=as.data.frame.matrix(table(FileToDummyCode$ByVarID,FileToDummyCode[,DummyColumns[i]]))
    
    # rename our columns
    colnames(Table) = paste(DummyColumns[i],'.',colnames(Table),sep='')
    
    # bind data back to original dataset
    df = cbind(df,Table)                            
    
  }
  if(dropcols == T){
  return(df[,-c(which(colnames(df) %in% DummyColumns))] )
  }else{  return(df)}
  
}

