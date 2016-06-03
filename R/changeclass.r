###############################################################################################################################
# Name             :  ChangeClass 
# Date             :  2016-06-03 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed to best estimate appropriae column classes 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160603               initial
############################################################################################################################### 

changeclass <- function(df){
  
  for(z in 1:length(colnames(df))){
    if(nrow(df)< 100){print('please change column types manually; record count < 100, stopping operation')
                      break 
    }
    len <- length(table(df[colnames(df)[z]]))
    total <- length(grep('[A-z]',df[[colnames(df)[z]]]))
    
    if(len > 32 & total == 0 & class(df[[colnames(df)[z]]]) %in% c('character','factor')){
      print(paste('changing ',colnames(df)[z], ':',class(df[[colnames(df)[z]]]),' to numeric',sep=''))
      df[,colnames(df)[z]] <- as.numeric(as.character(df[,colnames(df)[z]]))
    }
    if(len > 32 & total > 1 & class(df[[colnames(df)[z]]]) != 'character'){
      print(paste('changing ',colnames(df)[z], ':',class(df[[colnames(df)[z]]]),' to character',sep=''))
      df[,colnames(df)[z]] <- as.character(df[[colnames(df)[z]]])
    }
    if(len <= 32 & class(df[[colnames(df)[z]]]) != 'factor'){
      print(paste('changing ',colnames(df)[z], ':',class(df[[colnames(df)[z]]]),' to factor',sep=''))
      df[,colnames(df)[z]] <- as.factor(df[,colnames(df)[z]])
    }
  }
  return(df)
}
