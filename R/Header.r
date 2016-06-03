###############################################################################################################################
# Name             :  Header 
# Date             :  2016-06-03 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed to produce headers (very meta!) 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160603               initial
############################################################################################################################### 

header <- function(functionname,Author,Dept,user,Purpose){
  
 dt <- gsub("-","",Sys.Date())
 head =paste("
  ###############################################################################################################################
  # Name             : ",functionname,"
  # Date             : ",Sys.Date(),"
  # Author           : ",Author,"
  # Dept             : ",Dept,"
  # Purpose          : ",Purpose,"
  ###############################################################################################################################
  # ver    user        date(YYYYMMDD)        change  
  # 1.0  ",user,"      ",dt,"              initial
  ############################################################################################################################### 
  ")
  
  
  return(cat(head))
  
}

header("Header","Christopher Mooney","Business Analytics","w47593","function designed to produce headers (very meta!)")

