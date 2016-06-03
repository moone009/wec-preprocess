###############################################################################################################################
# Name             :  Metrics 
# Date             :  2016-06-03 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed to output regression metrics 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160603               initial
############################################################################################################################### 

RMSE <- function(y,y_pred){sqrt(mean((y-y_pred)^2))}
MSE <- function(y,y_pred){mean((y-y_pred)^2)}
MAE <- function(y,y_pred){mean(y-y_pred)}
