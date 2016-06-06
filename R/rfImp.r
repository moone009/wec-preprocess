###############################################################################################################################
# Name             :  RandomForest Importance 
# Date             :  2016-06-06 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed to quickly identify meaninful features 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160606               initial
############################################################################################################################### 

rfImp <- function(df,target,returnval){
  
  # Load required libraries
  require(ggplot2)
  require(lubridate)
  require(randomForest)
  require(sqldf)
  
  # Extract predictors
  predictors <- names(df)[names(df) != target]
  
  # Train a model across all the training data and plot the variable importance
  model_rf <- randomForest(df[,predictors], df[,target], ntree=500, importance=TRUE)
  
  # Export Variable importance
  dat = importance(model_rf, type=2)
  
  # Convert to frame
  featureImportance <- data.frame(dat)
  featureImportance$Var = rownames(featureImportance)
  rownames(featureImportance) <- 1:nrow(featureImportance)
  featureImportance$Var = as.character(featureImportance$Var)
  
  ## Var Plot
  featureImportance = sqldf("select * from featureImportance order by MeanDecreaseGini desc limit 20")
  plt = ggplot(featureImportance, aes(x=reorder(Var, order(MeanDecreaseGini, decreasing = F)), y=MeanDecreaseGini)) +
    geom_bar(stat="identity", fill="#E8AFAF") +
    coord_flip() + 
    theme_light(base_size=20) +
    xlab("Variable") +
    ylab("Importance") + 
    ggtitle("Random Forest Feature Importance") +
    theme(plot.title=element_text(size=18))
  
  # Select top twenty variables
  featureImportanceVals = sqldf("select Var, MeanDecreaseGini from featureImportance order by MeanDecreaseGini desc limit 20")
  featureImportance = sqldf("select Var from featureImportance order by MeanDecreaseGini desc limit 20")
  
  RfSelectedVariables = c()
  for(x in 1:nrow(featureImportance))
  {
    RfSelectedVariables = c(RfSelectedVariables , c=featureImportance[[1]][x])    
  }
  
  
  if(returnval == 1){
    print('Limited to top twenty variables')
    return(featureImportanceVals)
  }
  if(returnval == 2){
    print('limited to 20 features')
    return(RfSelectedVariables)
  }
  if(returnval == 3){
    return(plt)
  }
}

#rfImp(iris,'Species',1)
#rfImp(iris,'Species',2)
#rfImp(iris,'Species',3)
