###############################################################################################################################
# Name             :  RfByClassImp 
# Date             :  2016-06-07 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed to show importance by class compared to all others 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160607               initial
############################################################################################################################### 


library(ElemStatLearn)
library(gbm)


rfByClassImp <- function(df,target){
  

source("F:\\R_Project_wec-preprocess\\R\\changeclass.r")  
df <-df[complete.cases(df),]
colnames(df)[which(colnames(df)==target)] = 'targetClass'
# Extract predictors
predictors <- names(df)[names(df) != 'targetClass']

classes <- names(table(df$targetClass))
df$targetClass <- as.factor(as.character(df$targetClass))
df <- changeclass(df)

plot_list = list()
  for(i in 1:length(classes)){
    dff <- subset(df,targetClass ==classes[i])
    dff1 <- subset(df,targetClass !=classes[i])
    dff1$targetClass<- 'Not'
    dff <- rbind(dff,dff1)
    dff$targetClass <- as.factor(as.character(dff$targetClass))
    
    # Train a model across all the training data and plot the variable importance
    model_rf <- randomForest(dff[,predictors], dff$targetClass, ntree=500, importance=TRUE)
    
    # Export Variable importance
    dat = importance(model_rf, type=2)
    
    # Convert to frame
    featureImportance <- data.frame(dat)
    featureImportance$Var = rownames(featureImportance)
    rownames(featureImportance) <- 1:nrow(featureImportance)
    featureImportance$Var = as.character(featureImportance$Var)
    
    featureImportance = sqldf("select * from featureImportance order by MeanDecreaseGini desc limit 20")
    plt <- ggplot(featureImportance, aes(x=reorder(Var, order(MeanDecreaseGini, decreasing = F)), y=MeanDecreaseGini)) +
      geom_bar(stat="identity", fill="#E8AFAF") +
      coord_flip() + 
      theme_light(base_size=20) +
      xlab("Variable") +
      ylab("Importance") + 
      ggtitle(paste(classes[i],":Feature Importance",sep='')) +
      theme(plot.title=element_text(size=18))
    plot_list[[i]] = plt
  }

## Export
if(length(classes==3)){
grid.arrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], ncol=3)
} else if(length(classes==4)){
  grid.arrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],list[[4]], ncol=4)
  
}else if(length(classes==5)){
  grid.arrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],list[[4]],plot_list[[5]], ncol=5)
  
}else if(length(classes==6)){
  grid.arrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],list[[4]],plot_list[[5]],plot_list[[6]], ncol=6)
  
}else{print('sorry!')}

}


#rfByClassImp(df,"Dual_Income")

#rfByClassImp(mtcars,"cyl")

#rfByClassImp(diamonds,"cut")

#rfByClassImp(mpg[,-c(2)],"class")

