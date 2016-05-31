
impute <-function(df,col,mean,median,linear){
  
  if(class(df[,col]) %in% c('factor','character')){
  stop('Value is not numeric or integer, stopping operation')
  
  }
  
  test <- df[complete.cases(df),]
  testCases <- test[sample(nrow(test), nrow(test)*.1), ]
  trainCases <- test[-c(as.numeric(rownames(testCases))),]
  
  MeanVal <- mean(trainCases[,col])
  MedianVal <- median(trainCases[,col])
  form <- paste(colnames(df)[1],'~.')
  mod <- lm(as.formula(form),data=trainCases)
  Predictions <- predict(mod,testCases)
  
  blended <- (Predictions*.33+(MeanVal*.33)+(MedianVal*.33))
  
  MeanVal <- data.frame(Method = 'Mean',MSE = MSE(testCases[,1],MeanVal), MASE = MAE(testCases[,1],MeanVal),RMSE = RMSE(testCases[,1],MeanVal))
  MedianVal <- data.frame(Method = 'Median',MSE = MSE(testCases[,1],MedianVal), MASE = MAE(testCases[,1],MedianVal),RMSE = RMSE(testCases[,1],MedianVal))
  LinearModel <- data.frame(Method = 'LinearModel',MSE = MSE(testCases[,1],Predictions), MASE = MAE(testCases[,1],Predictions),RMSE = RMSE(testCases[,1],Predictions))
  BlendedModel <- data.frame(Method = 'BlendedModel',MSE = MSE(testCases[,1],blended), MASE = MAE(testCases[,1],blended),RMSE = RMSE(testCases[,1],blended))
  
  output <- rbind(MeanVal,MedianVal,LinearModel,BlendedModel)
  bestOption <- as.character(output[which(output$RMSE ==  min(output$RMSE)),1])
  print(paste('Based upon using a sample of 10% of the complete cases the imputer ran all three methods and believes',bestOption, 'is the best method'))
  print(output)
  
  if(mean==T){
    print('imputing mean')
    df[which(is.na(df[,1])==T),][1] <-  mean(df[which(is.na(df[,1])==F),][,1])
  } else if(median==T){
    print('imputing median')
    df[which(is.na(df[,1])==T),][1] <-  median(df[which(is.na(df[,1])==F),][,1])
  } else{
    print('imputing regression')
     form <- paste(colnames(df)[1],'~.')
     mod <- lm(as.formula(form),data=df[which(is.na(df[,1])==F),])
     df[which(is.na(df[,1])==T),][1] <- predict(mod,df[which(is.na(df[,1])==T),][-1])
  }
  return(df)
  
}











