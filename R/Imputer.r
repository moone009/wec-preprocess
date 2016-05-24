actual <- Boston[,1][c(1,6,8,12,66,77,88,101,303)]
Boston[,1][c(1,6,8,12,66,77,88,101,303)] <- NA

impute <-function(df,col,mean,median,linear){
  
  holder <-
  df[which(is.na(df[,1])==T),][1]
  
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

df <- impute(Boston,1,F,F,T)
pred <- df[,1][c(1,6,8,12,66,77,88,101,303)]

MSE(actual,pred)
MAE(actual,pred)
RMSE(actual,pred)

df <- impute(Boston,1,F,T,F)
pred <- df[,1][c(1,6,8,12,66,77,88,101,303)]

MSE(actual,pred)
MAE(actual,pred)
RMSE(actual,pred)

df <- impute(Boston,1,T,F,F)
pred <- df[,1][c(1,6,8,12,66,77,88,101,303)]

MSE(actual,pred)
MAE(actual,pred)
RMSE(actual,pred)




pred <-c(pred,59)
actual <-c(actual,5)

http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_cont_var/uos3/uos3_ko1.htm

MSE(actual,pred)
MAE(actual,pred)
RMSE(actual,pred)










