#head(Boston)
#df = Boston
#Target = 'medv'

InitialModeler <- function(df,Classes,Target,LinearModeling){
  require(readxl)
  source('F:\\R_Project_wec-preprocess\\R\\trim.r')
  source('F:\\R_Project_wec-preprocess\\R\\preprocess.r')
  source('F:\\R_Project_wec-preprocess\\R\\Dummycode.r')
  source('F:\\R_Project_wec-preprocess\\R\\changeclass.r')
  source('F:\\R_Project_wec-preprocess\\R\\SummaryStats.r')
  source('F:\\R_Project_wec-preprocess\\R\\Metrics.r')
  
  ## create folder
  dir.create(paste(Sys.Date(),'markdown',sep=''))
  dir <- paste(getwd(),'/',Sys.Date(),'markdown/',sep='')
  returndir <- dir
  ## Change Classes
  if(Classes==T){df <- changeclass(df)}
  
  ## Preprocess data
  PCA <- suppressMessages(preprocess(df,Target,which(colnames(df)!=Target),T,F))
  head(PCA)
  
  NON_PCA <- suppressMessages(preprocess(df,Target,which(colnames(df)!=Target),F,F))
  head(NON_PCA)
  
  ## Summary Stats
  data <- df_stats(df)
   
  ## Validate-Test
  smp_size <- floor(.6 * nrow(NON_PCA))
  train_ind <- sample(seq_len(nrow(NON_PCA)), size = smp_size)
  
  train <- NON_PCA[train_ind, ]
  test <- NON_PCA[-train_ind, ]
  
  smp_size <- floor(.6 * nrow(PCA))
  train_ind <- sample(seq_len(nrow(PCA)), size = smp_size)
  
  PCA_train <- PCA[train_ind, ]
  PCA_test <- PCA[-train_ind, ]
  
  ## Linear Modeling
  if(LinearModeling==T){
    
  NON_PCA_LM <- lm(response~.,train)
  PCA_LM <- lm(response~.,PCA_train)
  
  save(NON_PCA_LM, file = paste(dir,"NON_PCA_LM.rda",sep=''))
  save(PCA_LM,  file = paste(dir,"PCA_LM.rda",sep=''))
  
  ## Save Models
  NON_PCA_LM_Preds <- predict(NON_PCA_LM,test)
  PCA_LM_Pres <-predict(PCA_LM,PCA_test)
  
  NON_PCA_RMSE <- RMSE(test$response,NON_PCA_LM_Preds)
  NON_PCA_MSE <- MSE(test$response,NON_PCA_LM_Preds)
  NON_PCA_MAE <- MAE(test$response,NON_PCA_LM_Preds)
  
  NonPCAMetrics <- data.frame(RMSE = NON_PCA_RMSE,MSE = NON_PCA_MSE,MAE = NON_PCA_MAE)
  
  PCA_RMSE <- RMSE(PCA_test$response,PCA_LM_Pres)
  PCA_MSE <- MSE(PCA_test$response,PCA_LM_Pres)
  PCA_MAE <- MAE(PCA_test$response,PCA_LM_Pres)
  
  PCAMetrics <- data.frame(RMSE = PCA_RMSE,MSE = PCA_MSE,MAE = PCA_MAE)
  
  errors <- data.frame(errors = (test$response-NON_PCA_LM_Preds))
  
  ggplot(errors, aes(x=errors)) + 
    geom_histogram(binwidth=.5,color="blue", fill="salmon")+
    geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept=mean(errors)),
                                                        color="blue", linetype="dashed", size=1)
  ggsave(file="NONPCA.png", width=7, height=8)
  
  PCAerrors <- data.frame(errors = (PCA_test$response-PCA_LM_Pres))
  
  ggplot(PCAerrors, aes(x=errors)) + 
    geom_histogram(binwidth=.5,color="blue", fill="salmon")+
    geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept=mean(errors)),
                                                        color="blue", linetype="dashed", size=1)
  ggsave(file="PCA.png", width=7, height=8)
  
  ## Export Excel File
  l <- list("Summary" = data, "NonPCAErrors" = errors,"PCAErrors" = PCAerrors,'NonPCAMetrics'=NonPCAMetrics,'PCAMetrics'=PCAMetrics)
  FileName <- paste(dir,'Output','.xlsx',sep='')
  openxlsx::write.xlsx(l, file =FileName)
  }
  
  return(returndir)
  
}

 

