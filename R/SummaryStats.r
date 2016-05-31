df_stats <-function(df){
   
  source("./R/changeclass.r")
  require(data.table)
  require(reshape2)
  require(dplyr)
  
  df <- changeclass(df)
  numeric.vars <- names(which(sapply(df,class) == 'integer'))
  numeric.vars <- c(numeric.vars,names(which(sapply(df,class) == 'numeric')))
  df <- df[,numeric.vars]
  
  df$id =1:nrow(df)
  dff  = as.data.table(melt(df,id=c("id")))
  Stats =   dff %>%  
    na.omit() %>%
    group_by(variable)%>% 
    summarise(Mean=mean(value),Min=min(value),Max=max(value), Median=median(value), Std=sd(value),Sum = sum(value),Rowcount = n())
  
  df$id <- NULL
  collection = data.frame()
  for(i in 1:length(df)){
    
    col <- paste(colnames(df)[i],"_Z_Score",sep='')
    df[col]<- (df[i]-mean(df[,i]))/sd(df[,i])
    Z_Scores <- length(which(df[col]  < -5 ) ) +  length(which(df[col]  > 5) )
    df[col]<- NULL
    data <- data.frame(Col = colnames(df)[i],Zscore = Z_Scores)
    collection <- rbind(collection,data)
  }
  Missingcollection = data.frame()
  for(i in 1:length(df)){
    
    data <- data.frame(Col = colnames(df)[i],Missing =sum(is.na(df[,c(i)])))
    
    Missingcollection <- rbind(Missingcollection,data)
  }
  Uniquecollection = data.frame()
  for(i in 1:length(df)){
    
    data <- data.frame(Col = colnames(df)[i],
                       UniqueValues =length(table(df[,c(i)])),
                       FiveRepresent = round(sum(rev(sort(table(df[,c(i)])))[1:5])/nrow(df),2))
    
    Uniquecollection <- rbind(Uniquecollection,data)
  }  
  
  Uniquecollection$Col <- NULL
  Missingcollection$Col <- NULL
  collection$Col <- NULL
  
  data <- as.data.frame(cbind(Stats,collection,Missingcollection,Uniquecollection))
  data[,c(2:12)] <-  round(data[,c(2:12)],2)
  return(data)
  
}
