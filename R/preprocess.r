preprocess <- function(df,target,columns,dimension.reduction=F,Missing=F){
  
  if(class(target)!= 'character'){
    print('Please refer to target variable by name; not the index')
    break
  }
  
  # Required for missing value imputation
  require(missForest)
  require(caret)
  require(moments)
  
  # Hold to bind after preprocssing
  response <- df[,target]
  df[,target] <- NULL
  
  ##______________________________________________________________________________________________________
  #Impute Missing Values
  if(Missing ==T){
    df <- missForest(df[,columns],ntree=250)
    df <- df$ximp
    
    cat("\n")
    cat("\n")
  }
  
  
  ##______________________________________________________________________________________________________
  #Check for duplicate columns  
  skip <- c()
  names <- colnames(df)
  holder <- data.frame()
  for(i in 1:length(names)){
    
    check <- names[-c(i)]
    print(i)
    
    if(i %in% skip){print('already found this variable')
    }else{
      for(x in 1:length(check)){  
        
        if(identical(df[,names[i] ], df[,check[x]]) == TRUE){
          
          results <- data.frame(dupOne = names[i],dupTwo = check[x])
          
          holder <-rbind(holder,results)
          
          # We can remove our duplicate from our original list because other we will catch it twice.
          skip <- c(skip,which(names == check[x]))
        }
      }
    }
  }
  
  ##______________________________________________________________________________________________________
  ## Static Columns
  nzv <- nearZeroVar(df[,columns])
  if(length(nzv) > 0){
    
    print('Static columns will be removed')
    print(paste('Old Dimensions:',paste(dim(df),collapse=',')))
    df <- df[, -nzv]
    print(paste('New Dimensions:',paste(dim(df),collapse=',')))
    cat("\n")
    cat("\n")
  }
  
  ##______________________________________________________________________________________________________
  # Correlated Variables
  numeric.vars <- names(which(sapply(df,class) == 'integer'))
  numeric.vars <- c(numeric.vars,names(which(sapply(df,class) == 'numeric')))
  ## which(abs(descrCor[1,]) > .85 & abs(descrCor[1,]) != 1)
  descrCor <- cor(df[,numeric.vars])
  highlyCorDescr <- findCorrelation(descrCor, cutoff = .95)
  if(length(highlyCorDescr) > 0){
    print(paste('following variable is highly correlated and will be removed:',colnames(df)[highlyCorDescr],sep=''))
    cat("\n")
    cat("\n")
    df <- df[,-highlyCorDescr]
  }
  
  ##______________________________________________________________________________________________________
  # check for numerics that might be factors or factors that might be numeric
  check_for_factors <- function(x){length(table(x))}
  class <- sapply(df,class)
  distinct <-apply(df,2,check_for_factors)
  print('Distinct number of records by variable')
  print(as.data.frame(rbind(class,distinct)))
  cat("\n")
  cat("\n")
  
  ##______________________________________________________________________________________________________
  # skewed Variables
  numeric.vars <- names(which(sapply(df,class) == 'integer'))
  numeric.vars <- c(numeric.vars,names(which(sapply(df,class) == 'numeric')))
  
  scale.vars <- which(apply(df[,numeric.vars],2,kurtosis) > 5)
  check_these_vars <- which(apply(df[,numeric.vars],2,kurtosis) < 1) 
  if(length(check_these_vars)>0){
    print(paste('Check these variables because of low kurtosis:',paste(names(check_these_vars),collapse=', ')))
    cat("\n")
    cat("\n")
  }
  
  ##______________________________________________________________________________________________________
  # Dimension Reduction
  if(dimension.reduction==T){
    preProcValues <- preProcess(df[,numeric.vars], method = c("BoxCox", "center","scale", "pca"))
    dfd <- predict(preProcValues, df[,numeric.vars])
    df <- cbind(df[,-c(which(colnames(df) %in% numeric.vars))],dfd)
  }
  df <-cbind(df,response)
  
  return(df)
}

