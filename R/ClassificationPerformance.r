###############################################################################################################################
# Name             :  ClassificationPerformance 
# Date             :  2016-06-03 
# Author           :  Christopher Mooney 
# Dept             :  Business Analytics 
# Purpose          :  function designed to compute classification performance methods 
###############################################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0   w47593        20160603               initial
############################################################################################################################### 


ClassificationPerf <- function(results,explanation){
  print("Please make sure you inputed Actual by Predictions: table(Iris$Species,Predicted$Species) \n Also, please make sure factor levels are the same")
  
classes <- length(diag(results))
performance <- data.frame()
  for(i in 1:classes){
    

  NotesOne <-  paste('
  # True Positive: Number of correctly classified targets 
  # True Negative: Number of other correctly identified targets 
  # False Positives: Other targets being misclassifed as another target. (Watermelons and apples being mistaken for oranges) 
  # False Negatives: Target classified as belonging to another class (Target = Orange(s) mistaken as an apple and a watermelon) ')
  
  TP <- diag(results)[i]
  TN <- sum(diag(results)[-i]) 
  name <- names(diag(results)[i])
    
  FP <- sum(results[,i][-c(i)])
  FN <- sum(results[i,-c(i)])
  NotesTwo <-  paste(' 
  # Specificity: Proporition of correctly identified targets. (I have 100 bananas, I correctly classify 90, but then 5 oranges and 8 watermelon 
  are classied as bananas then my specificity = 90/(90+5+8) = .87) 
  # Sensitivity: Percentage of correctly identified targets (I have 100 bananas, I correctly classify 90 and 10 others are classified 
  as oranges then my Sensitivity = 90/100 = .9) 
  # Precision: When it predicts yes, how often is it correct? \n\n'
  )
  
  Precision=TP / (TP + FP)
  Sensitivity = TP / (TP + FN)
  Specificity = TN / (FP + TN)
  Fscore = 2*TP /(2*TP + FP + FN)

  performance = rbind(performance,data.frame(Class = name,
                                             TP = TP,
                                             TN = TN,
                                             FP = FP,
                                             FN=FN,
                                             Precision = Precision,
                                             Sensitivity = Sensitivity,
                                             Specificity = Specificity,
                                             Fscore = Fscore ))
  
  
  }
rownames(performance) <- NULL
print(paste('Model Accuracy: ',round( sum(diag(results))/sum(results),4),sep=''))
if(explanation == T){
  cat(NotesOne)
  cat(NotesTwo)
}
return(performance)

}

library(randomForest)
library(MASS)


mod = randomForest(Species~.,iris[c(1:5,45:60,90:120),])
results = predict(mod,iris)
Analyze = data.frame(Actual = as.character(iris$Species),Pred = as.character(results))
results = table(iris$Species,results)
results = as.matrix(results)

ClassificationPerf(results,T)

mod = randomForest(as.factor(cyl)~.,mtcars)
results = predict(mod,mtcars)
results = table(mtcars$cyl,results)
results = as.matrix(results)

ClassificationPerf(results,F)

mod = randomForest(as.factor(rad)~.,Boston[ sample(1:506, 200, replace=T),])
results = predict(mod,Boston)
results = table(Boston$rad,results)
results = as.matrix(results)

ClassificationPerf(results,F)

results = table(results,Boston$rad)
results = as.matrix(results)



ClassificationPerf(results,T)