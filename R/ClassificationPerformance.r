

mod = randomForest(Species~.,iris[c(1:5,45:60,90:120),])
results = predict(mod,iris)
Analyze = data.frame(Actual = as.character(iris$Species),Pred = as.character(results))
results = table(iris$Species,results)
results = as.matrix(results)

ClassificationPerf(results)

mod = randomForest(as.factor(cyl)~.,mtcars)
results = predict(mod,mtcars)
results = table(mtcars$cyl,results)
results = as.matrix(results)
 
ClassificationPerf(results)

mod = randomForest(as.factor(rad)~.,Boston[ sample(1:506, 200, replace=T),])
results = predict(mod,Boston)
results = table(Boston$rad,results)
results = as.matrix(results)

ClassificationPerf(results)

results = table(results,Boston$rad)
results = as.matrix(results)





ClassificationPerf <- function(results){
print("Please make sure you inputed Actual by Predictions: table(Iris$Species,Predicted$Species)")
  
classes <- length(diag(results))
performance <- data.frame()
  for(i in 1:classes){
    

  # True Positive: Number of correctly classified targets
  # True Negative: Number of other correctly identified targets
  # False Positives: Other targets being misclassifed as another target. (Watermelons and apples being mistaken for oranges)
  # False Negatives: Target classified as belonging to another class (Target = Orange(s) mistaken as an apple and a watermelon)
  
  TP <- diag(results)[i]
  TN <- sum(diag(results)[-i]) 
  name <- names(diag(results)[i])
    
  FP <- sum(results[,i][-c(i)])
  FN <- sum(results[i,-c(i)])
  
  # Specificity: Proporition of correctly identified targets. (I have 100 bananas, I correctly classify 90, but then 5 oranges and 8 watermelon are classied as bananas then my specificity = 90/(90+5+8) = .87)
  # Sensitivity: Percentage of correctly identified targets (I have 100 bananas, I correctly classify 90 and 10 others are classified as oranges then my Sensitivity = 90/100 = .9)
  # Precision: When it predicts yes, how often is it correct?
  
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
return(performance)
}










evaluate_model <- function(observed, predicted) {
  confusion <- table(observed, predicted)
  num_classes <- nlevels(observed)
  tp <- rep(0, num_classes)
  fn <- rep(0, num_classes)
  fp <- rep(0, num_classes)
  tn <- rep(0, num_classes)
  accuracy <- rep(0, num_classes)
  precision <- rep(0, num_classes)
  recall <- rep(0, num_classes)
  for(i in 1:num_classes) {
    tp[i] <- sum(confusion[i, i])
    fn[i] <- sum(confusion[-i, i])
    fp[i] <- sum(confusion[i, -i])
    tn[i] <- sum(confusion[-i, -i])
    accuracy[i] <- (tp[i] + tn[i]) / (tp[i] + fn[i] + fp[i] + tn[i])
    precision[i] <- tp[i] / (tp[i] + fp[i])
    recall[i] <- tp[i] / (tp[i] + fn[i])
  }
  overall_accuracy <- sum(tp) / sum(confusion)
  average_accuracy <- sum(accuracy) / num_classes
  micro_precision <- sum(tp) / (sum(tp) + sum(fp))
  macro_precision <- sum(precision) / num_classes
  micro_recall <- sum(tp) / (sum(tp) + sum(fn))
  macro_recall <- sum(recall) / num_classes
  metrics <- c("Overall accuracy" = overall_accuracy,
               "Average accuracy" = average_accuracy,
               "Micro-averaged Precision" = micro_precision,
               "Macro-averaged Precision" = macro_precision,
               "Micro-averaged Recall" = micro_recall,
               "Macro-averaged Recall" = macro_recall)
  return(metrics)
}


observed = c(0,1,1,0,0,1,0,1)
predicted = c(0,1,1,0,1,1,1,1)