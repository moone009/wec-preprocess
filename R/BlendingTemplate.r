






library(caret)
data(GermanCredit)

trainIndex <- createDataPartition(GermanCredit$credit_risk, p = .6,
                                  list = FALSE,
                                  times = 1)
training <- GermanCredit[ trainIndex,]
testing  <- GermanCredit[-trainIndex,]
colnames(training)[21] = 'Class'
colnames(testing)[21] = 'Class'

## Classification

models <- c('C5.0','multinom','rf','gbm')
trees <- vector(mode = "list", length =5)
for(i in 1:length(models)){
  print(models[i])
  mod <- train(Class ~ ., data = training, method = models[i])
  trees[i] <-  list(mod)

}
modellist <- data.frame(ID = 1:600)
for(i in 1:length(models)){
modellist <- cbind(modellist,as.data.frame(predict(trees[i],training)))
colnames(modellist)[1+i] <- models[i]
}

df1 <- data.frame(id = predict(trees[1],testing))
df2 <- data.frame(id = predict(trees[2],testing))
df3 <- data.frame(id = predict(trees[5],testing))
df4 <- data.frame(id = predict(trees[4],testing))

df = cbind(df1,df2,df3,df4)

colnames(df) <- c('one','two','three','four')

df$Final <- apply(df[,1:4],1, mode) 
table(df$Final)
df$Final[which(df$Final == 'tie')] <- 'good'
df$Final <- factor(df$Final ,levels = c("good", "bad"))
testing$Class <- factor(testing$Class ,levels = c("good", "bad"))

results = table(testing$Class,df$Final)
results = as.matrix(results)

ClassificationPerf(results,T)




milwaukee water works
2041346750