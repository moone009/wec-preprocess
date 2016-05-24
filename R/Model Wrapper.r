#########################################################################################################
# Name             : Model Wrapper
# Date             : 05-10-2016
# Author           : Christopher M
# Dept             : BEI
# Purpose          : Transform Data for AMI rollout
# Called by        : Not in production
#########################################################################################################
# ver    user        date(YYYYMMDD)        change  
# 1.0    w47593      20160510             initial
#########################################################################################################

## Load Libaries & Functions
Start <- Sys.time()
library(RODBC)

##______________________________________________________________________________________________________
## Model Description

ModelName <- 'BEI_tree_ensemble'
ModelType <- 'Prediction'
ModelLanguage <- 'R'
ModelOwner <- 'w47593'
ModelDept <- 'BEI'
ModelDescription <- 'Tree model to predict fraud probabilities'

##______________________________________________________________________________________________________
## Load Data (this should be a function that pulls all data)

load_data <- function(){
  df = rbind(mtcars,mtcars)
  for(i in 1:1000000){i + 1}
  return(df)
}

loadtime <- system.time(data <- load_data())
rc <- nrow(data)

##______________________________________________________________________________________________________
## Call Model

load("linearModel.rda")
Modeltime <- system.time(Predictions <- predict(linearModel,test))


##______________________________________________________________________________________________________
## Load Results to logging database

End <- Sys.time() - Start

input <- data.frame(
           ModelType = ModelType,
           ModelName = ModelName,
           ModelLanguage = ModelLanguage,
           ModelOwner = ModelOwner,
           ModelDept = ModelDept,
           ModelDescription = ModelDescription,
           TotalTime = End,
           DataAcquistionUser = loadtime[1],
           DataAcquistionSystem = loadtime[2],
           DataAcquistionElapsed = loadtime[3],
           ModelingUser = Modeltime[1],
           ModelingSystem = Modeltime[2],
           ModelingElapsed = Modeltime[3],
           RecordsProcessed = rc,
           PredictionsMean =  mean(Predictions),
           PredictionsMedian =  median(Predictions),
           PredictionsSD =  sd(Predictions),
           ClassificationResults = '')

row.names(input) <- 1:nrow(input)



train_ind <- sample(1:nrow(mtcars), size = floor(.6 * nrow(mtcars)))
train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]
linearModel <- lm(mpg~.,train)
save(linearModel, file = "linearModel.rda")
predict(linearModel,test)
load("linearModel.rda")
cbind(test,data.frame(Prediction = predict(linearModel,test)))

https://www.datacamp.com/community/tutorials/the-importance-of-preprocessing-in-data-science-and-the-machine-learning-pipeline-ii-centering-scaling-and-logistic-regression

select work_type,to_char(IN_DATE, 'MM-YYYY'),count(*) from work_unit_archive
INNER JOIN WORK_TYPE on work_unit_archive.WT_ID = WORK_TYPE.WT_ID
where Work_Type like 'High Cons NoBill%'
group by work_type,to_char(IN_DATE, 'MM-YYYY')
order by work_type,to_char(IN_DATE, 'MM-YYYY')


select * from work_unit_archive
INNER JOIN WORK_TYPE on work_unit_archive.WT_ID = WORK_TYPE.WT_ID
where Work_Type like 'High Cons NoBill%'
group by work_type







