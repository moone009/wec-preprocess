---
title: "Examples"
author: "Christopher M"
date: "Tuesday, May 24, 2016"
output: html_document
---

This package was created to provide analysts with functions that are useful relative to the data within the enterpise. 

[changeclass](#changeclass)

[Dummycode](#Dummycode)

[kfold](#kfold)

[imputer](#imputer)

[preprocess](#preprocess)

[SummaryStats](#SummaryStats)

[StaticMissingVars](#StaticMissingVars)

[parallelApply](#parallelApply)

[trim](#trim)



```
## [1] "Please make sure you inputed Actual by Predictions: table(Iris$Species,Predicted$Species)"
## [1] "Model Accuracy: 0.98"
## 
##   # True Positive: Number of correctly classified targets 
##   # True Negative: Number of other correctly identified targets 
##   # False Positives: Other targets being misclassifed as another target. (Watermelons and apples being mistaken for oranges) 
##   # False Negatives: Target classified as belonging to another class (Target = Orange(s) mistaken as an apple and a watermelon)  
##   # Specificity: Proporition of correctly identified targets. (I have 100 bananas, I correctly classify 90, but then 5 oranges and 8 watermelon 
##   are classied as bananas then my specificity = 90/(90+5+8) = .87) 
##   # Sensitivity: Percentage of correctly identified targets (I have 100 bananas, I correctly classify 90 and 10 others are classified 
##   as oranges then my Sensitivity = 90/100 = .9) 
##   # Precision: When it predicts yes, how often is it correct? 
## 
## [1] "Please make sure you inputed Actual by Predictions: table(Iris$Species,Predicted$Species)"
## [1] "Model Accuracy: 1"
```

```
## Error in ClassificationPerf(results): argument "explanation" is missing, with no default
```

#changeclass
### This function was designed to convert variables to their most likely column class.


```r
df = data.frame(point1 = rnorm(1000,1,0),point2 = rnorm(1000,1,100),point3 = rnorm(1000,1,100),point4 = rnorm(1000,1,100))
df$point1 = as.character(df$point1)
df$point2 = as.numeric(df$point2)
df$point3 = as.factor(df$point3)
df$point4 = as.character(df$point4)
str(df)
```

```
## 'data.frame':	1000 obs. of  4 variables:
##  $ point1: chr  "1" "1" "1" "1" ...
##  $ point2: num  23.1 -104.2 -21.9 51.5 163.9 ...
##  $ point3: Factor w/ 1000 levels "-325.314961663673",..: 467 726 678 811 569 964 145 472 936 759 ...
##  $ point4: chr  "84.674447863047" "36.5904196090862" "141.984031851925" "-40.393099998574" ...
```

```r
head(df)
```

```
##   point1     point2            point3            point4
## 1      1   23.09878 -13.0197232684149   84.674447863047
## 2      1 -104.22859  62.2599928562882  36.5904196090862
## 3      1  -21.85079  46.2929212612363  141.984031851925
## 4      1   51.45084   92.810432687834  -40.393099998574
## 5      1  163.87557  11.5403865992509  33.8887831961705
## 6      1  -61.70152  180.788071958246 -118.788942836654
```

### Now we can call our changeclass function 

```r
df <- changeclass(df)
```

```
## [1] "changing point1:character to factor"
## [1] "changing point3:factor to numeric"
## [1] "changing point4:character to numeric"
```

```r
str(df)
```

```
## 'data.frame':	1000 obs. of  4 variables:
##  $ point1: Factor w/ 1 level "1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ point2: num  23.1 -104.2 -21.9 51.5 163.9 ...
##  $ point3: num  -13 62.3 46.3 92.8 11.5 ...
##  $ point4: num  84.7 36.6 142 -40.4 33.9 ...
```

```r
head(df)
```

```
##   point1     point2    point3     point4
## 1      1   23.09878 -13.01972   84.67445
## 2      1 -104.22859  62.25999   36.59042
## 3      1  -21.85079  46.29292  141.98403
## 4      1   51.45084  92.81043  -40.39310
## 5      1  163.87557  11.54039   33.88878
## 6      1  -61.70152 180.78807 -118.78894
```

### If we have less than 100 rows the function will exit 


```r
df = data.frame(point1 = rnorm(31,1,0),point2 = rnorm(31,1,100),point3 = rnorm(31,1,100),point4 = rnorm(31,1,100))
df <- changeclass(df)
```

```
## [1] "please change column types manually; record count < 100, stopping operation"
```

#Dummycode
* There are many functions our there to dummy code but this function is very intuitive and fast enough for datasets < 100 million.
* We can state whether we want to keep or drop our columns that will be dummy coded

```r
df = DummyCode(mtcars,c('carb','am'),F)
```

```
## [1] "carb"
## [1] "am"
```

```r
head(df)
```

```
##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
##                   carb.1 carb.2 carb.3 carb.4 carb.6 carb.8 am.0 am.1
## Mazda RX4              0      0      0      1      0      0    0    1
## Mazda RX4 Wag          0      0      0      1      0      0    0    1
## Datsun 710             1      0      0      0      0      0    0    1
## Hornet 4 Drive         1      0      0      0      0      0    1    0
## Hornet Sportabout      0      1      0      0      0      0    1    0
## Valiant                1      0      0      0      0      0    1    0
```


```r
df = DummyCode(mtcars,c('carb','am'),T)
```

```
## [1] "carb"
## [1] "am"
```

```r
head(df)
```

```
##                    mpg cyl disp  hp drat    wt  qsec vs gear carb.1 carb.2
## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0    4      0      0
## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0    4      0      0
## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1    4      1      0
## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1    3      1      0
## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0    3      0      1
## Valiant           18.1   6  225 105 2.76 3.460 20.22  1    3      1      0
##                   carb.3 carb.4 carb.6 carb.8 am.0 am.1
## Mazda RX4              0      1      0      0    0    1
## Mazda RX4 Wag          0      1      0      0    0    1
## Datsun 710             0      0      0      0    0    1
## Hornet 4 Drive         0      0      0      0    1    0
## Hornet Sportabout      0      0      0      0    1    0
## Valiant                0      0      0      0    1    0
```

#kfold
* Typically we use kfold for partitioning data for cross validating models as well as an index for parallel functions

```r
iris = suppressMessages(kfold(iris,3))
table(iris$Species,iris$folds)
```

```
##             
##               1  2  3
##   setosa     18 14 18
##   versicolor 18 15 17
##   virginica  14 21 15
```

#imputer
* The imputer accepts a data frame and whichever column needs to be imputed and then you specify between the following: mean,median,regression
* The imputer will output which method was selected along with the method that it believes is best based upon RMSE. 

```r
df = Boston
df[,1][c(1,6,8,12,66,77,88,101,303)] <- NA

imputed.df <- impute(df,1,F,F,T)
```

```
## [1] "Based upon using a sample of 10% of the complete cases the imputer ran all three methods and believes BlendedModel is the best method"
##         Method       MSE        MASE     RMSE
## 1         Mean 20.925190 -1.23195244 4.574406
## 2       Median 24.181747  2.18500888 4.917494
## 3  LinearModel  8.798885 -0.84393045 2.966291
## 4 BlendedModel  8.451373  0.06052171 2.907125
## [1] "imputing regression"
```

```r
imputed.df <- impute(df,1,F,T,F)
```

```
## [1] "Based upon using a sample of 10% of the complete cases the imputer ran all three methods and believes LinearModel is the best method"
##         Method       MSE       MASE     RMSE
## 1         Mean 26.221783 -0.3335394 5.120721
## 2       Median 35.462068  3.0580277 5.955004
## 3  LinearModel  9.564691 -0.6605308 3.092683
## 4 BlendedModel 13.843443  0.7141175 3.720678
## [1] "imputing median"
```

```r
imputed.df <- impute(df,1,T,F,F)
```

```
## [1] "Based upon using a sample of 10% of the complete cases the imputer ran all three methods and believes LinearModel is the best method"
##         Method      MSE      MASE     RMSE
## 1         Mean 34.58237 -1.077915 5.880678
## 2       Median 39.23583  2.411506 6.263851
## 3  LinearModel 13.84402 -0.532268 3.720755
## 4 BlendedModel 19.83993  0.291454 4.454203
## [1] "imputing mean"
```


#SummaryStats
* FiveRepresent: This means 5 uniqe values represent x amount of the data in that variable
* Zscore: This is used for outlier detection, I have it set to display a +5 or -5 standard deviations 
* This function also calls the changeclass function

```r
data <- suppressMessages(df_stats(Boston))
```

```
## [1] "changing zn:numeric to factor"
## [1] "changing chas:integer to factor"
## [1] "changing rad:integer to factor"
```

```r
data 
```

```
##    variable   Mean    Min    Max Median    Std       Sum Rowcount Zscore
## 1      crim   3.61   0.01  88.98   0.26   8.60   1828.44      506      4
## 2     indus  11.14   0.46  27.74   9.69   6.86   5635.21      506      0
## 3       nox   0.55   0.38   0.87   0.54   0.12    280.68      506      0
## 4        rm   6.28   3.56   8.78   6.21   0.70   3180.03      506      0
## 5       age  68.57   2.90 100.00  77.50  28.15  34698.90      506      0
## 6       dis   3.80   1.13  12.13   3.21   2.11   1920.29      506      0
## 7       tax 408.24 187.00 711.00 330.00 168.54 206568.00      506      0
## 8   ptratio  18.46  12.60  22.00  19.05   2.16   9338.50      506      0
## 9     black 356.67   0.32 396.90 391.44  91.29 180477.06      506      0
## 10    lstat  12.65   1.73  37.97  11.36   7.14   6402.45      506      0
## 11     medv  22.53   5.00  50.00  21.20   9.20  11401.60      506      0
##    Missing UniqueValues FiveRepresent
## 1        0          504          0.01
## 2        0           76          0.43
## 3        0           81          0.18
## 4        0          446          0.03
## 5        0          356          0.12
## 6        0          412          0.04
## 7        0           66          0.46
## 8        0           46          0.48
## 9        0          357          0.26
## 10       0          455          0.03
## 11       0          229          0.09
```

#StaticMissingVars
* This will function do the following
* Drop any column that has more than 75% of its data missing
* Drop any column that is 95% or more static
* Warn you of any column that is 80-94% static


```r
df <- data.frame(ID = 1:100,X = rnorm(100),X2 = rnorm(100),X3 = rnorm(100),D3 = 2,Y = rnorm(100),Output = 1,Volume=4)
df[,2][c(1:75)] <- NA
df[,7][c(1:96)] <- 3
df[,5][c(1:85)] <- 3
df <- Static_Missing_Vars(df) 
```

```
## [1] "Variable D3 was not dropped, but 0.85 of the cases are static"
## [1] "The following variables will be dropped"
##                                              list
## 1          0.75% of the data is NA in variable: X
## 2 0.96% of the data is Static in variable: Output
## 3  100% of the data is Static in variable: Volume
```

#parallelApply 
* All of the workstations we use have 8 cores. However we set this to use only 5 of the cores 
* This function essentially works as a a wrapper. You impute the dataframe, function, and number of parameters (Currently the limit is two)


```r
p.func <- function(x){
  if(x > 2){"big"
  }else if(x == 1){"Thats random"
  }else{"Hello"}
}

df <- data.frame(id = rnorm(10000))
df <- suppressMessages(parallelApply(df,1,p.func,1))
table(df$ParRow)
```

```
## 
##   big Hello 
##   221  9779
```
 
## Example with two parameters 

```r
m.func <- function(x,y){
  if(x > 20 & y > 200){"big"
  }else if(x < 20 & y > 188){"Thats random"
  }else{"Hello"}
}

df <- data.frame(id = rnorm(10000,mean = 18,sd = 10),x =rnorm(10000), y = rnorm(10000,mean = 200,sd = 60))
df <- suppressMessages(parallelApply(df,c(1,3),m.func,2))
table(df$ParRow)
```

```
## 
##          big        Hello Thats random 
##         2108         4585         3307
```
 
#trim

* we a have a lot of free form text in our contact data with odd spacing

* the function is using the str_trim from stringr along with multiple gsub statements removing whitespace

```r
sentence <- "It    is a beautiful    day, let us go to the        park for a    picinic "
trim(sentence)
```

```
## [1] "It is a beautiful day, let us go to the park for a picinic"
```


 
 
 
 
 
 
