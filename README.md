---
title: "Examples"
author: "Christopher M"
date: "Tuesday, May 24, 2016"
output: html_document
---

This package was created to provide analysts with functions that are useful relative to the data within the enterpise. 

[changeclass](#changeclass)
[Dummycode](#Dummycode)
[Kfold](#Kfold)
[preprocess](#preprocess)
[SummaryStats](#SummaryStats)





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
##  $ point2: num  159.91 -8.24 165.77 199.44 -108.35 ...
##  $ point3: Factor w/ 1000 levels "-365.166396600387",..: 895 83 153 561 527 126 337 167 927 336 ...
##  $ point4: chr  "-10.8586108245594" "8.3556447971304" "-94.510862706066" "-8.07482983839825" ...
```

```r
head(df)
```

```
##   point1      point2            point3            point4
## 1      1  159.908558  120.188153401215 -10.8586108245594
## 2      1   -8.236693 -144.136939289412   8.3556447971304
## 3      1  165.771078 -111.185628864403  -94.510862706066
## 4      1  199.441814  16.5395263273032 -8.07482983839825
## 5      1 -108.349662  5.17942219090358  47.5282729859632
## 6      1   86.964060 -120.629402123547  42.2113076342095
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
##  $ point2: num  159.91 -8.24 165.77 199.44 -108.35 ...
##  $ point3: num  120.19 -144.14 -111.19 16.54 5.18 ...
##  $ point4: num  -10.86 8.36 -94.51 -8.07 47.53 ...
```

```r
head(df)
```

```
##   point1      point2      point3     point4
## 1      1  159.908558  120.188153 -10.858611
## 2      1   -8.236693 -144.136939   8.355645
## 3      1  165.771078 -111.185629 -94.510863
## 4      1  199.441814   16.539526  -8.074830
## 5      1 -108.349662    5.179422  47.528273
## 6      1   86.964060 -120.629402  42.211308
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
### There are many functions our there to dummy code but we feel that this function is very intuitive.

### We can state whether we want to keep or drop our columns that will be dummy coded

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
##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb folds
## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4     5
## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4     5
## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1     5
## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1     5
## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2     5
## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1     5
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
##                    mpg cyl disp  hp drat    wt  qsec vs gear folds carb.1
## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0    4     5      0
## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0    4     5      0
## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1    4     5      1
## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1    3     5      1
## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0    3     5      0
## Valiant           18.1   6  225 105 2.76 3.460 20.22  1    3     5      1
##                   carb.2 carb.3 carb.4 carb.6 carb.8 am.0 am.1
## Mazda RX4              0      0      1      0      0    0    1
## Mazda RX4 Wag          0      0      1      0      0    0    1
## Datsun 710             0      0      0      0      0    0    1
## Hornet 4 Drive         0      0      0      0      0    1    0
## Hornet Sportabout      1      0      0      0      0    1    0
## Valiant                0      0      0      0      0    1    0
```




 
