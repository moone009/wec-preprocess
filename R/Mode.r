


mode <- function(x){
  
  idx <- table(t(x))
  max <- names(idx[which(idx %in% max(idx))])
  if(length(idx)==1){
    return(names(idx))
  }else if(length(max) == 1){
    return(max)
  }else{
    return('tie')
  }
  
}


test <- structure(list(one = structure(c(1L, 1L, 2L, 1L, 1L, 1L), .Label = c("good", 
"bad"), class = "factor"), two = structure(c(1L, 1L, 2L, 1L, 
1L, 1L), .Label = c("good", "bad"), class = "factor"), three = structure(c(1L, 
1L, 1L, 1L, 1L, 1L), .Label = c("good", "bad"), class = "factor"), 
four = structure(c(1L, 1L, 2L, 1L, 1L, 1L), .Label = c("good", 
"bad"), class = "factor")), .Names = c("one", "two", "three", 
"four"), row.names = c(NA, 6L), class = "data.frame")

apply(test[,1:4],1, mode) 



