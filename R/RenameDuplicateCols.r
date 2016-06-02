
RenameDuplicateCols <- function(df){
  
  li <- which(colnames(df) %in% names(which(table(colnames(df))> 1)))
  while(length(li)>1){
    i = 1
    update.list <- which(colnames(df) %in% colnames(df)[li[i]])
    
    len <- length(update.list)
    
    # Remove records that are being renamed
    li <- li[-which(li %in% update.list)]
    colnames(df)[update.list] <- paste(colnames(df)[update.list],'_',1:len,sep='')
    # break operation once li is 0
    if(length(li) ==0){break}
  }
  
  return(df)
}
