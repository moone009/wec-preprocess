Static_Missing_Vars <- function(df) {
  
  list = c()
  for(i in 1:length(colnames(df))){
    
    # Agg data
    Cases = table(df[colnames(df)[i]])
    Cases = sort(Cases,decreasing=T)
    
    # Drop Variables that are > 95% missing
    if(sum(is.na(df[colnames(df)[i]]))/nrow(df) >.95)
    {
      list = c(list , c=colnames(df)[i])
    }
    # Drop variables that are close to static (Warning sometimes the minroity help explain variance)  
    else if(Cases[1]/nrow(df)>.95)
    {
      list = c(list , c=colnames(df)[i])     
    }
    else if(Cases[1]/nrow(df)>.8)
    {
      print(paste('Variable',colnames(df)[i],'was not dropped, but',Cases[1]/nrow(df),'of the cases are static'))
    }
  }
  if(length(list)>0){
    print('The following variables will be dropped')
    print(as.data.frame(list))
    df = df[, !(colnames(df) %in% c((list)))]
  }
  
  return(df)
}   