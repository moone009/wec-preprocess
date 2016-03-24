 

trim <-  function(x){ require(stringr)
                      x <- gsub('  ',' ',x) 
                      x <- gsub('  ',' ',x) 
                      x <- gsub('  ',' ',x) 
                      x <- gsub('  ',' ',x) 
                      x <- str_trim(x, side = c("both"))
                      return(x)}
