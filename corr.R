corr <- function(directory="specdata", threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations  
  correlaciones <- numeric()
  for(i in 1:332){
    fname <- formatC(i,width = 3,flag = "0")
    pname <- paste(directory,"/",fname,".csv",sep="")    
    valores <- read.csv(file = pname)
    valores <- na.omit(valores)    
    cantidad <- nrow(valores)
    if(cantidad > threshold){        
        correlaciones <- c(correlaciones,round(cor(valores$sulfate,valores$nitrate),4))
    }    
    
  }   
  correlaciones    
}