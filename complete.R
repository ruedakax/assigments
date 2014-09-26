complete <- function(directory="specdata", id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases  
  matriz <- matrix(nrow = length(id),ncol = 2)    
  j <- 1
  for(i in id){
    fname <- formatC(i,width = 3,flag = "0")
    pname <- paste(directory,"/",fname,".csv",sep="")    
    valores <- read.csv(file = pname)         
    matriz[j,] <- c(i,nrow(na.omit(valores)))    
    j<-j+1
  }   
  matriz <- data.frame(matriz)
  names(matriz)<-c("id","nobs")
  matriz
}
