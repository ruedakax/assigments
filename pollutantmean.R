pollutantmean <- function(directory="specdata", pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
    
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  columna <-numeric(0)
  
  for(i in id){
    fname <- formatC(i,width = 3,flag = "0")
    pname <- paste(directory,"/",fname,".csv",sep="")    
    valores <- read.csv(file = pname)
    columna <- c(columna,valores[,pollutant])    
  }  
  means <- round(mean(columna,na.rm = TRUE),3)
  means
}
##-------------------------------------------------------------------------------
