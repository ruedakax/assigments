best <- function(state, outcome) {    
  options(warn=-1)  
  ## Read outcome data
  outcomesindex <-c("heart attack","heart failure","pneumonia")
  outcomes_list <-c(11,17,23)
  names(outcomes_list)<-outcomesindex
  
  datos<-read.csv(file="outcome-of-care-measures.csv")    
  resstate <- sum(unique(datos$State) %in% state)
  resoutcome <- sum(outcomesindex %in% outcome)               
  #estados <-read.csv(file="hospital-data.csv")  
  ## Check that state and outcome are valid
  if(resstate < 1)  {
    stop("invalid state")
  }else if (resoutcome < 1){
    stop("invalid outcome")
  }  
  ## Return hospital name in that state with lowest 30-day death
  
  columna <- outcomes_list[[outcome]]
  columnas <- c(2,7,columna)
  datos<-datos[which(datos$State==state & datos[,columna] !="Not Available"),columnas]
  datos[,3]<-as.numeric(levels(datos[,3]))[datos[,3]]
  minimo <-min(datos[,3])
  finalistas <- as.vector(datos[which(datos[,3] == minimo),1])
  finalistas <- sort(finalistas)
  finalista <- finalistas[1]
  finalista
}