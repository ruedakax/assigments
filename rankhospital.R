rankhospital <- function(state, outcome,num="best") {      
  ## Read outcome data
  outcomesindex <- c("heart attack","heart failure","pneumonia")
  outcomes_list <- c(11,17,23)
  names(outcomes_list) <- outcomesindex  
  datos<-read.csv(file="outcome-of-care-measures.csv")    
  resstate <- sum(unique(datos$State) %in% state)
  resoutcome <- sum(outcomesindex %in% outcome)                 
  ## Check that state and outcome are valid
  if(resstate < 1)  {
    stop("invalid state")
  }else if (resoutcome < 1){
    stop("invalid outcome")
  }  
  # check num option 
  if(num!="best" & num !="worst" & is.numeric(num)==FALSE){
    stop("invalid num")
  }
  ## Return hospital name 
  columna <- outcomes_list[[outcome]]
  columnas <- c(2,7,columna)
  datos <- datos[which(datos$State == state & datos[,columna] != "Not Available"),columnas]
  calificaciones <- as.numeric(levels(datos[,3]))[datos[,3]]
  nombres <- as.character(levels(datos[,1]))[datos[,1]]
  ordenado <- order(calificaciones,nombres)  
  if(num =="best"){
    valor <- ordenado[1]
  }else if(num =="worst"){
    valor <- ordenado[length(nombres)]
  }else if(as.numeric(num) < length(calificaciones) ){
    valor <- ordenado [num]    
  } else{
    valor = length(nombres) + 1
  }      
  finalista <- nombres[valor]
  finalista  
}