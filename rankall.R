rankall <- function(outcome, num = "best") {
  options(warn=-1)
  ## Read outcome data
  outcomesindex <- c("heart attack","heart failure","pneumonia")
  outcomes_list <- c(11,17,23)
  names(outcomes_list) <- outcomesindex  
  datos<-read.csv(file="outcome-of-care-measures.csv")      
  resoutcome <- sum(outcomesindex %in% outcome)  
  ## Check that outcome is valid
  if (resoutcome < 1){
    stop("invalid outcome")
  } 
  # Check num option 
  if(num!="best" & num !="worst" & is.numeric(num)==FALSE){
    stop("invalid num")
  }  
  ## For each state, find the hospital of the given rank
  columna <- outcomes_list[[outcome]]
  columnas <- c(2,7,columna)
  datos <- datos[which(datos[,columna] != "Not Available"),columnas]
  factorizado<- split(datos,datos$State)
  iteraciones = length(factorizado)
  output <- matrix(ncol=2, nrow=iteraciones)  
  tels <- lapply(factorizado,function(data,...){    
    calificaciones <- as.numeric(levels(data[,3]))[data[,3]]
    nombres <- as.character(levels(data[,1]))[data[,1]]
    ordenado <- order(calificaciones,nombres) 
    cantidad <- length(nombres)   
    if(num=="best"){
      valor <- ordenado[1]
    }else if(num =="worst"){
      valor <- ordenado[cantidad]
    }else if(as.numeric(num) < cantidad){
      valor <- ordenado [num]    
    }else{
      valor = cantidad + 1
    }    
    finalista <- nombres[valor]    
  },num)
  tels <- do.call(rbind.data.frame, tels)
  tels$state = rownames(tels)
  colnames(tels)<- c("hospital","state")
  tels
}