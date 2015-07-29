---
output: html_document
---
rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  if(state %in% data[,7]==FALSE){
    stop("invalid state")
  }
  if(!(outcome=="pneumonia"|outcome=="heart attack"|outcome=="heart failure")){
    stop("invalid outcome")
  }
  ##if(num=="best"){num<-1}
    ##else if(num=="worst"){num}
  
  if(outcome=="heart attack") {
    
    a<-data[data$State==state,]   #creates data frame for state entered
    b<-a[complete.cases(a[,11]),] #removes NAs
    d<-order(b[,11],b[,2])        #creates index of ranked lines by column 11, then 2
                   
    
    if(num=="best"){rank<-1}
    else if(num=="worst"){rank<-nrow(b)} #rank<- number of rows for that state, hospital number if worst state needed
    else {rank<-num}
    
    if(rank>nrow(b)){
      return(NA)
    }
    else {
    e<-b[d,]
    return(e[rank,2])
    }
    
  }
  if(outcome=="heart failure") {
    
    a<-data[data$State==state,]   #creates data frame for state entered
    b<-a[complete.cases(a[,17]),] #removes NAs
    d<-order(b[,17],b[,2])        #creates index of ranked lines by column 11, then 2
    
    
    if(num=="best"){rank<-1}
    else if(num=="worst"){rank<-nrow(b)} #rank<- number of rows for that state, hospital number if worst state needed
    else {rank<-num}
    
    if(rank>nrow(b)){
      return(NA)
    }
    else {
      e<-b[d,]
      return(e[rank,2])
    }
    
  }
  if(outcome=="pneumonia") {
    
    a<-data[data$State==state,]   #creates data frame for state entered
    b<-a[complete.cases(a[,23]),] #removes NAs
    d<-order(b[,23],b[,2])        #creates index of ranked lines by column 11, then 2
    
    
    if(num=="best"){rank<-1}
    else if(num=="worst"){rank<-nrow(b)} #rank<- number of rows for that state, hospital number if worst state needed
    else {rank<-num}
    
    if(rank>nrow(b)){
      return(NA)
    }
    else {
      e<-b[d,]
      return(e[rank,2])
    }
    
  }
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}

