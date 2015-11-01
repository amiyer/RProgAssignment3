rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ds <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = "Not Available")

  ## Check that state and outcome are valid
  
  
  validState <- unique(ds[, 7])
  if (!state %in% validState ){ stop(c("invalid state"))}
  
  else if ( outcome != "heart attack" & outcome != "heart failure" & outcome!="pneumonia" ) { stop(c("invalid outcome"))}
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  dState <- subset(ds, ds$State == state)
  dHos <- data.frame( "HospitalName" = character(0), "Rate" = numeric(0))
  if ( outcome == "heart attack" ) {
    dHos <- dState[,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack") ]
  }
  else if ( outcome == "heart failure" ){
    dHos <- dState[,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure") ]
  }
  else {
    dHos <- dState[,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") ]
  }
  dHos <- dHos[complete.cases(dHos),] 
  names(dHos) = c("HospitalName","Rate")
  dHos[,2] <- as.numeric( dHos[,2])
  dHos <- dHos[order( dHos$Rate,  dHos$HospitalName),]
  ##return( dHos )
  if (num == "best") { return(head( dHos,1)$HospitalName)  }
  else if (num == "worst"){ return(tail(dHos,1)$HospitalName)}
  else if ( num > nrow(dHos)) { return ( NA ) }
  else {
    return ( dHos[num,1])
  }
  
}
