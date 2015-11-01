best <- function(state, outcome) {
  ## Read outcome data
  ds <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##ds[, 11] <- as.numeric(ds[, 11])
  ##ds[, 17] <- as.numeric(ds[, 17])
  ##ds[, 23] <- as.numeric(ds[, 23])
  
  ## Check that state and outcome are valid
  

  validState <- unique(ds[, 7])
  if (!state %in% validState ){ stop(c("invalid state"))}
  
  else if ( outcome != "heart attack" & outcome != "heart failure" & outcome!="pneumonia" ) { stop(c("invalid outcome"))}
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  dState <- subset(ds, ds$State == state)

  if ( outcome == "heart attack" ) {
      dHos <- subset(dState, dState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(as.numeric(dState[,11]), na.rm = TRUE))
  }
  else if ( outcome == "heart failure" ){
     dHos <- subset(dState, dState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(as.numeric(dState[,17]), na.rm = TRUE))
  }
  else {
    dHos <- subset(dState, dState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia== min(as.numeric(dState[,23]), na.rm = TRUE))
  }
  dHos <- dHos[order(dHos$Hospital.Name),]
  return ( head(dHos$Hospital.Name, 1))
  
}