rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ds <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = "Not Available")
  

  validState <- unique(ds[, 7])

  if ( outcome != "heart attack" & outcome != "heart failure" & outcome!="pneumonia" ) { stop(c("invalid outcome"))}
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  
  ##dState <- subset(ds, ds$State == state)
  dHos <- data.frame( "HospitalName" = character(0), "State" = character(0) , "Rate" = numeric(0))
  if ( outcome == "heart attack" ) {
    dHos <- ds[,c("Hospital.Name","State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack") ]
  }
  else if ( outcome == "heart failure" ){
    dHos <- ds[,c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure") ]
  }
  else {
    dHos <- ds[,c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") ]
  }
  dHos <- dHos[complete.cases(dHos),] 
  names(dHos) = c("HospitalName","State" , "Rate")
  dHos[,3] <- suppressWarnings( as.numeric( dHos[,3])  )
                      
  returnDS <- vector()
  for (i in 1:length(validState)) {
    stateDS <-   dHos[dHos$State == validState[i],]
    stateDS <-   stateDS[order( stateDS$Rate, stateDS$HospitalName),]
    if (num == "best") {  

        stateDS <-   head( stateDS, 1)  
        
        returnDS <- append(returnDS,  c( as.character( stateDS[1,1] ) , as.character( stateDS[1,2])))
      }
    else if (num == "worst"){
      stateDS <-   tail( stateDS, 1)    
      returnDS <- append(returnDS,  c( as.character( stateDS[1,1] ) , as.character( stateDS[1,2])))
      
    }
    else {
      if ( num > nrow(stateDS)) {
        returnDS <- append(returnDS,  c( NA , as.character( stateDS[1,2])))
        
      }
      else {
        returnDS <- append(returnDS,  c( stateDS[num, 1] , stateDS[num, 2]))
      }
    }  
  }

  returnDS <- as.data.frame(matrix(returnDS, length(validState), 2, byrow = TRUE))
  colnames(returnDS) <- c("hospital", "state")
  returnDS <- returnDS[ order( returnDS$state ),]
  
  return (  returnDS )
}
  
