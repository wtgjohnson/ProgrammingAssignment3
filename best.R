## Programming Assignment 3

## best returns the best hospital in a state as ranked by the user's outcome
## The outcome can be one of "heart attack", "heart failure", or 
## "pneumonia".  

best <- function(state, outcome) {
        ## Read outcome data
        hostpitalData <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
        
        ## Check that state and outcome are valid
        validStates <- unique(hostpitalData$State)
        validState <- state %in% validStates
        validOutcomes = c("heart attack", "heart failure",
                                "pneumonia")
        validOutcome <- outcome %in% validOutcomes
        #print(paste("length of validOutcome =",length(validOutcome)))
        if (!validState) {
                stop(message = "invalid State")
        }
        if (!validOutcome) {
                stop(message = "invalid outcome")
        }

        ## Return hospital name in that state with lowest 30-day death
        ## rate
        myVar <- 
                if (outcome == "heart attack") {
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                } else if (outcome == "heart failure") {
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                } else {
                        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                }
        stateData <- hostpitalData[hostpitalData$State == state,c("Hospital.Name",myVar)]
        suppressWarnings(stateData[,2] <- as.numeric(stateData[,2]))
        stateData <- stateData[complete.cases(stateData),]
        bestRate <- min(stateData[,2])
        bestHospitals <- stateData[stateData[,2] == bestRate,][,1]
        bestHospital <- bestHospitals[sort.list(bestHospitals)][1]
        bestHospital
}