## Programming Assignment 3

## The rankhospital function ranks all the hospitals in a state 
## by a user selected mortality rate, breaking ties alphabetically.
## Lower mortality rates are better. Options for the user selection
## of mortaility rates are "heart attack", "heart failure", and 
## "pneumonia". Users can request the "best" or "worst" rank
## or can pass an integer for rank to receive the hospital 
## with that ranking.

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        hostpitalData <- read.csv("outcome-of-care-measures.csv", 
                                  colClasses = "character")        
        
        ## Check that state, outcome, and num are valid
        validStates <- unique(hostpitalData$State)
        validState <- state %in% validStates
        validOutcomes = c("heart attack", "heart failure",
                          "pneumonia")
        validOutcome <- outcome %in% validOutcomes
        validNum <- (num %in% c("best","worst") | is.numeric(num))
        #print(paste("length of validOutcome =",length(validOutcome)))
        if (!validState) {
                stop(message = "invalid State")
        }
        if (!validOutcome) {
                stop(message = "invalid outcome")
        }        
        if (!validNum) {
                stop(message = "invalid num")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        myVar <- 
                if (outcome == "heart attack") {
                        paste("Hospital.30.Day.Death.",
                                ".Mortality..Rates.from.Heart.Attack",
                                sep = "")
                } else if (outcome == "heart failure") {
                        paste("Hospital.30.Day.Death.",
                                ".Mortality..Rates.from.Heart.Failure",
                                sep="")
                } else {
                        paste("Hospital.30.Day.Death..Mortality.",
                                ".Rates.from.Pneumonia",
                              sep="")
                }        
        stateData <- hostpitalData[hostpitalData$State == state,c("Hospital.Name",myVar)]
        suppressWarnings(stateData[,2] <- as.numeric(stateData[,2]))
        stateData <- stateData[complete.cases(stateData),]     
        sortedStateData <- order(stateData[,2],stateData[,1])
        index <- 
                if (num == "best") {
                        1
                } else if (num == "worst") {
                                length(sortedStateData)
                } else {num}
        answerIndex = sortedStateData[index]
        stateData[answerIndex,1]
}