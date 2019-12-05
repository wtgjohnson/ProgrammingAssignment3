## rankall returns a dataframe with two variables. The first variable is
## a hospital name, and the second variable is a state. The data in the 
## frame lists the hospitals the have the rank passed to rankall in the
## num parameter for the outcome passed to rankall. Outcomes can have
## three values: "heart attack", "heart failure",  and "pneumonia". 
## These outcomes correspond to the 30-day mortality rates in the 
## source data frame. The parameter num can take "best", "worst", 
## or an integer as values.

rankall <- function(outcome, num = "best") {
        ## Helper function for selecting a hospital out of data for a
        ## single state
        selectRank <- function(rank, sortedStateHospitals) {
                index <- 
                        if (num == "best") {
                                1
                        } else if (num == "worst") {
                                nrow(sortedStateHospitals)
                        } else {num}
                sortedStateHospitals[["Hospital.Name"]]
        }
        
        ## Read outcome data
        hospitalData <- read.csv("outcome-of-care-measures.csv", 
                                  colClasses = "character")        

        ## Check that state and outcome are valid
        validStates <- unique(hospitalData$State)
        validOutcomes = c("heart attack", "heart failure",
                          "pneumonia")
        validOutcome <- outcome %in% validOutcomes
        validNum <- (num %in% c("best","worst") | is.numeric(num))
        #print(paste("length of validOutcome =",length(validOutcome)))
        if (!validOutcome) {
                stop(message = "invalid outcome")
        }        
        if (!validNum) {
                stop(message = "invalid num")
        }

        ## For each state, find the hospital of the given rank
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
        suppressWarnings(
                hospitalData[[myVar]] <- as.numeric(hospitalData[[myVar]])
        )
        hospitalData <- hospitalData[!is.na(hospitalData[[myVar]]),]
        stateData <- split(hospitalData,hospitalData$State)
        stateIndexes <- 
                lapply(stateData,
                        function(x) {
                                order(x[[myVar]],x[["Hospital.Name"]])
                        })
        answers <- 
                mapply(function(data,inds) {
                        index <- 
                                if (num == "best") {
                                        1
                                } else if (num == "worst") {
                                        length(inds)
                                } else {num}                        
                        c(data[inds[index],"Hospital.Name"],
                          data[1,"State"])},
                       stateData, stateIndexes, SIMPLIFY = FALSE)
        names <- sapply(answers, function(x) {x[[1]]})
        states <- sapply(answers, function(x) {x[[2]]})
        data.frame("hospital" = names, "state" = states)
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}