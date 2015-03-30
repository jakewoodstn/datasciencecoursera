best <- function(state, outcome) {
     ## Read outcome data
     outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses="character")
     outcome <- tolower(outcome)
     ## Check that state and outcome are valid
     st <- unique(outcomeData[,7])                                 ##valid state vector
     outs <- c('heart attack', 'heart failure', 'pneumonia') ##valid outcomes (from data set)
     
     if (!state %in% st) stop('invalid state')
     if(!outcome %in% outs) stop('invalid outcome')
     
     ##compute location of minimum death rate
     colSelect <- c(11,17,23)
     outcomeSplit<-split(outcomeData,outcomeData$State)
     outcomeData<-outcomeSplit[[state]]
     
     col <- as.numeric(outcomeData[,colSelect[match(outcome,outs)]])
     loc <- which.min(col)
     
     ## Return hospital name in that state with lowest 30-day death
     ## rate
     
     outcomeData$Hospital.Name[loc]
     
}