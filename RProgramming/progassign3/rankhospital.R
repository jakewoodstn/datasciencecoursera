rankhospital <- function(state, outcome, num="best") {
     ## Read outcome data
     outcomeData <- read.csv("outcome-of-care-measures.csv")
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
     
     stdata <-outcomeData[,c(2,colSelect[match(outcome,outs)])]
     stdata[,2] <-as.numeric(levels(stdata[,2])[stdata[,2]])
     stdata<-stdata[complete.cases(stdata),]
     stdata<-stdata[order(stdata[,2],stdata[,1]),]
     
     if(!(num=="best" || num=="worst") && is.na(as.numeric(num))) stop("invalid index")
     else{
          if(num=="best") num<-1
          if(num=="worst") num<-dim(stdata)[1]
          else{
               num<-as.numeric(num)
               ##if (num>dim(stdata)[1]) stop('invalid index')
          }
     }
       
     as.character(stdata[num,1])

}