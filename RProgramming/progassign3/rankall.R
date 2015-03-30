rankall <- function(outcome, onum="best") {
     ## Read outcome data
     num<-onum
     outcomeData <- read.csv("outcome-of-care-measures.csv")
     outcome <- tolower(outcome)
     ## Check that outcome is valid
     outs <- c('heart attack', 'heart failure', 'pneumonia') ##valid outcomes (from data set)
     if(!outcome %in% outs) stop('invalid outcome')

     ##check that num is valid
          if(!(num=="best" || num=="worst") && is.na(as.numeric(num))) stop("invalid index")
     
     ##compute location of minimum death rate
     colSelect <- c(11,17,23)
     outcomeData<-outcomeData[,c(7,2,colSelect[match(outcome,outs)])]

     outcomeData[,3] <-suppressWarnings(as.numeric(levels(outcomeData[,3])[outcomeData[,3]]))
     outcomeData<-outcomeData[complete.cases(outcomeData),]
     outcomeData[4]=0
     outcomeData<-outcomeData[order(outcomeData[,1],outcomeData[,2]),] ##order by state then name
     names(outcomeData)[3:4]<-c('Measure','Rank')
     
     outcomeSplit<-split(outcomeData, outcomeData$State)
     
     result<-data.frame()
     
     for(i in names(outcomeSplit)){
          loopOutcome<-outcomeSplit[[i]]
          loopOutcome[,4]=rank(loopOutcome[,3],ties.method='first')

          if(onum=="best") num<-1
          else{
               if(onum=="worst") num<-dim(loopOutcome)[1]
               else{
                    num<-as.numeric(onum)
               }
          }
          if (i=="VI") {
               blah<-1
          }
          num
          loopOutcome<-loopOutcome[loopOutcome[,4]==num,]
          
          if (dim(loopOutcome)[1]==0)
               loopOutcome[1,]=c(i,'<NA>',NA,NA)
          
          result=rbind(result,loopOutcome[,c(2,1)])
     }
     names(result)<-c('hospital','state')
     result
}