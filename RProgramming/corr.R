corr <- function(directory, threshold = 0) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'threshold' is a numeric vector of length 1 indicating the
     ## number of completely observed observations (on all
     ## variables) required to compute the correlation between
     ## nitrate and sulfate; the default is 0
     
     ## Return a numeric vector of correlations

     fnames<-dir(directory,full.names=TRUE)
     completes = complete(directory)
     uses = completes$id[completes$nobs>threshold]
     ret = vector()
     
     for ( i in seq_along(uses)){
          df<-read.csv(fnames[uses[i]])
          n<- df$nitrate[complete.cases(df)]
          s<- df$sulfate[complete.cases(df)]
          ret<-c(ret,cor(n,s))
     }
     
     ret
}