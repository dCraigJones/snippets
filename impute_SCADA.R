.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")


Impute.WLL <- function(input) {
    #input <- "./scada/3 - scada/WLL.csv"
    #output <- "./scada/4 - impute/WLL.csv"
    
  error_msg <- c("CommFail", "Comm Fail", "NotConnect", "I/OTimeout", "IntfShut", "Shutdown", "Configure","Tagnotfound", "[-10722]PINET:TimeoutonPIRPCorSystemCall.")
    
    numCols <- ncol(read.csv(input, header=TRUE, nrows=1))
    
    WLL <- read.csv(input
                    , colClasses=c("character",rep("numeric", (numCols-1)))
                    , na.strings=error_msg)
    
    mu <- apply(WLL[,2:ncol(WLL)],2,mean, na.rm=TRUE)
    
    for (i in 2:ncol(WLL)){
      WLL[is.na(WLL[,i]),i] <- mu[i-1]
    }
    
    for (i in 2:ncol(WLL)){
      WLL[is.nan(WLL[,i]),i] <- 0
    }
    
    WLL[, 2:ncol(WLL)] <- round(WLL[, 2:ncol(WLL)],2)
    
    #write.csv(WLL, output)
    return(WLL)
}

Impute.RUN <- function(input) {
    error_msg <- c("CommFail", "Comm Fail", "NotConnect", "I/OTimeout", "IntfShut", "Shutdown", "Configure","Tagnotfound", "[-10722]PINET:TimeoutonPIRPCorSystemCall.")

    # get subset of data to export
    subset <- read.csv(input, header=TRUE, nrows=1)
    
    # get total number of columns in subset
    num_cols <- ncol(subset)
    
    # find columns with "RUN" in tag
    run_cols <- grep("RUN", names(subset))
    
    # find columns for VFDs
    vfd_cols <- setdiff(2:num_cols, run_cols)
    
    # set column class
    col_type <- rep("character", num_cols)
    col_type[vfd_cols] <- "numeric"
    
    RUN <- read.csv(input
                    , colClasses=col_type
                    , na.strings=error_msg
    )
    
    
    for (i in 1:length(run_cols)) {
      r <- run_cols[i]
      RUN[is.na(RUN[,r]),r] <- "OFF"
    }
    
    for (i in 1:length(run_cols)) {
      r <- run_cols[i]
      RUN[RUN[,r]=="START",r] <- 1
      RUN[RUN[,r]=="RUN",r] <- 1
      RUN[!(RUN[,r]==1),r] <- -1
    } 
    
    
    if(length(vfd_cols)>0) {
        for (i in 1:length(vfd_cols)) {
          r <- vfd_cols[i]
          RUN[is.na(RUN[,r]),r] <- 0
        }
        
        for (i in 1:length(vfd_cols)) {
          r <- vfd_cols[i]
          RUN[,r] <- 1800*RUN[,r]/60
        } 
    }
    
    #write.csv(RUN, output)
    return(RUN)
}

