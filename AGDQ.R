# Data Source: Copy of agqd_2016_data (by @minimaxir)
# Data available for download from https://docs.google.com/spreadsheets/d/1yyfkS0jvRK1cWrQesYiBn1TMGC93lo1MqahcU3XeGIU/edit

#Read in Data
setwd("~/Desktop/R Data/Analysis-Projects")
GetDat <- function() {
        #read in .CSV
        dat <<- read.csv("AGDQ2016.csv")
        dat.raw <<- dat
        
        #create POSIXlt out of time_donated
        dat$time_donated <<- strptime(dat$time_donated, "%m/%d/%Y %T")
} #All the formulas from here on are going to take dat as input

AnalyzeByHour <- function(dat) {
        ##Add and name Hour_of_Donation as last column
        dat2 <- data.frame()
        print(class(dat$time_donated))
        dat2 <- cbind(dat, dat$time_donated$hour)
        n <- names(dat2)
        n[length(n)] <- "Hour_of_Donation"
        names(dat2) <- n
        
        dat <<- dat2
}
