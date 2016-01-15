# Data Source: Copy of agqd_2016_data (by @minimaxir)
# Data available for download from https://docs.google.com/spreadsheets/d/1yyfkS0jvRK1cWrQesYiBn1TMGC93lo1MqahcU3XeGIU/edit

#Read in Data
setwd("~/Desktop/R Data/Analysis-Projects")
GetData <- function() {
        #read in .CSV
        dat <<- read.csv("AGDQ2016.csv")
        
        #create POSIXlt out of time_donated
        dat$time_donated <<- strptime(dat$time_donated, "%m/%d/%Y %T")
}

AnalyzeByHour <- function() {
        ##Add and name Hour_of_Donation
        dat2 <- data.frame()
        print(class(dat$time_donated))
        dat2 <- cbind(dat, dat$time_donated$hour)
        n <- names(dat2)
        n[length(n)] <- "Hour_of_Donation"
        names(dat2) <- n
        
        attach(dat2)
        plot(Hour_of_Donation, amount_donated)
        head(dat2)
        
}
