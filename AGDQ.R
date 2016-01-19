# Data Source: Copy of agqd_2016_data (by @minimaxir)
# Data available for download from https://docs.google.com/spreadsheets/d/1yyfkS0jvRK1cWrQesYiBn1TMGC93lo1MqahcU3XeGIU/edit

library(plyr)
library(stringr)

#Read in Data
setwd("~/Desktop/R Data/Analysis-Projects")
#download list of positive and negative words
GetLexicon <- function(){
        download.file("https://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar", "lexicon.rar")
        message("LEXICON DOWNLOADED TO WD. MANUAL UNPACKING NEEDED")
        message("Please Unpack to WD")
}

ReadLexicon <- function(AddWords=TRUE){
        #Read in the lists of positive and negative words
        positive <<- read.table("positive-words.txt", comment.char = ";")
        names(positive) <<- "words"
        positive$words <<- sapply(positive$words,as.character)
        negative <<- read.table("negative-words.txt", comment.char = ";")
        names(negative) <<- "words"
        negative$words <<- sapply(negative$words,as.character)
        
        #AddWords decides whether or not append a list of special words
        SpecialPositive <- list("leet","l33t","hacks")
        SpecialNegative <- list("rekt","failfish")

        
        if(AddWords==TRUE){
                positive2 <- c(positive$words, as.character(SpecialPositive))
                negative2 <- c(negative$words, as.character(SpecialNegative))
                #negative <<- negative2positive$words <<- c(positive$words, SpecialPositive)
                #negative$words <<- c(negative$words, SpecialNegative)
        }
        positive <<- as.list(positive2)
        negative <<- as.list(negative2)
}
        

GetDat <- function() {
        #read in .CSV
        library(ggplot2)
        dat <<- read.csv("AGDQ2016.csv")
        dat.raw <<- dat
        
        #create POSIXlt out of time_donated
        dat$time_donated <<- strptime(dat$time_donated, "%m/%d/%Y %T")
        dat$time_donated <<- as.POSIXlt(dat$time_donated)
        dat$amount_donated <<- as.numeric(dat$amount_donated)
        dat <<- dat
        dat.reset <<- dat
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

AnalyzeByTime <- function(dat) {
        ##Add and name Hour_of_Donation as last column
        dat2 <- data.frame()
        print(class(dat$time_donated))
        dat2 <- cbind(dat, dat$time_donated$hour + (dat$time_donated$min/60))
        n <- names(dat2)
        n[length(n)] <- "Time_of_Donation"
        names(dat2) <- n
        
        dat <<- dat2
}

RateSentiment <- function(sentences = dat$comment, pos=positive, neg=negative, .progress= "none", AppendToDat=TRUE) {
        n = 1
        scores <<- laply(sentences, function(sentence, pos, neg){
                #clean up sentences
                sentence <- gsub('[[:punct:]]','',sentence)
                sentence <- gsub('[[:cntrl:]]','',sentence)
                sentence <- gsub('\\d+','',sentence)
                #convert to lower
                sentence <- tolower(sentence)
                
                #create list of words
                WordList <- str_split(sentence, '\\s+')
                
                words <- unlist(WordList)
                
                #compare words to lexicon of positive and negatives
                pos.matches <- match(words, pos)
                #message(pos.matches)
                neg.matches <- match(words, neg)
                
                #match returns na or the position of the match
                #the following turns it all into true/false
                pos.matches <- !is.na(pos.matches)
                #message(pos.matches)
                neg.matches <- !is.na(neg.matches)
                
                #The sum of true/false is the same as counting.
                score <- sum(pos.matches) - sum(neg.matches)
                if(n %% 100 == 0){
                        print(n)
                }
                n <- n+1
                return(score)
                
        }, pos, neg, .progress=.progress)
        
        scores.df <- data.frame(score = scores, comment = sentences)
        
        if(AppendToDat==TRUE){
                dat <<- cbind(dat, scores.df[1])
        }
        
        return(scores.df)
}
polarity <- function(score){
        if(score > 0){return("positive")}
        if(score < 0){return("negative")}
        if(score == 0){return("neutral")}
}