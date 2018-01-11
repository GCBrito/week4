best <- function(state, outcome) {
        # 1. load data and produce a data.frame with useful columns 
        data <- read.csv("outcome-of-care-measures.csv", sep=",", 
                            na.strings="Not Available", stringsAsFactors=FALSE)
        
        mydata <- data[,c(2,7,11,17,23)]
        colnames(mydata) <- c("Hospital.Name","State","heart attack" ,
                              "heart failure","pneumonia")    
        
        # 2. Checks whether state exists
        validst <- state %in% mydata$State
        if(validst==0){stop("invalid state")}
        validout<- outcome %in% c("pneumonia","heart attack", "heart failure")
        if(validout==0){stop("invalid outcome")}
        
        # 3.Remove hospitals with NA in outcome 
        if(as.character(outcome)=="heart attack"){
        mydata2 <- mydata[complete.cases(mydata$'heart attack'),] }
        
        else if (as.character(outcome)=="heart failure"){
                mydata2 <- mydata[complete.cases(mydata$'heart failure'),] }
        
        else  mydata2 <- mydata[complete.cases(mydata$'pneumonia'),] 
        
        # 4. Take only the hospitals in the selected state

        mydata3 <- mydata2[mydata2$State==state,]
        
        # 5. Presents the hospital with the lowest value in the selected output 
        mydata4 <- mydata3[which.min(mydata3[,outcome]),]

                print(mydata4$Hospital.Name)
                
}