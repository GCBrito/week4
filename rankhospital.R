rankhospital <- function(state, outcome, num = "best") {
        # 1. load data and produce a data.frame with useful columns 
        data <- read.csv("outcome-of-care-measures.csv", sep=",", 
                         na.strings="Not Available", stringsAsFactors=FALSE)
        
        mydata <- data[,c(2,7,11,17,23)]
        colnames(mydata) <- c("Hospital.Name","State","heart attack" ,
                              "heart failure","pneumonia")    
        
        # 2. Checks whether state and outcome exist
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
        
        # 5. If rank number requested greater than number of hospitals in state, return NA, else
        # returns selected rank
        if (class(num)=="numeric"){ if(num>nrow(mydata3)) {return(NA)}
                else {   mydata4 <-  mydata3[order(mydata3[,outcome],mydata3[,1]),]  }
                print(mydata4$Hospital.Name[num])
                }
        # 6. Returns best hospital in selected state
        else if (num=="best") {   
                mydata4 <- mydata3[which.min(mydata3[,outcome]),]
                print(mydata4$Hospital.Name) }
        
        # 7. Returns worst hospital in selected state
        else if (num=="worst") {
                mydata4 <- mydata3[which.max(mydata3[,outcome]),]
                print(mydata4$Hospital.Name) } 
        
        
}
