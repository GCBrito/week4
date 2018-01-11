rankall <- function(outcome, num = "best") {
        # 1. load data and produce a data.frame with useful columns 
        data <- read.csv("outcome-of-care-measures.csv", sep=",", 
                         na.strings="Not Available", stringsAsFactors=FALSE)
        
        mydata <- data[,c(2,7,11,17,23)]
        colnames(mydata) <- c("Hospital.Name","State","heart attack" ,
                              "heart failure","pneumonia")    
        
        # 2. Check whether outcome is valid
        validout<- outcome %in% c("pneumonia","heart attack", "heart failure")
        if(validout==0){stop("invalid outcome")}
        
        # 3. Creates a factor with levels for every state and a data frame for storing results
        
        st <- as.factor(mydata$State)
        df <- matrix(,nrow=0,ncol=0)
    
        # 4. 
         
         
        if (class(num)=="numeric"){ if(num>nrow(mydata3)) {return(NA)}
                else {  x <- function(){ mydata4 <-  mydata[order(mydata[,outcome],mydata[,1]),]  }
                return(mydata4$Hospital.Name[num])}
        }
      
        else if (num=="best") {   
                mydata4 <- mydata[which.min(mydata[,outcome]),]
                print(mydata4$Hospital.Name) }
        
       
        else if (num=="worst") {
                mydata4 <- mydata[which.max(mydata[,outcome]),]
                print(mydata4$Hospital.Name) } 
        
        
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}