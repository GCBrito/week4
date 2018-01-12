rankall <- function(outcome, num = "best") {
      
        # 1. load data and produce a data.frame with useful columns 
                data <- read.csv("outcome-of-care-measures.csv", sep=",", na.strings="Not Available", stringsAsFactors=FALSE)
                mydata <- data[,c(2,7,11,17,23)]
                colnames(mydata) <- c("hospital","state","heart attack","heart failure","pneumonia")
        
        # 2. Check whether outcome is valid
                validout<- outcome %in% c("pneumonia","heart attack", "heart failure")
                if(validout==0){stop("invalid outcome")}
        
        # 3. Remove hospitals with NA in outcome 
        if      (as.character(outcome)=="heart attack"){
                mydata2 <- mydata[complete.cases(mydata$'heart attack'),] }
        
        else if (as.character(outcome)=="heart failure"){
                mydata2 <- mydata[complete.cases(mydata$'heart failure'),] }
        
        else  
                mydata2 <- mydata[complete.cases(mydata$'pneumonia'),] 
        
        # 4. Creates a factor with levels for every state, a data frame for storing results, and a vector with states abbreviations
                mydata6 <- as.data.frame(matrix(NA,ncol=2,nrow=54))
                colnames(mydata6) <- c("hospital","state")
                statesn <- factor(mydata2$state)
                stsn<- levels(statesn)
        
        # 5. Order data by state, then by outcome ranking and finally alphabetically  
                mydata3 <-  mydata2[order(mydata2[,2],mydata2[,outcome],mydata2[,1]),]
        
        # 6. Split data according to state
                mydata4 <- split(mydata3,mydata3$state)
                
        # 7. Produce a data.frame with the requested rank hospital which has a row for each hospital in that ranking for each state    
        if      (class(num)=="numeric"){
                for(i in 1:54) {   mm<- data.frame(mydata4[i])
        
                          mydata6[i,] <- mm[num,c(1)]}}
        
        else if (num=="best") {
                for(i in 1:54) {   mm<- data.frame(mydata4[i])
        
                          mydata6[i,] <- mm[1,c(1)]}}
                
        else if (num=="worst") {
                for(i in 1:54) {   mm<- data.frame(mydata4[i])
                
                          mydata6[i,] <- mm[nrow(mm),c(1)]}}
                
        # 8. Assigns names to the data.frame rows and then returns the final data.frame
                
        rownames(mydata6) <- stsn
        mydata6[2] <- stsn 
        mydata6
       
}
