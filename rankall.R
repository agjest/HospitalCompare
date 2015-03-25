rankall <- function(outcome, num = "best") {

        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name

        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        ## Check that state and outcome are valid
        ## Assumption: Assume that state is correctly registered in outcome-of-care-measures
        ## There are four PR, DC, VI and GU that are not strictly speaking a state. Ignore this.
        ## Make vector of "state code" (54 elements) from column 7 in data
        stateList <- sort(unique(data[,7]))
        ocList <- c("heart attack", "heart failure", "pneumonia")
        ## Allow Uppercase and mixed case for outcome
        outcome <- tolower(outcome)
        if(! outcome %in% ocList) {
                stop("invalid outcome")
        }

        if(outcome==ocList[1]) {
                ocCol <- 11
        } else if(outcome==ocList[2]) {
                ocCol <- 17
        } else if(outcome==ocList[3]) {
                ocCol <- 23
        } else {
                stop("This should not happen.")
        }

        ## 30-day death rate
        #work with subset
        data <- data[,c(2,7,ocCol)]
        names(data) <- c("hn", "state", "dRate")
        data <- data[! (is.na(data$dRate) | data$dRate=="Not Available"),]
        ## dRate is character. Must be converted to numeric else funny things happen in the following
        data$dRate <- as.numeric(data$dRate)

        data <- data[order(data$dRate,data$hn),]
        ## next line for help with debug
        ## data$rank <- seq(1,length(data$hn))
        ## split along state, returns list
        data <- split(data,data$state)

        ## Find numobs for every state
        numobs <- unlist(lapply(lapply(data,"[[",3),length))
        ## Find cumsum of numobs
        numobsCum <- cumsum(numobs)

        ## check num, This have to be done after the split of data because of worst
        ## Set num to 1 for 'best' and to dim(data)[[1]] for worst
        ## stop if num (rank) not whole number
        ## if whole number coerce to integer
        ## stop if neither of above
        ## num is really rank
        if(num == "best") {
                ## num is a constant
                num <- 1
        } else if(num=="worst") {
        ## data is now a list of dataframes. The dataframes differ in length (different number
        ## of hospitals in different sates). num is now a named vector
                num <- unlist(lapply(lapply(data,"[[",3),length))
        } else if(is.numeric(num)) {
                if(abs(num - round(num)) < .Machine$double.eps^0.5) {
                        num <- as.integer(num)
                        if(num <= 0) {
                                stop("Only positive rank allowed.")
                        }
                        }
                else {
                        stop("Rank must be integer.")
                }
        } else {
                stop("Rank can only be 'best', 'worst' or integer.")
        }
        # only positive rank has meaning

        #Flatten hospital names
        res <- unlist(lapply(data,"[[",1))
        ## index of the one we want. pmax adjust for cases where rank (i.e. num) is greater than number of
        ## hospitals in state
        ind <-  numobsCum - pmax((numobs-num),0)
        #pick out the ones we want. If rank > num hospitals we give NA
        res <- ifelse(pmax((numobs-num),-1) < 0, NA, res[ind])
        #make nice dataframe to return
        res <- as.data.frame(res)
        res$state <- substr(rownames(res),1,2)
        rownames(res) <- res$state
        names(res)[1] <- "hospital"
        res
}
