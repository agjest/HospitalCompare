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



        print(num)

        ## check that rank asked for is greater or equal to worst
        #if(num > dim(data)[[1]]) {
        #        stop("Rank asked for is lower than 'worst'.")
        #}

        ## Return hospital name in that state with lowest 30-day death
        ## rate
        data <- data[order(data$dRate,data$hn),]
        ## next line for help with debug
        ## data$rank <- seq(1,length(data$hn))
        ## split along state, returns list
        data <- split(data,data$state)

        ## check num, This have to be done after the split of data because of worst
        ## Set num to 1 for 'best' and to dim(data)[[1]] for worst
        ## stop if num (rank) not whole number
        ## if whole number coerce to integer
        ## stop if neither of above
        if(num == "best") {
                num <- 1
        } else if(num=="worst") {
                ## Get num of obs for each state. Set to worst for each state
                ## data is now a list of dataframes, each with 3 var, but different length
                ## the inne lapply picks the 3 column in the dataframes (dRate). The outer gets the length
                num <- unlist(lapply(lapply(data,"[[",3),length))
                numc <- cumsum(num)
                #Flatten hospital names
                res <- unlist(lapply(data,"[[",1))
                #pick out the worst from every state
                res <- res[unname(numc)]
                #make nice dataframe to return
                res <- as.data.frame(res)
                res$state <- substr(rownames(res),1,2)
                rownames(res) <- res$state
                names(res)[1] <- "hospital"
                return(res)
        } else if(is.numeric(num)) {
                if(abs(num - round(num)) < .Machine$double.eps^0.5) {
                        num <- as.integer(num)
                } else {
                        stop("Rank must be integer.")
                }
        } else {
                stop("Rank can only be 'best', 'worst' or integer.")
        }


        ## use the [ function to pick element, firste argument passed is rownumber, second is
        ## variable (column) in dataframe in list
        res <- lapply(data,"[",num,1)
        ## make into dataframe
        res <- as.data.frame(t(data.frame(res)))
        names(res) <- "hospital"
        res$state <- rownames(res)
        res
}
