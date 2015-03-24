rankhospital <- function(state, outcome, num = "best") {

        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        ## Check that state and outcome are valid
        ## Assumption: Assume that state is correctly registered in outcome-of-care-measures
        ## There are four PR, DC, VI and GU that are not strictly speaking a state. Ignore this.
        ## Make vector of "state code" (54 elements) from column 7 in data
        stateList <- sort(unique(data[,7]))
        ## Allow lowercase and mixed case for state code
        state <- toupper(state)
        if(! state %in% stateList) {
                stop("invalid state")
        }
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

        ## Return hospital name in that state with the given rank

        ## 30-day death rate
        #work with subset
        data <- data[,c(2,7,ocCol)]
        names(data) <- c("hn", "state", "dRate")
        data <- data[! (is.na(data$dRate) | data$dRate=="Not Available"),]
        ## dRate is character must be converted to numeric else funny things happen in the following
        data$dRate <- as.numeric(data$dRate)
        ## subset on state
        data <- data[data$state == state,]


        ## check num
        ## Set num to 1 for 'best' and to dim(data)[[1]] for worst
        ## stop if num (rank) not whole number
        ## if whole number coerce to integer
        ## stop if neither of above
        if(num == "best") {
                num <- 1
        } else if(num=="worst") {
                num <- dim(data)[[1]]
        } else if(is.numeric(num)) {
                if(abs(num - round(num)) < .Machine$double.eps^0.5) {
                        num <- as.integer(num)
                } else {
                        stop("Rank must be integer.")
                }
        } else {
                stop("Rank can only be 'best', 'worst' or integer.")
        }

        ## check that rank asked for is greater or equal to worst
        #if(num > dim(data)[[1]]) {
        #        stop("Rank asked for is lower than 'worst'.")
        #}

        ## Return hospital name in that state with lowest 30-day death
        ## rate
        data <- data[order(data$dRate,data$hn),]
        ## next line for help with debug
        ## data$rank <- seq(1,length(data$hn))
        data[num,1]
}
