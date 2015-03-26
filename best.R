best <- function(state, outcome) {

        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

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

        #work with subset
        data <- data[,c(2,7,ocCol)]
        names(data) <- c("hn", "state", "dRate")
        data <- data[! (is.na(data$dRate) | data$dRate=="Not Available"),]
        ## dRate is character must be converted to numeric else funny things happen in the following
        data$dRate <- as.numeric(data$dRate)
        ## subset on state
        data <- data[data$state == state,]
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        sort(data[data$dRate == min(as.numeric(data$dRate)), 1])[1]
}
