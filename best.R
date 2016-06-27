## best.R
## Function reads through the csv file into data variable, since we know all the data in my csv is of type "character"
## it saves some analyhsis time for R if specified upfront, hence the colClasses are defined. Similarly
## na.strings in R is used to set the columns that represent the missing values, we want to account for all hence the 
## value there is "Not Available"
best <- function(state, outcome) {
        
        ## Read outcome data
        data <- read.csv("hospitalQuality/outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
        
        ## Check that state and outcome are valid
	## samples from heart attach, heart failure and pneumonia are considered for analysis,hence 
	## marked as validOutcome
        validOutcome = c("heart attack","heart failure","pneumonia")
        if (!outcome %in% validOutcome) { stop("invalid outcome")}
        
        validState = unique(data[,7])
        if (!state %in% validState) stop("invalid state")
        
        ## convert outcome name into column name
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        colName <- fullColName[match(outcome,validOutcome)]
        
        ## Return hospital name in that state with lowest 30-day death rate
        data.state <- data[data$State==state,]
        idx <- which.min(as.double(data.state[,colName]))
        data.state[idx,"Hospital.Name"]
}
