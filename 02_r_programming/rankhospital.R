## Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available",
                   stringsAsFactors = FALSE)
  
  ## Subset data
  mydata <- data[, c(2, 7, 11, 17, 23)]
  names(mydata) <- c("hospital", "state", "heart_attack", "heart_failure", 
                     "penumonia")
  
  ## Check that state and outcome are valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!state %in% mydata$state) {
    stop("invalid state")
  }
  else if(!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  else {
    ## Return hospital name in that state with lowest 30-day death rate 
    if(outcome == "heart attack") {
      hospital_name = get_hospital_name(mydata, state, 3, num) # heart attack
    }
    else if (outcome == "heart failure") {
      hospital_name = get_hospital_name(mydata, state, 4, num) # heart failure
    }
    else if (outcome == "pneumonia") {
      hospital_name = get_hospital_name(mydata, state, 5, num) # pneumonia
    }
  }
  
  ranked_hospital <- hospital_name  
  ranked_hospital

}

# Helper function to get hospital name
get_hospital_name <- function(mydata, state, outcome_col, num) {
  # Subset hospital data
  state_subset <- mydata[mydata[, "state"] == state, ]
  
  # Order by outcome and hospital
  order_indices <- order(state_subset[, outcome_col], state_subset[, 1])
  ordered_hospitals <- state_subset$hospital[order_indices]
  
  # Calcualte worst index
  worst_index <- dim(state_subset[!is.na(state_subset[, outcome_col]), ])[1]
  
  # Get ranked hospital
  if(num == "best") {
    ordered_hospitals[1] # First rank
  }
  else if(num == "worst") {
    ordered_hospitals[worst_index] # Last rank
  }
  else if(num > length(ordered_hospitals)) {
    return(NA) # Invalid rank
  }
  else {
    ordered_hospitals[num] # Rank = num
  }
  
}

#Tests
source("rankhospital.R")

rankhospital("TX", "heart failure", 4)

rankhospital("MD", "heart attack", "worst")

rankhospital("MN", "heart attack", 5000)