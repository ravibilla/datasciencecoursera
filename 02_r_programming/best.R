## Finding the best hospital in a state
best <- function(state, outcome) {
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
      hospital_name = get_hospital_name(mydata, state, 3) # heart attack
    }
    else if (outcome == "heart failure") {
      hospital_name = get_hospital_name(mydata, state, 4) # heart failure
    }
    else if (outcome == "pneumonia") {
      hospital_name = get_hospital_name(mydata, state, 5) # pneumonia
    }
  }
 
  best_hospital <- hospital_name  
  best_hospital
  
}

# Helper function to get hospital name
get_hospital_name <- function(mydata, state, outcome_col) {
  # Subset hospital data
  state_subset <- mydata[mydata[, "state"] == state, ]
  
  # Order by outcome and hospital
  order_indices <- order(state_subset[, outcome_col], state_subset[, 1])
  ordered_hospitals <- state_subset$hospital[order_indices]
  ordered_hospitals[1]
}

#Tests
#source("best.R")

best("TX", "heart attack")

best("TX", "heart failure")

best("MD", "heart attack")

best("MD", "pneumonia")

best("BB", "heart attack")

best("NY", "heart attack")