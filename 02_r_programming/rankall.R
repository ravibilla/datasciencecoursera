## Ranking hospitals in all states
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available",
                   stringsAsFactors = FALSE)
  
  ## Subset data
  mydata <- data[, c(2, 7, 11, 17, 23)]
  names(mydata) <- c("hospital", "state", "heart_attack", "heart_failure", 
                     "penumonia")
  
  # Split data by state 
  by_state <- split(mydata, mydata$state)
  
  ## Check that state and outcome are valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")

  if(!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  else {
    ## For each state, find the hospital of the given rank
    if(outcome == "heart attack") {
      hospital_names = sapply(by_state, get_hospital_name, 3, num) # heart attack
    }
    else if (outcome == "heart failure") {
      hospital_names = sapply(by_state, get_hospital_name, 4, num)  # heart failure
    }
    else if (outcome == "pneumonia") {
      hospital_names = sapply(by_state, get_hospital_name, 5, num)  # pneumonia
    }
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state 
  ## name
  all_ranked_hospitals <- data.frame(hospital = hospital_names,
                                     state = names(hospital_names))  
  all_ranked_hospitals
  
}

# Helper function to get hospital name
get_hospital_name <- function(data, outcome_col, num) {
  # Order by outcome and hospital
  order_indices <- order(data[, outcome_col], data[, 1])
  ordered_hospitals <- data$hospital[order_indices]
  
  # Calcualte worst index
  worst_index <- dim(data[!is.na(data[, outcome_col]), ])[1]
  
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

# Tests
source("rankall.R")

head(rankall("heart attack", 20), 10)

tail(rankall("pneumonia", "worst"), 3)

tail(rankall("heart failure"), 10)