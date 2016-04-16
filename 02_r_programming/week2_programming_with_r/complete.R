complete <- function(directory, id = 1:332)  { 
  
  # Construct path to directory with monitor files
  dir_path <- file.path(getwd(), directory)
  
  # Initialize data frame to hold monitor data with complete cases
  complete_data <- data.frame(id = integer(), nobs =  integer())
  
  # Get names of all monitor files into a list
  file.names <- dir(dir_path, pattern = ".csv")
  
  # Loop through the monitor files in "id"
  for(i in id) {
    fpath <- file.path(dir_path, file.names[i])
    file <- read.csv(fpath, header = TRUE)
    complete_data[nrow(complete_data) + 1, ] <- c(i, sum(complete.cases(file)))
  }
  
  complete_data
}

# Tests
source("complete.R")

complete("specdata", 1)

complete("specdata", c(2, 4, 8, 10, 12))

complete("specdata", 3)