complete <- function(directory, id = 1:332)  { 
  ## 'directory' is a character vector of length l indicating 
  ## the location of CSV files
  ##
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  ##  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the 
  ## number of complete cases
  
  # Construct path to directory containing monitor files
  dir_path <- file.path(getwd(), directory)
  
  # Initialize data frame to hold monitor data with complete cases
  complete_data <- data.frame(id = integer(), nobs =  integer())
  
  # Get names of all monitor files into a list
  file.names <- dir(dir_path, pattern = ".csv")
  
  # Loop through all monitor files in 'id' and assemble 'complete_data' dataframe
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