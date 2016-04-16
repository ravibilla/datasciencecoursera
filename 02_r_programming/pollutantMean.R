pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length l indicating 
  ## the location of CSV files
  ##
  ## 'pollutant' is a character vector of length l indicating 
  ## the name of the pollutant for which we will calculate the 
  ## mean; either "sulfate" or "nitrate"
  ##
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  ##
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  # Construct path to directory containing monitor files
  dir_path <- file.path(getwd(), directory)
  
  # Initialize data frame to hold all monitor data 
  monitor_data <- NA

  # Get names of all monitor files into a list
  file.names <- dir(dir_path, pattern = ".csv")

  # Loop through all monitor files in 'id' and assemble 'monitor_data' dataframe
  for(i in id) {
    fpath <- file.path(dir_path, file.names[i])
    file <- read.csv(fpath, header = TRUE)
    monitor_data <- rbind(monitor_data, file)
  }
  
  # Get pollutant data into a vector
  pollutant_data <- monitor_data[[pollutant]]

  # Get a vector of NA data
  bad_data <- is.na(monitor_data[[pollutant]])

  # Calculate mean of valid pollutant data
  pollutant_mean <-  mean(pollutant_data[!bad_data])
  pollutant_mean 

}

# Tests
source("pollutantmean.R")

pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "nitrate", 23)