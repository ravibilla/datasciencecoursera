pollutantmean <- function(directory, pollutant, id = 1:332) { 
  
  # Construct path to directory with monitor files
  dir_path <- file.path(getwd(), directory)
  
  # Initialize data frame to hold all monitor data 
  monitor_data <- NA

  # Get file names of monitors
  file.names <- dir(dir_path, pattern = ".csv")

  # Loop through the monitor files in "id" and assemble the monitor_data dataframe
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
