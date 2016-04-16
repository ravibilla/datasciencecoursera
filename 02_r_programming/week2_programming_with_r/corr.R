corr <- function(directory, threshold=0)  {
  
  # Construct path to directory with monitor files
  dir_path <- file.path(getwd(), directory)
  
  # Get names of all monitor files into a list
  file.names <- dir(dir_path, pattern = ".csv")
  
  # Get a dataframe of monitor id's with compelte cases
  complete_df <- complete(directory)
  nobs <- complete_df$nobs
  ids <- complete_df$id[nobs > threshold]

  # Initialize correlation vector
  corr_vector <- rep(0, length(ids))
  
  # Loop through the monitor files in "ids"
  for(i in ids) {
    fpath <- file.path(dir_path, file.names[i])
    file <- read.csv(fpath, header = TRUE)
    corr_vector[i] <- cor(file$sulfate, file$nitrate)
  }

  corr_vector
}

# Tests
source("corr.R")
source("complete.R")

cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)







