complete <- function(directory, id=1:332)
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV file
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # Get all files
  filelist <- list.files(directory, full.names = TRUE)
  
  alldata <- data.frame()
  
  for (i in id)
  {
    # Read files corresponding to id monitor
    content <- read.table(filelist[i], header = TRUE, sep=",")
    # Take the sum of complete cases
    nobs <- sum(complete.cases(content))
    
    tmp_dataset <- data.frame(i, nobs)
    alldata <- rbind(alldata, tmp_dataset)
  }

  colnames(alldata)[colnames(alldata) == "i"] <- "id"
  alldata
  print(alldata)
}