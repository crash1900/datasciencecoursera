pollutantmean <- function(directory, pollutant, id = 1:332)
{
  ## 'directory is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating 
  ## the name of the pulltant for which we will calculate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID number
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  mypattern <- paste(formatC(id, width=3, format="d", flag="0"), collapse="|")
  filelist <- list.files(directory, mypattern, full.names = TRUE)
  alldata <- data.frame()
  
  for (file in filelist)
  {
      tmp_dataset <-read.table(file, header=TRUE, sep=",")
      alldata <- rbind(alldata, tmp_dataset)
      rm(tmp_dataset)
  }

  transform(alldata, sulfate = as.numeric(sulfate))
  transform(alldata, nitrate = as.numeric(nitrate))
  pollutantCol <- alldata[pollutant]
  theMean <- colMeans(pollutantCol, na.rm = TRUE)
  print(theMean)
}