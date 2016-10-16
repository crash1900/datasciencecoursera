corr <- function(directory, threshold = 0)
{
  ## 'directory is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  filelist <- list.files(directory, full.names = TRUE)
  
  alldata <- data.frame()
  correlatedData <- numeric()
  
  for (file in filelist)
  {
    # Read files
    content <- read.table(file, header = TRUE, sep=",")
    
    # Filter sulfate and nitrate columns in content
    sulfateContent <- !is.na(content$sulfate)
    nitrateContent <- !is.na(content$nitrate)
    
    # Check that nitrate and sulfate exceed threshold
    nobs <- sum(sulfateContent & nitrateContent)
    if (nobs > threshold) 
    {
      # Correlate where both sulfate and nitrate are not NA
      nitrateDataset <- content[which(!is.na(content$nitrate)), ]
      sulfateAndNitrateDataset <- nitrateDataset[which(!is.na(nitrateDataset$sulfate)), ]
      correlatedData <- c(correlatedData, cor(sulfateAndNitrateDataset$nitrate, sulfateAndNitrateDataset$sulfate))
    }
  }
  correlatedData
}