pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  #setwd(file.path(getwd(), directory)) 
  total = 0                            
  observations = 0                     
  
  for (i in id)
  {
    if (i < 10)
    {
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),  
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    else if (i>=10 & i<100)
    {
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    else
    {
      data <- read.csv(paste(as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    ## getting rid of all the "NA" values
    data = na.omit(data)
    ##  cumulative addition of the complete observations
    observations = observations + nrow(data)
    
    if (pollutant == "sulfate")
    {
      total = total + sum(data$sulfate)
    }
    else {
      total = total + sum(data$nitrate)
    }
    
  }
  
  
  return (total / observations)
  
}



complete <- function(directory, id = 1:332)
{
  dataframe = NULL
  #setwd(file.path(getwd(), directory))
  
  for (i in id)
  {
    if (i < 10)
    {
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),  
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    else if (i>=10 & i<100)
    {
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    else
    {
      data <- read.csv(paste(as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
  
  
  data = na.omit(data)
  data = as.matrix(data)
  dataframe = rbind(dataframe, c(i, nrow(data)))
  }
  
  dataframe = data.frame(dataframe)
  names(dataframe) = c('ID','NOBS')
  return(dataframe)
}

corrfunc <- function(directory, threshold = 0)
{
  #setwd(file.path(getwd(),directory))
  
  #initialising the correlation vector
  corr_vec = NULL
  
  #correcting the file names as before
  for (i in 1:322)
  {
    if (i < 10)
    {
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),  
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    else if (i>=10 & i<100)
    {
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    else
    {
      data <- read.csv(paste(as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    
    #removing na values
    data = na.omit(data)
    
    if (nrow(data) > threshold)
    {
      corr_vec = c(corr_vec, cor(data[,2],data[,3]))
    }
  }
  return(corr_vec)
}

