getwd()
setwd("C:/Users/graci/OneDrive/Data_Science_Specialisation/Programming_Assignment1")
getwd()
#Assignment1 Data Science Sepcialisation, R Programming 27-02-18 - Graciela Carrillo

#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) 
#across a specified list of monitors. The function 'pollutantmean' takes three arguments: 
#'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' 
#'reads that monitors' particulate matter data from the directory specified in the 'directory' argument and 
#'returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. 
#'


pollutantmean <- function(directory, pollutant, id = 1:332) {
        #readfiles
        csvfiles <- dir(directory, "*\\.csv$", full.names = TRUE)
        data <- lapply(csvfiles[id], read.csv)
        #aux variables
        numDataPoints <- 0L
        total <- 0L
        #loop for each id
        for (filedata in data) {
                d <- filedata[[pollutant]] # relevant column data
                d <- d[complete.cases(d)] # remove NA values
                numDataPoints <- numDataPoints + length(d)
                total <- total + sum(d)
        }
        total / numDataPoints
}


#test1
pollutantmean2("specdata", "sulfate", 1:10)
#test2
pollutantmean("specdata", "nitrate", 70:72)
#test3
pollutantmean("specdata", "sulfate", 34)
