#Write a function that takes a directory of data files and a threshold for complete cases 
#and calculates the correlation between sulfate and nitrate for monitor locations where the 
#number of completely observed cases (on all variables) is greater than the threshold. 
#The function should return a vector of correlations for the monitors that meet the threshold 
#requirement. If no monitors meet the threshold requirement, then the function should return a 
#numeric vector of length 0. 

source("complete.R")
corr <- function(directory, threshold = 0) {
        corr <- function(fname){
                #put all files together
                data <- read.csv(file.path(directory, fname))
                nobs <- sum(complete.cases(data))
                #loop for threshold
                if(nobs > threshold){
                        return(cor(data$nitrate, data$sulfate, use = "complete.obs"))
                }
        }
        #total correlations with nulls
        totcorrs <- sapply(list.files(directory), corr)
        #total correlations without nulls
        totcorrs <- unlist(totcorrs[!sapply(corr, is.null)])
        #print results
        return(totcorrs)
}


#test1
cr <- corr("specdata", 150)
head(cr)
summary(cr)
