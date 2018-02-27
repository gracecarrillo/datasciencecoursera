#Write a function that reads a directory full of files and reports the number of completely 
#observed cases in each data file. The function should return a data frame where the first 
#column is the name of the file and the second column is the number of complete cases. 

complete <- function(directory, id = 1:332) {
        #get directory path
        directory <- paste(getwd(),"/", directory, "/", sep = "")
        #get files names
        folders <- list.files(directory)
        ids <- vector()
        nobs <- vector()
        #for each id loop
        for(idnumber in id){
                #paste data together
                data1 <- paste(directory, folders[idnumber], sep = "")
                ascsv <- read.csv(data1)
                #accumulate values on vectors
                ids <- c(ids, idnumber)
                nobs <- c(nobs, sum(complete.cases(ascsv)))
        }
        #return dataframe
        data.frame(ids = ids, nobs = nobs)
}

#test1
complete("specdata", 1)
#test2
complete("specdata", c(2, 4, 8, 10, 12))
#test3
complete("specdata", 30:25)
#test4
complete("specdata", 3)
