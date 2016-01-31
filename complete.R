## Below is code for Programming Assignment 1: Air Pollution for 
## The Coursera JHU R Programming course

## Part 2

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    nobs <- vector(mode="numeric")
    
    for(index in id) {
        temp_table <- read.csv(sprintf("%s/%03i.csv",directory,index))
        nobs <- c(nobs,sum(complete.cases(temp_table)))
        rm(temp_table)
    }
    data.frame(id,nobs)
}