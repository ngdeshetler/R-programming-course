## Below is code for Programming Assignment 1: Air Pollution for 
## The Coursera JHU R Programming course

## Part 1

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    poll_list <- vector(mode="numeric")
    
    for(index in id) {
        temp_table <- read.csv(sprintf("%s/%03i.csv",directory,index))
        poll_list <- c(poll_list,temp_table[[pollutant]])
        rm(temp_table)
    }
    
    mean(poll_list,na.rm = T)
}
