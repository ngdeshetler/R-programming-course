## Below is code for Programming Assignment 1: Air Pollution for 
## The Coursera JHU R Programming course

## Part 3

corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    source("complete.R")
    
    corr_list=vector(mode="numeric")
    comp_cases <- complete(directory)
    for(i in 1:nrow(comp_cases)){
        if(comp_cases$nobs[i]>=threshold){
            temp_table <- read.csv(sprintf("%s/%03i.csv",directory,comp_cases$id[i]))
            corr_list <- c(corr_list,cor(temp_table$sulfate,y=temp_table$nitrate,use = "na.or.complete"))
        }
    }
    corr_list
}