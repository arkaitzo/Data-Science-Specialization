##################################################
##### Author: Arkaitz Etxezarreta   ##############
##### Date:   2017/05/03            ##############
##################################################

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id   nobs
    ## 1    117
    ## 2    1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    col1 <- NULL
    col2 <- NULL
    df <- data.frame()
    for(file in id) {
        filename <- paste0(directory, "/", sprintf("%03d", file), ".csv")
        temp_data <- read.csv(filename)
        col1 <- append(col1, file)
        col2 <- append(col2, sum(complete.cases(temp_data)))
    }
    
    df <- data.frame(col1, col2)
    names(df) <- c("id","nobs")
    return(df)
}
