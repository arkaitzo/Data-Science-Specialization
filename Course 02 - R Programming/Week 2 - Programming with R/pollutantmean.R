##################################################
##### Author: Arkaitz Etxezarreta   ##############
##### Date:   2017/05/03            ##############
##################################################

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return  the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    avg <- NULL
    data <- numeric()
    for(file in id) {
        filename <- paste0(directory, "/", sprintf("%03d", file), ".csv")
        temp_data <- read.csv(filename)
        data <- append(data, temp_data[[pollutant]])
    }
    
    avg <- mean(data, na.rm = TRUE)
    return(avg)
}
