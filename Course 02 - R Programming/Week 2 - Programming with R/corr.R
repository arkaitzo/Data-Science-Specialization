##################################################
##### Author: Arkaitz Etxezarreta   ##############
##### Date:   2017/05/03            ##############
##################################################

corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    cases <- complete("specdata")$nobs > threshold
    id <- 1:332
    id <- id[cases]
    
    data <- numeric()
    for(file in id) {
        filename <- paste0(directory, "/", sprintf("%03d", file), ".csv")
        temp_data <- read.csv(filename)
        data <- append(data, cor(temp_data$sulfate, temp_data$nitrate, use = "complete.obs"))
    }
    
    return(data)
}
