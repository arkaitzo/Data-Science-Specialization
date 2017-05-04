##################################################
##### Author: Arkaitz Etxezarreta   ##############
##### Date:   2017/05/03            ##############
##################################################

##################################################
## Checking my functions before taking the quiz ##
## NOTE: extract "specdata.zip" before starting ##
##################################################

# PART 1
source("pollutantmean.R")
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

# PART 2
source("complete.R")
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

# PART 3
source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)
length(cr)



##################################################
###################### Quiz ######################
##################################################

#1
round(pollutantmean("specdata", "sulfate", 1:10), digits = 3)

#2
round(pollutantmean("specdata", "nitrate", 70:72), digits = 3)

#3
round(pollutantmean("specdata", "sulfate", 34), digits = 3)

#4
round(pollutantmean("specdata", "nitrate"), digits = 3)

#5
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

#6
cc <- complete("specdata", 54)
print(cc$nobs)

#7
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

#8
cr <- corr("specdata")
cr <- sort(cr)
set.seed(868)
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

#9
cr <- corr("specdata", 129)
cr <- sort(cr)
n <- length(cr)
set.seed(197)
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

#10
cr <- corr("specdata", 2000)
n <- length(cr)
cr <- corr("specdata", 1000)
cr <- sort(cr)
print(c(n, round(cr, 4)))
