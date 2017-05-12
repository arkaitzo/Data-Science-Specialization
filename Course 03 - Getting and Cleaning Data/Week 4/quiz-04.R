#1 - What is the value of the 123 element of the resulting list?
rm(list = ls())
if(!file.exists("./data")) {dir.create("./data")}
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv",
    destfile = ".data/ss06hid.csv", method = "curl")
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf",
    destfile = "./data/PUMSDataDict06.pdf", method = "curl")
dateDownloaded <- date()
data <- read.csv("./data/ss06hid.csv")
# Split all the names of the data frame on the characters "wgtp".
splitNames <- strsplit(x = names(data), split = "wgtp")
# What is the value of the 123rd element of the resulting list?
names(data)[[123]]
splitNames[[123]]



#2 - # What is the average GDP?
rm(list = ls())
# Load the Gross Domestic Product data for the 190 ranked countries...
if(!file.exists("./data")) {dir.create("./data")}
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",
    destfile = "./data/GDP.csv", method = "curl")
dateDownloaded <- date()
gdp <- read.csv(file = "./data/GDP.csv", header = FALSE, skip = 5, na.strings = "..")
gdp <- gdp[c(1:190), -c(3,6:10)]
names(gdp) <- c("country", "ranking", "economy", "USD")
# Remove the commas from the GDP numbers in millions of dollars
gdp$USD <- as.integer(gsub(pattern = ",", replacement = "", x = gdp$USD))
# What is the average?
mean(gdp$USD)



#3 - Count the number of countries ("countryNames.") whose name...
#  begins with "United"? # How many countries begin with United?
length(grep("^United", gdp$economy))



#4 - How many end in June?
rm(list = ls())
# Load the Gross Domestic Product data for the 190 ranked countries...
if(!file.exists("./data")) {dir.create("./data")}
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",
    destfile = "./data/GDP.csv", method = "curl")
# ... and the educational data from this data set:
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv",
    destfile = "./data/EDSTATS_Country.csv", method = "curl")
dateDownloaded <- date()
# Match the data based on the country shortcode
gdp <- read.csv(file = "./data/GDP.csv", header = FALSE, skip = 5, na.strings = "..")
summary(gdp)
gdp <- gdp[c(1:190), -c(3,6:10)]
names(gdp) <- c("country", "ranking", "economy", "USD")

edu <- read.csv(file = "./data/EDSTATS_Country.csv", header = TRUE)

mergedData <- merge(x = gdp, y = edu,
                    by.x = "country", by.y = "CountryCode",
                    all = FALSE) # FALSE = they must appear in both data sets
# Of the countries for which the end of the fiscal year is available...
# ... how many end in June?
sum(grepl("^[Ff]iscal year end: [Jj][Uu][Nn][Ee]", mergedData$Special.Notes))



#5 - Dwnld data on Amazon's stock price and get the times the data was sampled
rm(list = ls())
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
# How many values were collected in 2012?
days2012 <- grepl("2012-", sampleTimes)
sum(days2012)
# How many values were collected on Mondays in 2012?
library(lubridate)
sum(wday(sampleTimes[days2012]) == 2)
sum(wday(sampleTimes[days2012], label = TRUE) == "Mon")
