#1 - What are the first 3 values that result?
rm(list = ls())
if(!file.exists("./data")) {dir.create("./data")}
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv",
    destfile = "./data/ss06hid.csv", method = "curl")
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf",
    destfile = "./data/PUMSDataDict06.pdf", method = "curl")
dateDownloaded <- date()
data <- read.csv("./data/ss06hid.csv")
# Households on >10 acres who sold $10,000+ worth of agriculture products
# Assign that logical vector to the variable agricultureLogical
agricultureLogical <- ifelse(data$ACR ==3 & data$AGS == 6, TRUE, FALSE)
# Identify the rows of the data frame where the logical vector is TRUE
head(which(agricultureLogical), n = 3)



#2 - What are the 30th and 80th quantiles of the resulting data?
rm(list = ls())
library(jpeg)
if(!file.exists("./data")) {dir.create("./data")}
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg",
    destfile = "./data/jeff.jpg", method = "curl")
dateDownloaded <- date()
data <- readJPEG("./data/jeff.jpg", native = TRUE)
str(data)
quantile(x = data, probs = c(.3, .8))



#3 - How many of the IDs match?
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
# How many of the IDs match?
nrow(mergedData)
# Sort the data frame in descending order by GDP rank (so US is last)
library(dplyr)
mergedData$ranking <- as.integer(as.character(mergedData$ranking))
mergedData <- arrange(mergedData, desc(ranking))
# What is the 13th country in the resulting data frame?
mergedData$Short.Name[[13]]



#4 - What is the average GDP ranking for the "High income: OECD"...
#  ... and "High income: nonOECD" group?
sapply(split(x = mergedData$ranking, f = mergedData$Income.Group), mean)
tapply(mergedData$ranking, mergedData$Income.Group, mean)



#5 - Cut the GDP ranking into 5 separate quantile groups
library(Hmisc)
mergedData$rankingGroups <- cut2(x = mergedData$ranking, g = 5)
# Make a table versus "Income.Group". How many countries are...
# ... "Lower middle income" but among the 38 nations with highest GDP?
table(mergedData$rankingGroups , mergedData$Income.Group) # 5
