#1 - How many properties are worth $1,000,000 or more?
rm(list = ls())
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv",
    destfile = "ss06hid.csv")
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf",
    destfile = "PUMSDataDict06.pdf")
dateDownloaded <- date()
data <- read.csv("ss06hid.csv")
# How many properties are worth $1,000,000 or more
hist(as.numeric(data$VAL), main = "Property price distribution",
     xlab = "Code for property value")
table(data$VAL == 24) # Property value $1000000+



#3 - Read rows 18-23 and columns 7-15 and execute the sum...
rm(list = ls())
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx",
    destfile = "DATA.gov_NGAP.xlsx")
dateDownloaded <- date()

library(openxlsx)
dat <- read.xlsx("DATA.gov_NGAP.xlsx", sheet = 1,
                 rows = 18:23, cols = 7:15)
sum(dat$Zip * dat$Ext, na.rm = TRUE)



#4 - How many restaurants have zipcode 21231?
rm(list = ls())
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata/data/restaurants.xml",
    destfile = "restaurants.xml")
dateDownloaded <- date()

library(XML)
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata/data/restaurants.xml"
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)
table(xpathSApply(rootNode, "//zipcode", xmlValue) == 21231)

#5
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv",
    destfile = "ss06pid.csv")
rm(list = ls())
library(data.table)
DT <- fread(input = "ss06pid.csv", data.table = TRUE)

rowMeans(DT)[DT$SEX == 1]
rowMeans(DT)[DT$SEX == 2]

system.time(sapply(split(DT$pwgtp15, DT$SEX), mean))
system.time(DT[,mean(pwgtp15),by = SEX])
system.time(mean(DT$pwgtp15, by = DT$SEX))

system.time(mean(DT[DT$SEX==1,]$pwgtp15))
system.time(mean(DT[DT$SEX==2,]$pwgtp15))

system.time(tapply(DT$pwgtp15, DT$SEX, mean))
