#1 - # Find the datasharing repo... What time was it created?
rm(list = ls())
library(httr)
# Find OAuth settings for github:
oauth_endpoints("github")
# To make your own application, register it and then...
# ... replace your key and secret below
myapp <- oauth_app("github",
                   key = "your_key",
                   secret = "your_secret")
# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
# Read the data and find the solution
json1 <- content(req)
json2 <- jsonlite::fromJSON(jsonlite::toJSON(json1))
json2[match("jtleek/datasharing", json2$full_name),]$created_at



#2 - Select only the data for the prob weights "pwgtp1" with ages less than 50
#  Use the sqldf package to practice the queries we might send with the...
#  dbSendQuery command in RMySQL.
rm(list = ls())
library(sqldf)
library(data.table)
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv",
    destfile = "ss06pid.csv")
acs <- fread(input = "ss06pid.csv", data.table = TRUE)
head(sqldf("select pwgtp1, AGEP from acs where AGEP < 50"))



#3 - The equivalent function to unique(acs$AGEP)
sqldf("select distinct AGEP from acs")
sort(unique(acs$AGEP))
sort(sqldf("select distinct AGEP from acs")$AGEP)



#4 - How many characters are in the 10th, 20th, 30th and 100th lines of HTML...
#  ... from this page: http://biostat.jhsph.edu/~jleek/contact.html
rm(list = ls())
con <- url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode <- readLines(con)
close(con);
nchar(htmlCode[c(seq(10, 30, by = 10), 100)])



#5 - Read this data set into R and report the sum of the numbers in the 4th...
#  of the 9 cols: https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for
# Hint: this is a fixed width file format
rm(list = ls())
download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for",
    destfile = "wksst8110.for")
library(data.table)
fw_table <- read.fwf(file = "wksst8110.for", widths = c(1,9,5,4,1,3,5,4,1,3,5,4,1,3,5,4,1,3),
                skip = 4)
fw_table <- fw_table[-c(seq(1, 18, by = 2))]
names(fw_table) <- c("Week", "Nino1+2 SST", "Nino1+2 SSTA",
                "Nino3 SST", "Nino3 SSTA",
                "Nino34 SST", "Nino34 SSTA",
                "Nino4 SST", "Nino4 SSTA")
sum(fw_table[, 4])
sum(fw_table$`Nino3 SST`)
