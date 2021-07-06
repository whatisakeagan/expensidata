# Load required libraries
## NOTE: If you haven't run the initial setup file, do that first!
library (easycsv)
library (dplyr)
library (tidyr)
library (plyr)
library (scales)
library (stringr)
library (lubridate)

# Set working directory
setwd(easycsv::choose_dir())

# Define custom functions/variables
cleanHTMLtags <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
}

date <- Sys.Date()

## Load CSV files
escalations <- read.csv (file.choose(new = FALSE))
totalescalations <- nrow(escalations)
repeatables <- read.csv (file.choose(new = FALSE))

# Format column names and dates
names(escalations)[1]<-"replyDate"
names(repeatables)[1]<-"eventID"
names(repeatables)[3]<-"response"
escalations$replyDate <- parse_date_time(escalations$replyDate, orders="mdy")
repeatables$Created <- parse_date_time(repeatables$Created, orders="mdy")
escstotal <- str_c("In this period, FRs escalated conversations ", totalescalations, " times.")

# Create combined dataset
aggregated <- left_join(escalations, repeatables, by = "eventID")
aggregated$response.y <- NULL
# Write to a file for categorization
aggcsv <- paste(date, "needscategorization", ".csv", sep="")
write.csv(aggregated, file=aggcsv, row.names = FALSE)
# Read the file in again with categorizations
aggregated <- read.csv (file.choose(new = FALSE))

# How many escalations used a repeatable reply overall?
escreps <- count(aggregated, eventID)
escreps$percent <- escreps$n/(sum(escreps$n))
percreps <- percent(as.numeric(1-escreps[1,3]))
escrepstate <- str_c("Of the total, ", percreps, " of FR escalations were resolved with a repeatable.")

# Okay, so how often was that repeatable actually available to them?
aggregated$timediff <- (aggregated$replyDate - aggregated$Created)/86400
escrepsavail <- count(aggregated, c("eventID", "timediff"))
escrepsavail<-escrepsavail[!(escrepsavail$timediff <= 0),]
escrepsavail <- count(escrepsavail, "eventID")
escrepsavail <- na.omit(escrepsavail)
totalavailescalations <- sum(escrepsavail$freq)
escrepsavail <- group_by(escrepsavail, eventID) %>% mutate(percent = freq/sum(freq))
percavailreps <- percent(as.numeric(totalavailescalations/totalescalations))
escrepstateavail <- str_c("Of the total, ", percavailreps, " of FR escalations were resolved with a repeatable that was available to the FR at the time of escalation.")

# Count categorizations
cats <- count(aggregated, type)
cats <- na.omit(cats)
cats$percent <- cats$n/(sum(cats$n))
cats$percent <- percent(as.numeric(cats$percent))
cats <- cats[rev(order(cats$percent)),]
esccats <- paste(date, "escalationscategorized", ".csv", sep="")
write.csv(cats, file=esccats, row.names = FALSE)

# Write findings to a text file
fileConn<-file("findings.txt")
writeLines(c(escstotal, escrepstate, escrepstateavail), fileConn)
close(fileConn)
