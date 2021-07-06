### PREP
# Load required packages
library (stringr)
library(easycsv)
# Set working directory
setwd(easycsv::choose_dir())
# Define date (for naming later)
date <- Sys.Date()
# Read in file
repeatables <- read.csv (file.choose(new = FALSE))
# Set up link column
repeatables$link <- NA
###################################
### DO WORK
### Extract URLs
# Define pattern
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+#{}]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

# Extract URLs
repeatables$link <- str_extract_all(repeatables$Details, url_pattern, simplify=TRUE)

#Convert into matrix so links will go into separate columns
repeatables <- as.matrix(repeatables)

###################################
### FINISHING
# Define filename
linkcsv <- paste(date,"linksout",".csv",sep="")
write.csv(repeatables, file=linkcsv, row.names = FALSE)