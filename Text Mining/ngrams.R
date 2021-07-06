# Load required libraries
## NOTE: If you haven't run the initial setup file, do that first!
###### YOU MUST HAVE THE MOST UP TO DATE VERSION OF JAVA INSTALLED!!!
library (easycsv)
library (tidytext)
library (dplyr)
library (janeaustenr)

# Define custom functions
cleanHTMLtags <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
}

cleanHTML <- function(htmlString) {
    return(gsub("&.*?;", "", htmlString))
}

date <- Sys.Date()

# Set working directory
setwd(easycsv::choose_dir())

# Load file containing all current replies
responses <- read.csv (file.choose(new = FALSE))

# Rename the columns so the rest of this will work
names(responses)[1]<-"eventID"
names(responses)[2]<-"response"

# Remove HTML from response column
responses$response <- cleanHTMLtags(responses$response)
responses$response <- cleanHTML(responses$response)

# Pull out most common 2-word phrases
doswords <- responses %>% unnest_tokens(bigram, response, token = "ngrams", n = 2)

dosphrases <- doswords %>% count(bigram, sort = TRUE)

# Write file to directory
dpsv <- paste(date,"dosphrases",".csv",sep="")
write.csv(dosphrases, file=dpsv, row.names = FALSE)

# Pull out most common 3-word phrases
treswords <- responses %>% unnest_tokens(bigram, response, token = "ngrams", n = 3)

tresphrases <- treswords %>% count(bigram, sort = TRUE)

# Write file to directory
tpsv <- paste(date,"tresphrases",".csv",sep="")
write.csv(tresphrases, file=tpsv, row.names = FALSE)

# Pull out most common 4-word phrases
cuatrowords <- responses %>% unnest_tokens(bigram, response, token = "ngrams", n = 4)

cuatrophrases <- cuatrowords %>% count(bigram, sort = TRUE)

# Write file to directory
cupsv <- paste(date,"cuatrophrases",".csv",sep="")
write.csv(cuatrophrases, file=cupsv, row.names = FALSE)

# Pull out most common 5-word phrases
cincowords <- responses %>% unnest_tokens(bigram, response, token = "ngrams", n = 5)

cincophrases <- cincowords %>% count(bigram, sort = TRUE)

# Write file to directory
cipcsv <- paste(date,"cincophrases",".csv",sep="")
write.csv(cincophrases, file=cipcsv, row.names = FALSE)
