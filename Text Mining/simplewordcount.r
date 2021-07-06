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

# Create a row/token for each word in the response column
tidyrepeats <- responses %>% unnest_tokens(word, response)

# Creates a wordcount dataframe
wordcount <- tidyrepeats %>% count(word, sort = TRUE)

# Remove non-important from wordcount dataframe
notimportantwords <- c("i", "you", "he", "she", "it", "we", "they", "me", "you", 
	"him", "her", "it", "us", "them", "my", "mine", "yours", "yours", "his", 
	"her", "hers", "its", "ours", "ours", "their", "theirs", "the", "to", "a", 
	"and", "can", "this", "if", "on", "in", "for", "of", "be", "that", "is", "will", 
	"have", "are", "or", "as", "from", "any", "an", "by", "me", "with", "it", "not", 
	"let", "at", "into", "out", "when", "you're", "up", "all", "they", "so", 
	"do", "here", "these", "then", "there", "has", "would", "see", "other", "we", 
	"you'll", "which", "them", "about", "how", "once", "i'm", "but", "also", "within", 
	"should", "just", "get", "not", "was", "it's", "don't", "may", "what", "through", 
	"you'd", "i'll", "either", "those", "else", "two", "who", "us", "per", "you've", 
	"doesn't", "isn't", "again", "i'd", "â", "í", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0")

wordcount <- wordcount[grep(paste(notimportantwords, collapse="|"), wordcount$word, invert=TRUE),]

# Write file to directory
wcsv <- paste(date,"wordcount",".csv",sep="")
write.csv(wordcount, file=wcsv, row.names = FALSE)