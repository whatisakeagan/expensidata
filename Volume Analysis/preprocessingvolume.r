# Load necessary packages
library (easycsv)


# Set working directory
setwd(easycsv::choose_dir())

# Define custom functions/values
cleanHTMLtags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

cleanHTML <- function(htmlString) {
  return(gsub("&.*?;", "", htmlString))
}
date <- Sys.Date()

# Choose file to import
convos  <- read.csv (file.choose(new = FALSE))


# Indicate auto-responders via subject and/or body contents
arooosubmatch <- c("Auto-Reply", "Ticket #", "Auto-response", "automated response", "Auto response", "Automatická odpověď", "Automatic reply", "Automātiskā atbilde")
convos$auto1 <- ifelse(grepl(paste(arooosubmatch, collapse="|"), convos$subject, fixed=FALSE), "AutoResponderOoO", "")

arooobodmatch <- c("away from the office until", "out of the office until")
convos$auto2 <- ifelse(grepl(paste(arooobodmatch, collapse="|"), convos$body, fixed=FALSE), "AutoResponderOoO", "")

aroooemmatch <- c("noreply", "no-reply", "do_not_reply", "donotreply", "expensereports@squareup.com", "helpdesk@", "notification")
convos$auto3 <- ifelse(grepl(paste(aroooemmatch, collapse="|"), convos$user__email, fixed=FALSE), "AutoResponderOoO", "")

# Indicate marketing spam via contents
markbodmatch <- c("mailchimp", "unsubscribe</a>")
convos$markspam <- ifelse(grepl(paste(markbodmatch, collapse="|"), convos$body, fixed=FALSE), "MarketingSpam", "")

# Indicate us talking to us via from: email addresses
usemailmatch <- c("contact@zapier.com", "expensify.com", "donotreply@f6s.com", "noreply@modeanalytics.com", "electronictransmissionsteam@aexp.com", "help@expensify.com")
convos$usemail1 <- ifelse(grepl(paste(usemailmatch, collapse="|"), convos$user__email, fixed=FALSE), "UsEmailingUs", "")

ussubmatch <- c("Accepted: Invitation", "Additional Authentication Provided for NetSuite Login")
convos$usemail2 <- ifelse(grepl(paste(ussubmatch, collapse="|"), convos$subject, fixed=FALSE), "UsEmailingUs", "")

# Indicate receipt forwards via subject and/or body contents
recsubmatch <- c("FW", "Fw", ".pdf", "fw")
convos$receipt <- ifelse(grepl(paste(recsubmatch, collapse="|"), convos$subject, fixed=FALSE), "Receipt", "")

# Clean HTML from messages and subject lines
convos$body <- cleanHTMLtags(convos$body)
convos$body <- cleanHTML(convos$body)

convos$subject <- cleanHTMLtags(convos$subject)
convos$subject <- cleanHTML(convos$subject)

# Write pre-processed data to file
ppcsv <- paste(date,"preprocessedconvos",".csv",sep="")
write.csv(convos, file=ppcsv, row.names = FALSE)