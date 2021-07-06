# Load necessary packages
library (easycsv)
library (plyr)

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

# Choose convo file to import
convos  <- read.csv (file.choose(new = FALSE))
names(convos)[6] <- "user_email"
# Import Intercom data (user-level and bulk convo export)
icinfo <- read.csv (file.choose(new = FALSE))
icinfo[,1] <- NULL
names(icinfo)[1] <- "user_email"
icconvobulk <- read.csv (file.choose(new = FALSE))
names(icconvobulk)[1] <- "conversation_id"
#Remove outbound convos from icconvobulk

# Merge them together by convoID, then email address
convos <- join(convos, icinfo, by='user_email', type='left', match='all')
convos <- join(convos, icconvobulk, by='conversation_id', type='left', match='all')
# Set types of each column so Xcel doesn't poop its pants
convos[,1] <- as.character(convos[,1])
convos[,2] <- as.integer(convos[,2])
convos[,3] <- as.character(convos[,3])
convos[,4] <- as.character(convos[,4])
convos[,5] <- as.integer(convos[,5])
convos[,6] <- as.character(convos[,6])
convos[,7] <- as.character(convos[,7])
convos[,8] <- as.character(convos[,8])
convos[,9] <- as.character(convos[,9])
convos[,10] <- as.character(convos[,10])
convos[,11] <- as.character(convos[,11])
convos[,12] <- as.numeric(convos[,12])
convos[,13] <- as.character(convos[,13])
convos[,14] <- as.numeric(convos[,14])

autoresponders <- convos
ooos <- convos
noreplys <- convos
helpdesks <- convos
marketing <- convos
ouremails <- convos
serviceproviders <- convos
validationcodes <- convos
calinvites <- convos
recfws <- convos

# Indicate auto-responders via subject and/or body contents
arooosubmatch <- c("Auto-Reply", "Auto-response", "automated response", "Auto response", "Automatická odpověď", "Automatic reply", "Automātiskā atbilde", "expensereports@squareup.com", "notification")
autoresponders$status <- ifelse(grepl(paste(arooosubmatch, collapse="|"), autoresponders$subject, fixed=FALSE), "", "remove")
autoresponders <- autoresponders[which(autoresponders$status !='remove'),]

arooobodmatch <- c("away from the office until", "out of the office until")
ooos$status <- ifelse(grepl(paste(arooobodmatch, collapse="|"), ooos$body, fixed=FALSE), "", "remove")
ooos <- ooos[which(ooos$status !='remove'),]

aroooemmatch <- c("noreply", "no-reply", "do_not_reply", "donotreply")
noreplys$status <- ifelse(grepl(paste(aroooemmatch, collapse="|"), noreplys$user_email, fixed=FALSE), "", "remove")
noreplys <- noreplys[which(noreplys$status !='remove'),]

arooohdmatch <- c("helpdesk@", "support@")
helpdesks$status <- ifelse(grepl(paste(arooohdmatch, collapse="|"), helpdesks$user_email, fixed=FALSE), "", "remove")
helpdesks <- helpdesks[which(helpdesks$status !='remove'),]
arooohdmatch2 <- c("Ticket #")
autoresponders$status <- ifelse(grepl(paste(arooohdmatch2, collapse="|"), autoresponders$subject, fixed=FALSE), "", "remove")
autoresponders <- autoresponders[which(autoresponders$status !='remove'),]

# Indicate marketing spam via contents
markbodmatch <- c("mailchimp", "unsubscribe</a>")
marketing$status <- ifelse(grepl(paste(markbodmatch, collapse="|"), marketing$body, fixed=FALSE), "", "remove")
marketing <- marketing[which(marketing$status !='remove'),]

# Indicate us talking to us via from: email addresses
usemailmatch <- c("expensify.com", "help@expensify.com")
ouremails$status <- ifelse(grepl(paste(usemailmatch, collapse="|"), ouremails$user_email, fixed=FALSE), "", "remove")
ouremails <- ouremails[which(ouremails$status !='remove'),]

usspmatch <- c("contact@zapier.com", "donotreply@f6s.com", "noreply@modeanalytics.com")
serviceproviders$status <- ifelse(grepl(paste(usspmatch, collapse="|"), serviceproviders$subject, fixed=FALSE), "", "remove")
serviceproviders <- serviceproviders[which(serviceproviders$status !='remove'),]

usvdmatch <- c("Additional Authentication Provided for NetSuite Login")
validationcodes$status <- ifelse(grepl(paste(usvdmatch, collapse="|"), validationcodes$subject, fixed=FALSE), "", "remove")
validationcodes <- validationcodes[which(validationcodes$status !='remove'),]

uscalsmatch <- c("Accepted: Invitation")
calinvites$status <- ifelse(grepl(paste(uscalsmatch, collapse="|"), calinvites$subject, fixed=FALSE), "", "remove")
calinvites <- calinvites[which(calinvites$status !='remove'),]

# Indicate receipt forwards via subject and/or body contents
recsubmatch <- c("FW", "Fw", ".pdf", "fw")
recfws$status <- ifelse(grepl(paste(recsubmatch, collapse="|"), recfws$subject, fixed=FALSE), "", "remove")
recfws <- recfws[which(recfws$status !='remove'),]

# Clean HTML from messages and subject lines
convos$body <- cleanHTMLtags(convos$body)
convos$body <- cleanHTML(convos$body)

convos$subject <- cleanHTMLtags(convos$subject)
convos$subject <- cleanHTML(convos$subject)

# Write pre-processed data to file
#ppcsv <- paste(date,"preprocessedconvos",".csv",sep="")
#write.csv(convos, file=ppcsv, row.names = FALSE)

# Write everything to a single Excel file
require(openxlsx)
list_of_datasets <- list("Autoresponders" = autoresponders, 
                         "OoO" = ooos, 
                         "NoReply" = noreplys,
                         "HelpDesk" = helpdesks,
                         "MarketingSpam" = marketing,
                         "UsEmailingUs" = ouremails,
                         "Services" = serviceproviders,
                         "ValCodes" = validationcodes,
                         "CalInvites" = calinvites,
                         "Receipts" = recfws
)
ppfile <- paste(date,"preprocessedconvos",".xlsx",sep="")
write.xlsx(list_of_datasets, file = ppfile)
