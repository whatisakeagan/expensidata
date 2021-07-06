# Load required packages
library(easycsv)
library (stringr)
library(openxlsx)

# Set working directory
setwd(easycsv::choose_dir())

# Define custom functions/variables
cleanHTMLtags <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
}

date <- Sys.Date()

# Load CSV files
repeatables <- read.csv (file.choose(new = FALSE))
repsnoHTML <- read.csv (file.choose(new = FALSE))

# Rename the columns so the rest of this will work
names(repeatables)[1]<-"eventID"
names(repeatables)[2]<-"Details"
names(repsnoHTML)[1]<-"eventID"
names(repsnoHTML)[2]<-"Details"

# Make new dataframes for each type of replacement
## Good to go--keeps HTML, doesn't need any other adjustments (i.e., single word or contained phrase replacements)
g2g <- repeatables

## Needs review--too much potential HTML in the way so we had to remove it, which means we'll need to add formatting back
reformatBreadcrumb <- repsnoHTML
reformatDeleting <- repsnoHTML
reformatDuping <- repsnoHTML
reformatDefault <- repsnoHTML
reformatDownUpgrade <- repsnoHTML
reformatReimbursement <- repsnoHTML
reformatInbox <- repsnoHTML

# Make replacements
g2g$replaced <- str_replace(g2g$Details, "Team", "Collect")
g2g$replaced <- str_replace(g2g$replaced, "Corporate", "Control")
g2g$replaced <- str_replace(g2g$replaced, c("Personal Policy", "Personal policy", "personal policy", "personal Policy"), "Individual policy")
g2g$replaced <- str_replace(g2g$replaced, c("Personal Plan", "Personal plan", "personal plan", "personal Plan"), "Individual plan")
g2g$replaced <- str_replace(g2g$replaced, c("Personal Subscription", "Personal subscription", "personal subscription", "personal Subscription"), "Monthly Subscription")
g2g$replaced <- str_replace(g2g$replaced, c("Company Policy", "Company policy", "company Policy", "company policy"), "Group policy")
g2g$replaced <- str_replace(g2g$replaced, c("Company Plan", "Company plan", "company Plan", "company plan"), "Group plan")
g2g$replaced <- str_replace(g2g$replaced, c("Flex", "flex"), "Pay-per-use")
g2g$replaced <- str_replace(g2g$replaced, "Settings > Policies > [Personal Policy Name]", "Settings > Policies > Individual > [Policy Name]")
g2g$replaced <- str_replace(g2g$replaced, "Settings > Policies > [Company Policy Name]", "Settings > Policies > Group > [Policy Name]")


# Flag breadcrumbs for review
bcmatch <- c("Billing & Subscriptions >", "Billing & Subscriptions>", "Billing &amp; Subscriptions>", "Billing &amp; Subscriptions >", "Billing & Subscriptions", "Billing &amp; Subscriptions")
reformatBreadcrumb$status <- ifelse(grepl(paste(bcmatch, collapse="|"), reformatBreadcrumb$Details, fixed=FALSE), "REVIEW (billing/subs)", "ok")


# Flag deleting policies for review
reformatDeleting$status <- ifelse(grepl("trash can", reformatDeleting$Details, fixed=FALSE), "REVIEW (deleting policies)", "ok")

# Flag duplicating policies for review
reformatDuping$status <- ifelse(grepl('"Duplicate"', reformatDuping$Details, fixed=FALSE), "REVIEW (duping policies)", "ok")

# Flag defaulting for review
defmatch <- c("default policy", "default Policy")
reformatDefault$status <- ifelse(grepl(paste(defmatch, collapse="|"), reformatDefault$Details, fixed=FALSE), "REVIEW (default policies)", "ok")

# Flag up/downgrading policies for review
dgmatch <- c("downgrade", "Downgrade", "upgrade", "Upgrade")
reformatDownUpgrade$status <- ifelse(grepl(paste(dgmatch, collapse="|"), reformatDownUpgrade$Details, fixed=FALSE), "REVIEW (up/downgrading)", "ok")

# Flag failed reimbursements/who to ask for review
reimmatch <- c("reimbursement fails", "reimbursement is managed", "reimbursement are managed", "management and reimbursement", "reinitiate", "re-initiate")
reformatReimbursement$status <- ifelse(grepl(paste(reimmatch, collapse="|"), reformatReimbursement$Details, fixed=FALSE), "REVIEW (reimbursement)", "ok")

# Flag inbox plan selection for review
inboxmatch <- c("Inbox", "task")
reformatInbox$status <- ifelse(grepl(paste(inboxmatch, collapse="|"), reformatInbox$Details, fixed=FALSE), "REVIEW (inboxtask)", "ok")


# Write everything to a single Excel file
require(openxlsx)
list_of_datasets <- list("Replacements" = g2g, 
	"Breadcrumbs" = reformatBreadcrumb,
	"Deleting" = reformatDeleting,
	"Duplicating" = reformatDuping,
	"Defaulting" = reformatDefault,
	"UpDowngrading" = reformatDownUpgrade,
	"Reimbursement" = reformatReimbursement,
	"Inbox" = reformatInbox
)
write.xlsx(list_of_datasets, file = "hardlaunchreplies.xlsx")
