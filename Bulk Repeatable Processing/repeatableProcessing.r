# Load required libraries
## NOTE: If you haven't run the initial setup file, do that first!
###### YOU MUST HAVE THE MOST UP TO DATE VERSION OF JAVA INSTALLED!!!
library (qdap)
library (notifier)
library (textclean)
library (easycsv)
library (tidytext)
library (dplyr)
library (stringr)

# Set working directory
setwd(easycsv::choose_dir())

# Notify user to ensure they're importing a CSV (i.e., the original file should have been converted)
notify(title = "BE CAREFUL!", msg = "Make sure you're importing a CSV! You may need to convert it manually first.")

# Define custom functions
cleanHTMLtags <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
}

date <- Sys.Date()

# Load file containing all current replies
repeatables <- read.csv (file.choose(new = FALSE))


# Rename the columns so the rest of this will work
names(repeatables)[1]<-"eventID"
names(repeatables)[2]<-"Details"

# Create a separate dataframe with HTML stripped out
repsnohtml <- repeatables
repsnohtml$noHTML <- cleanHTMLtags(repsnohtml$Details)
repsnohtml$noHTML <- str_replace(repsnohtml$noHTML, "$nbsp;", " ")

# Create a column indicating whether or not the reply contains a question
repsnohtml$question <- ifelse(grepl("\\?", repsnohtml$Details, fixed=FALSE), "REVIEW", "ok")


# Create a column indicating whether or not the reply uses we/us/our/ours or other plural pronouns
## Make sure to keep the spaces around the words; otherwise a string like "Wells Fargo" will read a false positive
plupromatch <- c(" we ", " us ", " our ", " ours ")
repsnohtml$plupro <- ifelse(grepl(paste(plupromatch, collapse="|"), repsnohtml$Details, fixed=FALSE), "REVIEW", "ok")


# Create a column indicating whether Concierge might have been referred to in the 3rd person
thirdpersmatch <- c("Concierge", "Concierge's")
repsnohtml$thirdpers <- ifelse(grepl(paste(thirdpersmatch, collapse="|"), repsnohtml$Details, fixed=FALSE), "REVIEW", "ok")

# Create a column indicating potentially wonky HTML, part 1 (older replies only; ol+li, )
maybejankyhtmlmatch <- c('div data-block="true" data-editor="', "data-offset-key")
repeatables$maybejankyhtml <- ifelse(grepl(paste(maybejankyhtmlmatch, collapse="|"), repeatables$Details, fixed=FALSE), "REVIEW", "ok")


# Create a column indicating non-breaking spaces 
repeatables$nbsp <- ifelse(grepl("&nbsp;", repeatables$Details, fixed=FALSE), "REVIEW", "ok")


# Create a column indicating decoded HTML
decodedhtmlmatch <- c("&exclamation;", "&quot;", "&percent;", "&amp;", "&apos;", "&add;", "&lt;", "&equal;", "&gt;", "&pound;", "&copy;", "&reg;")
repeatables$decodedhtml <- ifelse(grepl(paste(maybejankyhtmlmatch, collapse="|"), repeatables$Details, fixed=FALSE), "REVIEW", "ok")


# Create a column indicating breadcrumb trails
breadcrumbmatch <- c(" > ", "->", "->")
repsnohtml$breadcrumbs <- ifelse(grepl(paste(breadcrumbmatch, collapse="|"), repsnohtml$Details, fixed=FALSE), "REVIEW", "ok")


# Spelling check
spellcheck <- as.data.frame(check_spelling(repsnohtml$Details, assume.first.correct=FALSE)) 
spellcheck <- spellcheck[order(spellcheck[,'not.found']),]
spellcheck <- spellcheck[!duplicated(spellcheck$not.found),]

## Remove Expensify-specific words
expensispell <- c("expensify", "fargo", "accountsafe", "ach", "ack", "adminstrators", "aexp", 
	"amazonses", "americanexpress", "amex", "amex's", "analytics", "anz", "api", "aud", "australians", 
	"autofy", "azul", "backend", "barclays", "blindside", "calendly", "cardholder", "cardholder's", "cc", 
	"cdf", "ceo", "checkbox", "checkboxes", "checkmark", "citi", "clearbit", "comerica", "concierge", 
	"connectivity", "contactinfo", "copiloted", "copiloting", "customization", "deactivated", "deactivating", 
	"deliverability", "deliverable", "deprovision", "deprovisioning", "desktop", "diem", "dns", "docx", "domainname", 
	 "doublecheck", "download", "downloadable", "downloaded", "downloading", "downloads", "dpa", "dpas", "dropdown", 
	"dynamically", "easyweb", "egencia", "electronictransmissionsteam", "email", "emailed", "emailing", 
	"emails", "encryption", "ereceipt", "ereceipts", "essentials", "europe", "expensierror", "faq", "faqs", 
	"ffa", "filename", "financialforce", "financials", "firefox", "firewall", "freelancers", "functionality", 
	"gdpr", "glitchy", "gmail", "googleplay", "gotowebinar", "guesswork", "hipaa", "hq", "hr", "href", "hsbc", 
	"http", "https", "hyperlink", "iban", "ifttt", "inbox", "inboxes", "infomercial", "intacct", "integrations", 
	"internet", "ios", "irs", "isp", "itineraries", "itunes", "javascript", "jpg", "jpmorgan", "lefthand", 
	"lloyds", "login", "logins", "lyft", "makeover", "mastercard", "mastercard's", "mastercards", "mca", "merchantname", 
	"messaging", "microsoft", "mimecast", "misc", "misunderstood", "mondays", "myob", "nbsp", "netsuite", "netsuite's", 
	"newcompany", "newdomain", "nextravel", "ocl", "ocr", "offline", "ofx", "oiicpdkmeclmgmlmbajefnkalcfageek", "okta", 
	"oldcompany", "olddomain", "onboard", "onboarding", "online", "ooov", "openexchangerates", "paperclip", "paypal", 
	"pdf", "pdfs", "pgp", "placeholders", "pnc", "png", "policyid", "pollcy", "previouslyscanned", "promo", "provisioning", 
	"psa", "pst", "qb", "qbd", "qbo", "quickbooks", "realtime", "rebillable", "reboot", "recategorization", "recategorize", 
	"recategorized", "reconnection", "referencing", "reimbursability", "reimbursables", "reimburser", "reinitiate", 
	"remediation", "reportbasics", "reporttitle", "requestername", "resetpassword", "resync", "righthand", "rightnetworks", 
	"roadmap", "rtf", "rulepermission", "rulespermissions", "saas", "salesforce", "saml", "samsung", "scansnap", "scim", 
	"screenshot", "screenshots", "scrolling", "seamlessly", "searchable", "selectively", "sep", "seq", "sf", 
	"shareableinvitelink", "smartdata", "smartscan", "smartscan's", "smartscanned", "smartscanning", "smartscans", 
	"smiley", "smtp", "soc", "spam", "spreadsheet", "ssae", "ssh", "sso", "starbucks", "starttime", "stoftware", 
	"stoneridge", "stoneridgesoftware", "strictworkflow", "submitter", "submitter's", "submitters", "submitters'", 
	"submittersemployees", "substring", "subtab", "suggesteddate", "svb", "swissnet", "tada", "taxcode", "temps", "timeframe", 
	"timeframes", "timeline", "timestamps", "transactional", "travelintegration", "travelperk", "travelport", "tripactions", 
	"tsheets", "txt", "uber", "uber's", "unapprove", "unassign", "unassigned", "unassigning", "unassignment", 
	"uncategorized", "uncheck", "unchecking", "undelete", "undeleting", "unencrypted", "uninstall", "uninstalling", "unreported", 
	"unsubmitted", "unsubscribe", "unsupported", "upload", "uploaded", "uploading", "uploads", "url", "urlsubstr", "usd", 
	"userid", "username", "usernames", "userpersonal", "users'", "vcf", "vendorbill", "versa", "vs", "webinar", "webinars", 
	"webpage", "webstore", "whitelist", "whitelisted", "whitelisting", "wifi", "wistia", "workaround", "workflow", "workflows", 
	"www", "xero", "xero's", "xls", "xml", "xmlgateway", "xx", "xxxxx", "xxxxxxxxxx", "xyz", "yourdomain", "youremail", "yourname", 
	"yyyy", "zapier", "zenefits", "‘em", "actionable", "admin", "admin’s", "administering", "admins", "app", "BCAs", "billingpanel", 
	"Bon", "br (?)", "cardsbank", "cmd", "companies'", "companycards", "connections'", "creditcards", "ctrl", "custom field", 
	"customers'", "days’", "dcadaily", "dd", "employees’", "employeesvendors", "eod", "exchangerate", "expenselevel", "expensereport", 
	"expenserules", "experiencing", "fiddly", "formattedamount", "gbp", "gif", "gl", "gps", "gst", "hoc", "html", "inc", "ip", "ips", 
	"journalentry", "mailto", "mm", "multi", "nofollow", "non", "noopener", "ns", "others'", "param", "paymentnet", "pdt", 
	"peoplecontainer", "php", "predefinedresponses", "qr", "rsa", "signin", "signout", "uh")

spellcheck <- spellcheck[grep(paste(expensispell, collapse="|"), spellcheck$not.found, invert=TRUE),]

## Append the spellcheck data frame with the eventID and reponse text
spellcheck$eventID <- repeatables$eventID[spellcheck$row]
spellcheck$Details <- repeatables$Details[spellcheck$row]

## Convert the more.suggestions column to not be a list so we can save it as a CSV
spellcheck$more.suggestions <- as.character(spellcheck$more.suggestions)

## Notification telling the user to choose an appropriate filename given the spellcheck is saved separately
## Actually save the spellchecker output
spcsv <- paste(date,"spellcheckedrepeats",".csv",sep="")
write.csv(spellcheck, file=spcsv, row.names = FALSE)


# Remove cases where no review is needed
repeatables <- filter(repeatables, (maybejankyhtml!="ok" | nbsp!="ok" & decodedhtml!="ok"))

repsnohtml <- filter(repsnohtml, (question!="ok" | plupro!="ok" | breadcrumbs!="ok" | thirdpers!="ok"))

# Save file to user-selected location
## Notify the user about which file they're saving and to name it appropriately
## Write file
prnhcsv <- paste(date,"processedrepeatsNOHTML",".csv",sep="")
write.csv(repsnohtml, file=prnhcsv, row.names = FALSE)

## Notify the user about which file they're saving and to name it appropriately
## Write file
prcsv <- paste(date,"processedrepeats",".csv",sep="")
write.csv(repeatables, file=prcsv, row.names = FALSE)

