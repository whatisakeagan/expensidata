# Load required libraries
## NOTE: If you haven't run the initial setup file, do that first!
library (easycsv)

# Set working directory
setwd(easycsv::choose_dir())

# Choose export file to read in
convosmaster <- read.csv (file.choose(new = FALSE))

# Keep only inbound conversations
convosmaster <- convosmaster[which(convosmaster$Inbound.Outbound=='Inbound'),]

# Keep only convos assigned to Concierge
convosmaster <- convosmaster[which(convosmaster$Assigned.to..name.=='Concierge'),]

# Remove Closed convos
#convosmaster <- convosmaster[which(convosmaster$Conversation.status !='Closed'),]

# Keep only convos without a reply
convosmaster <- convosmaster[which(is.na(convosmaster$Time.to.first.reply..seconds.)),]

# Remove unnecessary columns
convos <- convosmaster[,1:5]

# Remove unneeded column
convos[,4] <- NULL 

# Create channel viewer links for each convo
# NOTE: redacted private URL in paste command
convos$channelViewer <- paste("", convos$Conversation.ID, sep="")

# Reorder columns to avoid confusion
convos <- convos[,c(3,2,1,5,4)]

# Write the file to the active directory
write.csv(convos, file="unansweredconciergeconvos.csv", row.names=FALSE)
