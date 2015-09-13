"

Merge Congressional Speech data from the Sunlight Foundation w/ Common Space from Voteview 
Note: Uses bioguide to icpsr csv from: https://gist.github.com/konklone/1642406

@author: Gaurav Sood 
"

# Set the wd
setwd(basedir)

# Load the libs.
library(readr)

# Read in common space dee-dub
cdw <- read.csv("DdubScores.csv")

# Read in the icpsr to bioguide
bridge 			<- read.csv("results_plus_hand_entry.csv", header=F) # see https://gist.github.com/konklone/1642406
names(bridge) 	<- c("fullname", "bioguide_id_1", "idno_2") 

# Merge dw/bridge
cdwb <- merge(cdw, bridge, all.x=F, all.y=F, by.x="idno", by.y="idno_2")

# Make unique by Congress
cdwb$biocong <- paste0(cdwb$cong, cdwb$bioguide_id)

# Read in the cleaned words
cap 		<- read_csv("capitolwords_clean.csv")
	
# Take out rows which don't have bioguide
small_cap 	<- subset(cap, bioguide_id!="")

# Make unique by Congress
small_cap$biocong <- paste0(small_cap$congress, small_cap$bioguide_id)

# Merge small_cap and cdwb
out <- merge(small_cap, cdwb, by="biocong", all.x=FALSE, all.y=FALSE)

# Out 
write_csv(out, "capitol_dDub.csv")
	