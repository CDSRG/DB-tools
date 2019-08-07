### newStanford.R
### This script counts frequencies of terms (predetermined) appearing in the clinical notes of patients.
### Patient cohort phenotype: stage IV cancer and radiation treatments.
### All clinical notes from one year before first radiation treatment until censoring.
### This script also deidentifies the VA CDW patient data by: replacing the patient identifier PatientICN
### with a randomly generated number, replacing the document identifier TIUDocumentSID with a randomly generated number,
### and replacing all actual dates with the time interval from 0 = date radiation began.

### prepare package dependencies

if (!requireNamespace("RODBC", partial = TRUE, quietly = TRUE)) {
	warning("installing missing package 'RODBC'")
	install.packages("RODBC", quiet = TRUE)
}
if (!isNamespaceLoaded("RODBC")) {
	suppressPackageStartupMessages(require("RODBC"))
}

if (!requireNamespace("dplyr", partial = TRUE, quietly = TRUE)) {
	warning("installing missing package 'dplyr'")
	install.packages("dplyr", quiet = TRUE)
}
if (!isNamespaceLoaded("dplyr")) {
	suppressPackageStartupMessages(require("dplyr"))
}

if (!requireNamespace("tm", partial = TRUE, quietly = TRUE)) {
	warning("installing missing package 'tm'")
	install.packages("tm", quiet = TRUE)
}
if (!isNamespaceLoaded("tm")) {
	suppressPackageStartupMessages(require("tm"))
}

### prepare database connection
### to make reusable: functionalize, with server and database names as parameters

con <- odbcDriverConnect(connection="driver={SQL Server};
	server=VHACDWRB03;
	database=ORD_Thompson_201805044D;
	trusted_connection=TRUE")

################## FROM THIS POINT UNTIL NOTED, SCRIPT SHOULD NOT BE RUN AGAIN. ###################################

### instantiate empty vector that will hold identifiers of patients with completed processing

completedPats <- c()

### retrieve cohort identifiers
### (table was developed earlier in database)
### to make reusable: functionalize, with table and column names as parameters

query <- "SELECT * FROM ORD_Thompson_201805044D.Dflt.CJM_Stanford"
cohort <- sqlQuery(con, query, as.is = FALSE, stringsAsFactors = FALSE)
rm(query)
cohort$DateRadiationStarted <- as.Date(cohort$DateRadiationStarted)

### create random 6 digit replacements for patient identifiers

PatIDs <- c(sample(100000:999999, size = nrow(cohort), replace = FALSE))

### create key correlating randomly generated and actual identifiers -- key never leaves VINCI environment

cohort <- cbind(cohort, PatIDs)
rm(PatIDs)

### save key

write.table(cohort, file = "P:/ORD_Thompson_201805044D/Celia/Radiation/Stanford/StanfordCohortKey.csv", sep = ",", row.names = FALSE)

### create file to hold list of completed patients

write.table(completedPats, file = "P:/ORD_Thompson_201805044D/Celia/Radiation/Stanford/StanfordCompleted.csv", sep = ",", row.names = FALSE)

################### END OF PREPARATION.  BELOW CAN BE RERUN. #######################################

### retrieve list of patients with completed processing

completedPats <- read.csv(file = "P:/ORD_Thompson_201805044D/Celia/Radiation/Stanford/StanfordCompleted.csv", header = FALSE, sep = ",")
completedPats <- as.vector(completedPats[,1])

### generate list of patients to process

cohort <- read.csv(file = "P:/ORD_Thompson_201805044D/Celia/Radiation/Stanford/StanfordCohortKey.csv", header = TRUE, sep = ",")
cohortList <- cohort$PatientICN

### remove completed patients from list still to be processed

cohortList <- cohortList[which(!cohortList %in% completedPats)]

### retrieve term dictionary

terms <- read.csv("P:/ORD_Thompson_201805044D/Celia/Radiation/Stanford/text_features_edited.csv")
terms <- terms[,1]
terms <- as.character(terms)

### process patient notes

for (pat in 1:length(cohortList)) {

	patData <- cohort %>% filter(cohort$PatientICN == cohortList[pat])

### retrieve all PatientSID and Sta3n values for the PatientICN

	query <- paste("SELECT PatientSID, Sta3n FROM ORD_Thompson_201805044D.Src.CohortCrosswalk WHERE PatientICN = ", cohortList[pat], sep = "")
	SIDs <- sqlQuery(con, query)
	rm(query)

### build WHERE clause of query to retrieve all notes for the patient

	zeroDate <- patData$DateRadiationStarted

	WHEREclause <- paste(" WHERE ((PatientSID = ", SIDs[1,1], " AND Sta3n = ", SIDs[1,2], ")", sep = "")
	if (nrow(SIDs) > 1) {
		for (sid in 2:nrow(SIDs)) {
			WHEREclause <- paste(WHEREclause, " OR (PatientSID = ", SIDs[sid,1], " AND Sta3n = ", SIDs[sid,2], ")", sep = "")
		}
	}
	WHEREclause <- paste(WHEREclause, ") AND DATEDIFF(DAY, '", zeroDate, "', EntryDateTime) >= -366", sep = "")

### retrieve all relevant notes for the patient

	query <- paste("SELECT TIUDocumentSID, ReportText, DATEDIFF(DAY, '", zeroDate, "', EntryDateTime) FROM ORD_Thompson_201805044D.Src.TIU_TIUDocument_8925", sep = "")
	query <- paste(query, WHEREclause, " ORDER BY EntryDateTime", sep = "")
	notes <- sqlQuery(con, query)
	colnames(notes) <- c("doc_id", "text", "interval")

### check if results set is null
### if not, create document term matrix for patient

	if (!nrow(notes) == 0) {
		notesDTM <- notes[,1:2]
		notesDTM <- SimpleCorpus(DataframeSource(notesDTM), control = list(language = "en"))
		notesDTM <- DocumentTermMatrix(notesDTM, list(dictionary = terms))
		notesDTM <- as.data.frame(as.matrix(notesDTM))
		notesDTM <- notesDTM[c(terms)]

### merge data frames to replace date interval and remove document identifier

	doc_id <- rownames(notesDTM)
	notesDTM <- cbind(notesDTM, doc_id)
	notes <- notes[,-2]
	thisPat <- merge(notes, notesDTM, by = "doc_id", all = TRUE, no.dups = FALSE)
	thisPat <- thisPat[,-1]

### add randomly generated patient identifier to each row

	patID <- patData$PatIDs
	thisPat <- cbind(patID, thisPat)

### save deidentified data to file

	write.table(thisPat, file = "P:/ORD_Thompson_201805044D/Celia/Radiation/Stanford/StanfordDTM.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)

	}

### save PatientICN to completed patients file

	ICN <- patData$PatientICN
	write.table(patData$PatientICN, file = "P:/ORD_Thompson_201805044D/Celia/Radiation/Stanford/StanfordCompleted.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)

}


