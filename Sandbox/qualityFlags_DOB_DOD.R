### RETRIEVE AND ASSESS QUALITY OF ALL VALUES OF DOB AND DOD FOR EACH PATIENT

### Tables containing DOB data were identified by:

### Tables containing DOD data were identified by:



### prepare package dependencies

if (!requireNamespace("dplyr", partial = TRUE, quietly = TRUE)) {
	warning("installing missing package 'dplyr'")
	install.packages("dplyr", quiet = TRUE)
}
if (!isNamespaceLoaded("dplyr")) {
	suppressPackageStartupMessages(require("dplyr"))
}

if (!requireNamespace("RODBC", partial = TRUE, quietly = TRUE)) {
	warning("installing missing package 'RODBC'")
	install.packages("RODBC", quiet = TRUE)
}
if (!isNamespaceLoaded("RODBC")) {
	suppressPackageStartupMessages(require("RODBC"))
}

### prepare database connection
### to make reusable: functionalize, with server and database names as parameters

con <- odbcDriverConnect(connection="driver={SQL Server};
	server=VHACDWRB03;
	database=ORD_Thompson_201805044D;
	trusted_connection=TRUE")

### retrieve patient identifiers
### note: This retrieves all patients from the database, rather than a cohort thereof.

query <- "SELECT a.PatientICN, a.PatientSID, a.Sta3n, a.ScrSSN, PERSON_ID
		FROM ORD_Thompson_201805044D.Src.CohortCrosswalk a
		LEFT OUTER JOIN ORD_Thompson_201805044D.Src.OMOPV5_CohortCrosswalk b
		ON a.PatientSID = b.PatientSID AND a.Sta3n = b.Sta3n"
PatIDs <- sqlQuery(con, query)
PatICNs <- as.data.frame(c(unique(PatIDs$PatientICN)))
colnames(PatICNs) <- ("PatientICN")
rm(query)

### prepare to access tables using PatientSID and Sta3n

table1 <- c("SPatient_SPatient", "BirthDateTime")
table2 <- c("SPatient_PlaceOfBirth", "BirthDateTime")
table3 <- c("SPatient_LabReferralPatient", "BirthDate")
table4 <- c("Oncology_Oncology_Patient_160", "DateOfBirthFilemanFormat")
SIDtables <- rbind(table1, table2, table3, table4)
rm(table1)
rm(table2)
rm(table3)
rm(table4)

### prepare to access tables using ScrSSN
### note: omitting VitalStatus_Master at this time

SCRtables <- as.data.frame(c("VitalStatus_Mini", "DOB"))

### prepare to access tables using PERSON_ID

P_IDtables <- as.data.frame(c("OMOPV5_PERSON", "BIRTH_DATETIME"))

### prepare list to hold output

DOB <- list()

### retrieve DOB data

for (ICN in 1:nrow(PatICNs)) {
	thisPatIDs <- PatIDs %>% filter(PatIDs$PatientICN == PatICNs[ICN,1])
	SIDs <- thisPatIDs %>% select(PatientSID, Sta3n)
	SCRs <- thisPatIDs %>% select(ScrSSN)
	P_IDs <- thisPatIDs %>% select(PERSON_ID)
	
	AllPatDOBs <- as.data.frame(matrix(, nrow = 0, ncol = 7))
	
	namesForColumns <- c("DOB", "year", "month", "day", "PatientICN", "SourceTable", "SourceID")
	colnames(AllPatDOBs) <- namesForColumns


### using PatientSID and Sta3n

	WHEREclause <- paste(" WHERE (PatientSID = ", SIDs[1,1], " AND Sta3n = ", SIDs[1,2], ")", sep = "")
	if (nrow(SIDs) > 1) {	
		for (SID in 2:nrow(SIDs)) {
			WHEREclause <- paste(WHEREclause, " OR (PatientSID = ", SIDs[SID,1], " AND Sta3n = ", SIDs[SID,2], ")", sep = "")
		}
	}

	for (table in 1:nrow(SIDtables)) {
		OneTablePatDOBs <- as.data.frame(matrix(, nrow = 0, ncol = 7))
		colnames(OneTablePatDOBs) <- namesForColumns
		SELECTclause <- paste("SELECT ", SIDtables[table,2], " AS DOB, DATEPART(YEAR, ", SIDtables[table,2], 
					") AS year, DATEPART(MONTH, ", SIDtables[table,2], ") AS month, DATEPART(DAY, ",
					SIDtables[table,2], ") AS day, '", SIDtables[table,1], 
					"' AS SourceTable, CONCAT(PatientSID, '_', Sta3n) AS SourceID", sep = "")
		FROMclause <- paste(" FROM ORD_Thompson_201805044D.Src.", SIDtables[table,1],  sep = "")
		query <- paste(SELECTclause, FROMclause, WHEREclause, sep = "")
		OneTablePatDOBs <- sqlQuery(con, query)
		AllPatDOBs <- rbind(AllPatDOBs, OneTablePatDOBs)
		rm(OneTablePatDOBs)
		rm(SELECTclause)
		rm(FROMclause)
		rm(query)
	}
	rm(WHEREclause)
}







