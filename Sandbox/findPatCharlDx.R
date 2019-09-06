### amass all Charlson-relevant patient diagnoses

### FIRST MUST RUN prepToQueryICDTables AND findCharlsonICDSIDs


### prepare package dependencies
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
	database=ORD_Conlin_201708011D;
	trusted_connection=TRUE")

### prepare data structures for holding output
AllCatDx <- list()

OneCatDx <- as.data.frame(matrix(, nrow = 0, ncol = 8))
OneSourceDx <- as.data.frame(matrix(, nrow = 0, ncol = 8))

namesForColumns <- c("ICDSID", "PatientSID", "Sta3n", "EstDxYear", "EstDxMonth", "EstDxDay", "SourceTable", "SourceSID")

colnames(OneCatDx) <- namesForColumns
colnames(OneSourceDx) <- namesForColumns

### prepare vector of Charlson category names for use in later processing
CharlsonCats <- c("MI", "CHF", "PVasc", "CVasc", "Dem", "Pulm", "Rheum", "PUD", "MilLiv", "DiabCompNO", "DiabCompYES", "Plegia", "Renal", "Malig", "ModSevLiv", "Mets", "AIDS_HIV")

subquery_9 <- c()
subquery_10 <- c() 

for (cat in 1:length(CharlsonCats)) {
	subquery_9 <- paste(" WHERE ICD9SID IN (SELECT ICDSID FROM CJM_ICDSIDs WHERE ICDVersion = 'ICD9' AND CharlsonCat = '", CharlsonCats[cat], "')", sep = "")
	for (source in 1:length(query_9)) {
		query <- paste(query_9[source], subquery_9, sep = "")
		OneSourceDx <- sqlQuery(con, query)
		colnames(OneSourceDx) <- namesForColumns
		OneCatDx <- rbind(OneCatDx, OneSourceDx)
		rm(OneSourceDx)
	}
	rm(subquery_9) 
	rm(query)
	rm(source)
	subquery_10 <- paste(" WHERE ICD10SID IN (SELECT ICDSID FROM CJM_ICDSIDs WHERE ICDVersion = 'ICD10' AND CharlsonCat = '", CharlsonCats[cat], "')", sep = "")
	for (source in 1:length(query_10)) {
		query <- paste(query_10[source], subquery_10, sep = "")
		OneSourceDx <- sqlQuery(con, query)
		colnames(OneSourceDx) <- namesForColumns
		OneCatDx <- rbind(OneCatDx, OneSourceDx)
		rm(OneSourceDx)
	}
	rm(subquery_10)
	rm(query)
	rm(source)
	AllCatDx[[cat]] <- OneCatDx
	TableName <- paste("CJM_PatDx_", CharlsonCats[cat], sep = "")
	sqlSave(con, OneCatDx, get(TableName))
	rm(OneCatDx)
}

