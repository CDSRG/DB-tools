### Verify and query all ICD info containing tables; prepare to calculate Charlson score.

### first draft: 9/23/2019
### last edit: 10/1/2019

### The information schema was queried in several databases to compile the list in the associated Excel file.
### Any database in which this is used should be queried to ensure all relevant tables have been identified.

### The Excel file ALL_ICD_TABLES_COLUMNS_DATES comprises a list of tables and corresponding columns
### representing ICD 9 and 10 identifiers and the date most likely to be most associated with the ICD field instance.
### Note that any given CDW database may very well not include all listed tables.

### The R file findCharlsonICDSIDs.R must be executed against the database and the resulting VHAPOR schema table must
### be converted to a Dflt schema table before this script is run.

### load file

targets <- read.csv("P:/ORD_Conlin_201708011D/Celia/Comorb/ALL_ICD_TABLES_COLUMNS_DATES.csv", header = TRUE, stringsAsFactors = FALSE)

### change all instances of 'X' to null values (X was used as a placeholder in the file)

for (x in 1:nrow(targets)) {
  targets[x, which(targets[x,] == 'X')] <- NA
}

### prepare package dependencies

if (!requireNamespace("RODBC", partial = TRUE, quietly = TRUE)) {
  warning("installing missing package 'RODBC'")
  install.packages("RODBC", quiet = TRUE)
}
if (!isNamespaceLoaded("RODBC")) {
  suppressPackageStartupMessages(require("RODBC"))
}

### prepare database connection

con <- odbcDriverConnect(connection="driver={SQL Server};
                         server=VHACDWRB03;
                         database=ORD_Gundle_201703063D;
                         trusted_connection=TRUE")

### store database name in variable (yes this should be functionalized)

useDB <- 'ORD_Gundle_201703063D'

### determine which tables exist in the given database 

for (target in 1:nrow(targets)) {
  query <- paste("SELECT NULLIF(COUNT(*), 0) AS TableExists
                 FROM ", useDB, ".INFORMATION_SCHEMA.TABLES
                 WHERE TABLE_SCHEMA = 'Src'
                 AND TABLE_NAME = '", targets[target,1], "'", sep = "")
  targets$TABLE.EXISTS[target] <- sqlQuery(con, query)
}

### subset targets dataframe to contain only rows where TABLE.EXISTS == 1

targets <- targets[which(targets$TABLE.EXISTS == 1),]




### build queries against existing target tables

### instantiate empty vectors to hold the queries

query_9 <- c()
query_10 <- c()


### build queries

for (target in 1:nrow(targets)) {
  if (targets$REQUIRES.JOIN[target] == 'N') {
    query_9[target] <- paste("SELECT PatientSID, ", targets[target,2], ", ", targets[target,4], " FROM ", useDB, ".Src.", targets[target,1], sep = "")
    query_10[target] <- paste("SELECT PatientSID, ", targets[target,3], ", ", targets[target,4], " FROM ", useDB, ".Src.", targets[target,1], sep = "")
  } else if (targets$REQUIRES.JOIN[target] == 'Y') {
    query_9[target] <- paste("SELECT PatientSID, a.", targets[target,2], ", b.", targets[target,4], " FROM ", useDB, ".Src.", targets[target,1], 
                             " a INNER JOIN ", useDB, ".Src.", targets[target,6], " b ON a.", 
                             targets[target,8], " = b.", targets[target,7], sep = "")
    query_10[target] <- paste("SELECT PatientSID, a.", targets[target,3], ", b.", targets[target,4], " FROM ", useDB, ".Src.", targets[target,1], 
                              " a INNER JOIN ", useDB, ".Src.", targets[target,6], " b ON a.", 
                              targets[target,8], " = b.", targets[target,7], sep = "")
  }
  
  ### add subquery to limit ICDSID retrieval to corresponding Charlson ICD codes in predeveloped DB table
  
  WHEREclause_9 <- paste(" WHERE ", targets[target,2], " IN (SELECT ICDSID FROM ", useDB, ".Dflt.CJM_ICDSIDs WHERE ICDVersion = 'ICD9')", sep = "")
  WHEREclause_10 <- paste(" WHERE ", targets[target,2], " IN (SELECT ICDSID FROM ", useDB, ".Dflt.CJM_ICDSIDs WHERE ICDVersion = 'ICD10')", sep = "")

  ### join select and where clauses
  
  query_9[target] <- paste(query_9[target], WHEREclause_9, sep = "")
  query_10[target] <- paste(query_10[target], WHEREclause_10, sep = "")
  
  ### tidy
  
  rm(WHEREclause_9)
  rm(WHEREclause_10)
}

### build data frame holding query results

charlDx <- data.frame("PatientSID" = numeric(), "ICDSID" = numeric(), "DateDx" = as.Date(character()))

### retrieve patient Charlson diagnosis data

for (query in 1:length(query_9)) {
  
  results_9 <- sqlQuery(con, query_9[query], as.is = TRUE)
  results_10 <- sqlQuery(con, query_10[query], as.is = TRUE)
  
  if ((!is.null(nrow(results_9))) && (!is.null(nrow(results_10)))) {
    
    colnames(results_9) <- c("PatientSID", "ICDSID", "DxDate")
    results_9$DxDate <- as.Date(results_9$DxDate)
    
    colnames(results_10) <- c("PatientSID", "ICDSID", "DxDate")
    results_10$DxDate <- as.Date(results_10$DxDate)
    
    charlDx <- rbind(charlDx, results_9, results_10)
    colnames(charlDx) <- c("PatientSID", "ICDSID", "DxDate")

  } else if ((!is.null(nrow(results_9))) && (is.null(nrow(results_10)))) {
    
    colnames(results_9) <- c("PatientSID", "ICDSID", "DxDate")
    results_9$DxDate <- as.Date(results_9$DxDate)
    
    charlDx <- rbind(charlDx, results_9)
    colnames(charlDx) <- c("PatientSID", "ICDSID", "DxDate")
    
  } else if ((is.null(nrow(results_9))) && (!is.null(nrow(results_10)))) {
    
    colnames(results_10) <- c("PatientSID", "ICDSID", "DxDate")
    results_10$DxDate <- as.Date(results_10$DxDate)
    
    charlDx <- rbind(charlDx, results_10)
    colnames(charlDx) <- c("PatientSID", "ICDSID", "DxDate")
  }
  
  rm(results_9)
  rm(results_10)
  
}

charlDx <- unique(charlDx)

###### identify earliest diagnostic date for each patient in each Charlson category 
### retrieve ICDSID values and corresponding Charlson categories from predeveloped table in database
### need error checking here!

charlQuery <- paste("SELECT ICDSID, CharlsonCat FROM ", useDB, ".Dflt.CJM_ICDSIDs", sep = "")
charlCats <- sqlQuery(con, charlQuery)
rm(charlQuery)

### add Charlson categories to patient diagnostic data

charlDx <- merge(charlDx, charlCats, by = "ICDSID", all = TRUE, suffixes = c(".pat", ".cat"))

### subset to only rows with patient data not null
### possible that using all = FALSE in command above would accomplish the same thing, but not feeling trusting

charlDx <- charlDx[which(!is.na(charlDx[,2])),]


### get current date to use as upper limit of diagnosis date validation

today <- Sys.Date()

### implement upper limit of date
### could do above and below lines in one line

charlDx <- charlDx[which(today > charlDx[,3]),]

### retrieve patient cohort
### note that this script is missing the recovery from interruption stuff because it should be able to run quickly in this DB

patQuery <- "SELECT a.PatientICN, PatientSID, BirthDateTime FROM ORD_Gundle_201703063D.Dflt.CJM_cohort_FINAL a INNER JOIN ORD_Gundle_201703063D.Src.SPatient_SPatient b ON a.PatientICN = b.PatientICN"
patIDs <- sqlQuery(con, patQuery, as.is = TRUE)

### coerce DOB to Date

patIDs$BirthDateTime <- as.Date(patIDs$BirthDateTime)

### instantiate vector of distinct PatientICNs (equivalent to unique individual patients) 

patList <- as.vector(unique(patIDs$PatientICN))

### hold Charlson category names

CharlsonCats <- c("MI", "CHF", "PVasc", "CVasc", "Dem", "Pulm", "Rheum", "PUD", "MilLiv", "DiabCompNO", "DiabCompYES", "Plegia", "Renal", "Malig", "ModSevLiv", "Mets", "AIDS_HIV")

### instantiate data frame to hold patient diagnosis dates in Charlson categories

patCharl <- data.frame(PatientICN = numeric(), MI=as.Date(character()), CHF=as.Date(character()), PVasc=as.Date(character()), CVasc=as.Date(character()), Dem=as.Date(character()), Pulm=as.Date(character()), Rheum=as.Date(character()), PUD=as.Date(character()), MilLiv=as.Date(character()), DiabCompNO=as.Date(character()), DiabCompYES=as.Date(character()), Plegia=as.Date(character()), Renal=as.Date(character()), Malig=as.Date(character()), ModSevLiv=as.Date(character()), Mets=as.Date(character()), AIDS_HIV=as.Date(character()))
for (cats in 2:(length(CharlsonCats)+1)) {
  patCharl[1,cats] <- NULL
}

### iterate through patients

for (pat in 1:length(patList)) {
  
  ### store PatientICN in output dataframe
  
  patCharl[pat,1] <- patList[pat]
  
  ### store patient's DOB (error checking? validation?)
  
  dob <- patIDs[which(patIDs$PatientICN == patList[pat]), 3]
  dob <- dob[[1]]
  
  ### calculate patient's 18th birthday to use as lower limit of diagnosis date validation
  
  dob18 <- seq(dob, length = 2, by = "+18 years")[2]
  
  ### retrieve all PatientSID values for current patient being processed
  
  patSIDs <- patIDs[which(patIDs$PatientICN == patList[pat]),]  
  patSIDs <- patSIDs$PatientSID
  
  ### subset data to only current patient
  
  patDx <- charlDx[which(charlDx$PatientSID %in% patSIDs),]
  
  ### eliminate rows with invalid dates by lower limit
  
  patDx <- patDx[which(patDx[,3] > dob18),]

  
  ### get earliest patient diagnosis for each category

  for (cat in 2:(length(CharlsonCats)+1)) {
    
    if (!is.null(length(patDx[which(patDx[,4] == CharlsonCats[cat]),3]))) {
      
      patCharl[pat, CharlsonCats[cat]] <- min(patDx[which(patDx[,4] == CharlsonCats[cat]),3], na.rm = TRUE)
      
    }
  }

  ### tidy

  rm(patDx)
  
}

### save data frame to database as table

sqlSave(con, patCharl, "CJM_Charlson_dates", rownames = FALSE, colnames = TRUE, nastring = "NULL")
