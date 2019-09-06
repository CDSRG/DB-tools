### RETRIEVE VA VINCI CDW IDENTIFIERS CORRESPONDING TO CHARLSON-INDEX ICD CODES

### First draft: 7/29/2019
### Last edit: 7/31/2019

### The purpose of this script is to amass all CDW identifiers that correspond to 
### the ICD codes for comorbidities targeted by the Charlson Index.  
### (Note that it could be adapted to target any conditions of interest.)
### This script is meant to work in conjunction with SQL Server.
### The purpose of amassing the identifiers is to reduce the processing burden
### when assessing the comorbid conditions of a patient cohort.
### This script is one of several that address different aspects of comorbidity calculation.

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

### prepare codes of Charlson comorbid conditions, with wildcard characters for use in SQL Server
### ICD codes identified by Quan et al, Coding algorithms for defining Cormorbidities in ICD-9-CM and ICD-10 administrative data. Med Care 1130-9 2005

### Myocardial Infarction (MI)
MI_9_inexact <- c("410.%", "412.%")
MI_9_exact <- NULL
MI_9_range <- NULL
MI_10_inexact <- c("I21.%", "I22.%")
MI_10_exact <- c("I25.2")
MI_10_range <- NULL

### Congestive Heart Failure (CHF)
CHF_9_inexact <- c("428.%")
CHF_9_exact <- c("398.91", "402.01", "402.11", "402.91", "404.01", "404.03", "404.11", "404.13", "404.91", "404.93")
CHF_9_start <- c("425.4")
CHF_9_end <- c("425.9")
CHF_9_range <- as.data.frame(cbind(CHF_9_start, CHF_9_end))
colnames(CHF_9_range) <- c("start", "end")
rm(CHF_9_start)
rm(CHF_9_end)
CHF_10_inexact <- c("I43.%", "I50.%")
CHF_10_exact <- c("I09.9", "I11.0", "I13.0", "I13.2", "I25.5", "I42.0", "P29.0")
CHF_10_start <- c("I42.5")
CHF_10_end <- c("I42.9")
CHF_10_range <- as.data.frame(cbind(CHF_10_start, CHF_10_end))
colnames(CHF_10_range) <- c("start", "end")
rm(CHF_10_start)
rm(CHF_10_end)

### Peripheral Vascular Disease (PVasc)
PVasc_9_inexact <- c("440.%", "441.%")
PVasc_9_exact <- c("093.0", "437.3", "447.1", "557.1", "557.9", "V43.4")
PVasc_9_start <- c("443.1")
PVasc_9_end <- c("443.9")
PVasc_9_range <- as.data.frame(cbind(PVasc_9_start, PVasc_9_end))
colnames(PVasc_9_range) <- c("start", "end")
rm(PVasc_9_start)
rm(PVasc_9_end)
PVasc_10_inexact <- c("I70.%", "I71.%")
PVasc_10_exact <- c("I73.1", "I73.8", "I73.9", "I77.1", "I79.0", "K55.1", "K55.8", "K55.9", "Z95.8", "Z95.9")
PVasc_10_range <- NULL

### Cerebrovascular Disease (CVasc)
CVasc_9_inexact <- NULL
CVasc_9_exact <- c("362.34")
CVasc_9_start <- c("430.")
CVasc_9_end <- c("438.999")
CVasc_9_range <- as.data.frame(cbind(CVasc_9_start, CVasc_9_end))
colnames(CVasc_9_range) <- c("start", "end")
rm(CVasc_9_start)
rm(CVasc_9_end)
CVasc_10_inexact <- c("G45.%", "G46.%")
CVasc_10_exact <- c("H34.0")
CVasc_10_start <- c("I60.")
CVasc_10_end <- c("I69.999")
CVasc_10_range <- as.data.frame(cbind(CVasc_10_start, CVasc_10_end))
colnames(CVasc_10_range) <- c("start", "end")
rm(CVasc_10_start)
rm(CVasc_10_end)

### Dementia (Dem)
Dem_9_inexact <- c("290.%")
Dem_9_exact <- c("294.1", "331.2")
Dem_9_range <- NULL
Dem_10_inexact <- c("G30.%")
Dem_10_exact <- c("F05.1", "G31.1")
Dem_10_start <- c("F00.")
Dem_10_end <- c("F03.999")
Dem_10_range <- as.data.frame(cbind(Dem_10_start, Dem_10_end))
colnames(Dem_10_range) <- c("start", "end")
rm(Dem_10_start)
rm(Dem_10_end)

### Chronic Pulmonary Disease (Pulm)
Pulm_9_inexact <- NULL
Pulm_9_exact <- c("416.8", "416.9", "506.4", "508.1", "508.8")
Pulm_9_start <- c("490.")
Pulm_9_end <- c("505.999")
Pulm_9_range <- as.data.frame(cbind(Pulm_9_start, Pulm_9_end))
colnames(Pulm_9_range) <- c("start", "end")
rm(Pulm_9_start)
rm(Pulm_9_end)
Pulm_10_inexact <- NULL
Pulm_10_exact <- c("I27.8", "I27.9", "J68.4", "J70.1", "J70.3")
Pulm_10_start <- c("J40.", "J60.")
Pulm_10_end <- c("J47.999", "J67.999")
Pulm_10_range <- as.data.frame(cbind(Pulm_10_start, Pulm_10_end))
colnames(Pulm_10_range) <- c("start", "end")
rm(Pulm_10_start)
rm(Pulm_10_end)

### Rheumatic Disease (Rheum)
Rheum_9_inexact <- c("725.%")
Rheum_9_exact <- c("446.5", "714.8")
Rheum_9_start <- c("710.0", "714.0")
Rheum_9_end <- c("710.4", "714.2")
Rheum_9_range <- as.data.frame(cbind(Rheum_9_start, Rheum_9_end))
colnames(Rheum_9_range) <- c("start", "end")
rm(Rheum_9_start)
rm(Rheum_9_end)
Rheum_10_inexact <- c("M05.%", "M06.%")
Rheum_10_exact <- c("M31.5", "M35.1", "M35.3", "M36.0")
Rheum_10_start <- c("M32.")
Rheum_10_end <- c("M34.999")
Rheum_10_range <- as.data.frame(cbind(Rheum_10_start, Rheum_10_end))
colnames(Rheum_10_range) <- c("start", "end")
rm(Rheum_10_start)
rm(Rheum_10_end)

### Peptic Ulcer Disease (PUD)
PUD_9_inexact <- NULL
PUD_9_exact <- NULL
PUD_9_start <- c("531.")
PUD_9_end <- c("534.999")
PUD_9_range <- as.data.frame(cbind(PUD_9_start, PUD_9_end))
colnames(PUD_9_range) <- c("start", "end")
rm(PUD_9_start)
rm(PUD_9_end)
PUD_10_inexact <- NULL
PUD_10_exact <- NULL
PUD_10_start <- c("K25.")
PUD_10_end <- c("K28.999")
PUD_10_range <- as.data.frame(cbind(PUD_10_start, PUD_10_end))
colnames(PUD_10_range) <- c("start", "end")
rm(PUD_10_start)
rm(PUD_10_end)

### Mild Liver Disease (MilLiv)
MilLiv_9_inexact <- c("570.%", "571.%")
MilLiv_9_exact <- c("070.22", "070.23", "070.32", "070.33", "070.44", "070.54", "070.6", "070.9", "573.3", "573.4", "573.8", "573.9", "V42.7")
MilLiv_9_range <- NULL
MilLiv_10_inexact <- c("B18.%", "K73.%", "K74.%")
MilLiv_10_exact <- c("K70.9", "K71.7", "K76.0", "K76.8", "K76.9", "Z94.4")
MilLiv_10_start <- c("K70.0", "K71.3", "K76.2")
MilLiv_10_end <- c("K70.3", "K71.5", "K76.4")
MilLiv_10_range <- as.data.frame(cbind(MilLiv_10_start, MilLiv_10_end))
colnames(MilLiv_10_range) <- c("start", "end")
rm(MilLiv_10_start)
rm(MilLiv_10_end)

### Diabetes Without Chronic Complication (DiabCompNO)
DiabCompNO_9_inexact <- NULL
DiabCompNO_9_exact <- c("250.8", "250.9")
DiabCompNO_9_start <- c("250.0")
DiabCompNO_9_end <- c("250.3")
DiabCompNO_9_range <- as.data.frame(cbind(DiabCompNO_9_start, DiabCompNO_9_end))
colnames(DiabCompNO_9_range) <- c("start", "end")
rm(DiabCompNO_9_start)
rm(DiabCompNO_9_end)
DiabCompNO_10_inexact <- NULL
DiabCompNO_10_exact <- c("E10.0", "E10.1", "E10.6", "E10.8", "E10.9", "E11.0", "E11.1", "E11.6", "E11.8", "E11.9", "E12.0", "E12.1", "E12.6", "E12.8",
	"E12.9", "E13.0", "E13.1", "E13.6", "E13.8", "E13.9", "E14.0", "E14.1", "E14.6", "E14.8", "E14.9")
DiabCompNO_10_range <- NULL

### Diabetes With Chronic Complication (DiabCompYES)
DiabCompYES_9_inexact <- NULL
DiabCompYES_9_exact <- NULL
DiabCompYES_9_start <- c("250.4")
DiabCompYES_9_end <- c("250.7")
DiabCompYES_9_range <- as.data.frame(cbind(DiabCompYES_9_start, DiabCompYES_9_end))
colnames(DiabCompYES_9_range) <- c("start", "end")
rm(DiabCompYES_9_start)
rm(DiabCompYES_9_end)
DiabCompYES_10_inexact <- NULL
DiabCompYES_10_exact <- c("E10.7", "E11.7", "E12.7", "E13.7", "E14.7")
DiabCompYES_10_start <- c("E10.2", "E11.2", "E12.2", "E13.2", "E14.2")
DiabCompYES_10_end <- c("E10.5", "E11.5", "E12.5", "E13.5", "E14.5")
DiabCompYES_10_range <- as.data.frame(cbind(DiabCompYES_10_start, DiabCompYES_10_end))
colnames(DiabCompYES_10_range) <- c("start", "end")
rm(DiabCompYES_10_start)
rm(DiabCompYES_10_end)

### Hemiplegia or Paraplegia (Plegia)
Plegia_9_inexact <- c("342.%", "343.%")
Plegia_9_exact <- c("334.1", "344.9")
Plegia_9_start <- c("344.0")
Plegia_9_end <- c("344.6")
Plegia_9_range <- as.data.frame(cbind(Plegia_9_start, Plegia_9_end))
colnames(Plegia_9_range) <- c("start", "end")
rm(Plegia_9_start)
rm(Plegia_9_end)
Plegia_10_inexact <- c("G81.%", "G82.%")
Plegia_10_exact <- c("G04.1", "G11.4", "G80.1", "G80.2", "G83.9")
Plegia_10_start <- c("G83.0")
Plegia_10_end <- c("G83.4")
Plegia_10_range <- as.data.frame(cbind(Plegia_10_start, Plegia_10_end))
colnames(Plegia_10_range) <- c("start", "end")
rm(Plegia_10_start)
rm(Plegia_10_end)

### Renal Disease (Renal)
Renal_9_inexact <- c("582.%", "585.%", "586.%", "V56.%")
Renal_9_exact <- c("403.01", "403.11", "403.91", "404.02", "404.03", "404.12", "404.13", "404.92", "404.93", "588.0", "V42.0", "V45.1")
Renal_9_start <- c("583.0")
Renal_9_end <- c("583.7")
Renal_9_range <- as.data.frame(cbind(Renal_9_start, Renal_9_end))
colnames(Renal_9_range) <- c("start", "end")
rm(Renal_9_start)
rm(Renal_9_end)
Renal_10_inexact <- c("N18.%", "N19.%")
Renal_10_exact <- c("I12.0", "I13.1", "N25.0", "Z94.0", "Z99.2")
Renal_10_start <- c("N03.2", "N05.2", "Z49.0")
Renal_10_end <- c("N03.7", "N05.7", "Z49.2")
Renal_10_range <- as.data.frame(cbind(Renal_10_start, Renal_10_end))
colnames(Renal_10_range) <- c("start", "end")
rm(Renal_10_start)
rm(Renal_10_end)

### Any malignancy, including lymphoma and leukemia, except malignant neoplasm of skin (Malig)
Malig_9_inexact <- NULL
Malig_9_exact <- c("238.6")
Malig_9_start <- c("140.", "174.", "200.")
Malig_9_end <- c("172.999", "195.8", "208.999")
Malig_9_range <- as.data.frame(cbind(Malig_9_start, Malig_9_end))
colnames(Malig_9_range) <- c("start", "end")
rm(Malig_9_start)
rm(Malig_9_end)
Malig_10_inexact <- c("C43.%", "C88.%")
Malig_10_exact <- NULL
Malig_10_start <- c("C00.", "C30.", "C37.", "C45.", "C60.", "C81.", "C90.")
Malig_10_end <- c("C26.999", "C34.999", "C41.999", "C58.999", "C76.999", "C85.999", "C97.999")
Malig_10_range <- as.data.frame(cbind(Malig_10_start, Malig_10_end))
colnames(Malig_10_range) <- c("start", "end")
rm(Malig_10_start)
rm(Malig_10_end)

### Moderate or Severe Liver Disease (ModSevLiv)
ModSevLiv_9_inexact <- NULL
ModSevLiv_9_exact <- NULL
ModSevLiv_9_start <- c("456.0", "572.2")
ModSevLiv_9_end <- c("456.2", "572.8")
ModSevLiv_9_range <- as.data.frame(cbind(ModSevLiv_9_start, ModSevLiv_9_end))
colnames(ModSevLiv_9_range) <- c("start", "end")
rm(ModSevLiv_9_start)
rm(ModSevLiv_9_end)
ModSevLiv_10_inexact <- NULL
ModSevLiv_10_exact <- c("I85.0", "I85.9", "I86.4", "I98.2", "K70.4", "K71.1", "K72.1", "K72.9", "K76.5", "K76.6", "K76.7")
ModSevLiv_10_range <- NULL

### Metastatic Solid Tumor (Mets)
Mets_9_inexact <- NULL
Mets_9_exact <- NULL
Mets_9_start <- c("196.")
Mets_9_end <- c("199.999")
Mets_9_range <- as.data.frame(cbind(Mets_9_start, Mets_9_end))
colnames(Mets_9_range) <- c("start", "end")
rm(Mets_9_start)
rm(Mets_9_end)
Mets_10_inexact <- NULL
Mets_10_exact <- NULL
Mets_10_start <- c("C77.")
Mets_10_end <- c("C80.999")
Mets_10_range <- as.data.frame(cbind(Mets_10_start, Mets_10_end))
colnames(Mets_10_range) <- c("start", "end")
rm(Mets_10_start)
rm(Mets_10_end)

### AIDS/HIV (AIDS_HIV)
AIDS_HIV_9_inexact <- NULL
AIDS_HIV_9_exact <- NULL
AIDS_HIV_9_start <- c("042.")
AIDS_HIV_9_end <- c("044.999") 
AIDS_HIV_9_range <- as.data.frame(cbind(AIDS_HIV_9_start, AIDS_HIV_9_end))
colnames(AIDS_HIV_9_range) <- c("start", "end")
rm(AIDS_HIV_9_start)
rm(AIDS_HIV_9_end)
AIDS_HIV_10_inexact <- c("B24.%")
AIDS_HIV_10_exact <- NULL
AIDS_HIV_10_start <- c("B20.")
AIDS_HIV_10_end <- c("B22.999")
AIDS_HIV_10_range <- as.data.frame(cbind(AIDS_HIV_10_start, AIDS_HIV_10_end))
colnames(AIDS_HIV_10_range) <- c("start", "end")
rm(AIDS_HIV_10_start)
rm(AIDS_HIV_10_end)

### prepare list of Charlson categories (in abbreviated, variable name format) for use in later processing
CharlsonCats <- c("MI", "CHF", "PVasc", "CVasc", "Dem", "Pulm", "Rheum", "PUD", "MilLiv", "DiabCompNO", "DiabCompYES", "Plegia", "Renal", "Malig", "ModSevLiv", "Mets", "AIDS_HIV")

### prepare data frame to hold CDW identifiers (ICD9SID and ICD10SID values) for the ICD codes relevant to Charlson index
ICDSIDs <- data.frame(ICDSID = numeric(), ICDVersion = character(), ICDCode = character(), CharlsonCat = character())

### retrieve all relevant CDW identifiers
### to make reusable: functionalize, with table and column names as parameters
SELECTclause_9 <- "SELECT ICD9SID AS ICDSID, 'ICD9' AS ICDVersion, ICD9Code AS ICDCode, '"
FROMclause_9 <- "' AS CharlsonCat FROM CDWWork.Dim.ICD9 WHERE"
SELECTclause_10 <- "SELECT ICD10SID AS ICDSID, 'ICD10' AS ICDVersion, ICD10Code AS ICDCode, '"
FROMclause_10 <- "' AS CharlsonCat FROM CDWWork.Dim.ICD10 WHERE"

for (i in 1:length(CharlsonCats)) {
	query_9 <- paste(SELECTclause_9, CharlsonCats[i], FROMclause_9, sep = "")
	query_10 <- paste(SELECTclause_10, CharlsonCats[i], FROMclause_10, sep = "")
	exactCodes_9 <- get(paste(CharlsonCats[i], "_9_exact", sep = ""))
	inexactCodes_9 <- get(paste(CharlsonCats[i], "_9_inexact", sep = ""))
	rangeCodes_9 <- get(paste(CharlsonCats[i], "_9_range", sep = ""))
	exactCodes_10 <- get(paste(CharlsonCats[i], "_10_exact", sep = ""))
	inexactCodes_10 <- get(paste(CharlsonCats[i], "_10_inexact", sep = ""))
	rangeCodes_10 <- get(paste(CharlsonCats[i], "_10_range", sep = ""))
	WHEREclause_9 <- ""
	WHEREclause_10 <- ""
	if (!is.null(exactCodes_9)) {
		WHEREclause_9 <- paste(" ICD9Code = '", exactCodes_9[1], "'", sep = "")
		if (length(exactCodes_9) > 1) {
			for (code in 2:length(exactCodes_9)) {
				WHEREclause_9 <- paste(WHEREclause_9, " OR ICD9Code = '", exactCodes_9[code], "'", sep = "")
			}
		}
	}
	if (!is.null(inexactCodes_9)) {
		if (!is.null(exactCodes_9)) {
			WHEREclause_9 <- paste(WHEREclause_9, " OR", sep = "")
		}
		WHEREclause_9 <- paste(WHEREclause_9, " ICD9Code LIKE '", inexactCodes_9[1], "'", sep = "")
		if (length(inexactCodes_9) > 1) {
			for (code in 2:length(inexactCodes_9)) {
				WHEREclause_9 <- paste(WHEREclause_9, " OR ICD9Code LIKE '", exactCodes_9[code], "'", sep = "")
			}
		}
	}
	if (!is.null(rangeCodes_9)) {
		if (!is.null(exactCodes_9) | !is.null(inexactCodes_9)) {
			WHEREclause_9 <- paste(WHEREclause_9, " OR", sep = "")
		}
		start1 <- rangeCodes_9$start[1]
		end1 <- rangeCodes_9$end[1]
		WHEREclause_9 <- paste(WHEREclause_9, " (ICD9Code BETWEEN '", start1, "' AND '", end1, "')", sep = "")
		if (nrow(rangeCodes_9) > 1) {
			for (code in 2:nrow(rangeCodes_9)) {
				startCode <- rangeCodes_9$start[code]
				endCode <- rangeCodes_9$end[code]
				WHEREclause_9 <- paste(WHEREclause_9, " OR (ICD9Code BETWEEN '", startCode, "' AND '", endCode, "')", sep = "")
				rm(startCode)
				rm(endCode)
			}
		}
	rm(start1)
	rm(end1)
	}
	if (!is.null(exactCodes_10)) {
		WHEREclause_10 <- paste(WHEREclause_10, " ICD10Code = '", exactCodes_10[1], "'", sep = "")
		if (length(exactCodes_10) > 1) {
			for (code in 2:length(exactCodes_10)) {
				WHEREclause_10 <- paste(WHEREclause_10, " OR ICD10Code = '", exactCodes_10[code], "'", sep = "")
			}
		}
	}
	if (!is.null(inexactCodes_10)) {
		if (!is.null(exactCodes_10)) {
			WHEREclause_10 <- paste(WHEREclause_10, " OR", sep = "")
		}
		WHEREclause_10 <- paste(WHEREclause_10, " ICD10Code LIKE '", inexactCodes_10[1], "'", sep = "")
		if (length(inexactCodes_10) > 1) {
			for (code in 2:length(inexactCodes_10)) {
				WHEREclause_10 <- paste(WHEREclause_10, " OR ICD10Code LIKE '", inexactCodes_10[code], "'", sep = "")
			}
		}
	}
	if (!is.null(rangeCodes_10)) {
		if (!is.null(exactCodes_10) | !is.null(inexactCodes_10)) {
			WHEREclause_10 <- paste(WHEREclause_10, " OR", sep = "")
		}
		start1 <- rangeCodes_10$start[1]
		end1 <- rangeCodes_10$end[1]
		WHEREclause_10 <- paste(WHEREclause_10, " (ICD10Code BETWEEN '", start1, "' AND '", end1, "')", sep = "")
		if (nrow(rangeCodes_10) > 1) {
			for (code in 2:nrow(rangeCodes_10)) {
				startCode <- rangeCodes_10$start[code]
				endCode <- rangeCodes_10$end[code]
				WHEREclause_10 <- paste(WHEREclause_10, " OR (ICD10Code BETWEEN '", startCode, "' AND '", endCode, "')", sep = "")
				rm(startCode)
				rm(endCode)
			}
		}
	rm(start1)
	rm(end1)
	}	
	query_9 <- paste(query_9, WHEREclause_9, sep = "")
	results_9 <- sqlQuery(con, query_9)
	ICDSIDs <- rbind(ICDSIDs, results_9)
	query_10 <- paste(query_10, WHEREclause_10, sep = "")
	results_10 <- sqlQuery(con, query_10)
	ICDSIDs <- rbind(ICDSIDs, results_10)
	rm(query_9)
	rm(query_10)
	rm(exactCodes_9)
	rm(inexactCodes_9)
	rm(rangeCodes_9)
	rm(exactCodes_10)
	rm(inexactCodes_10)
	rm(rangeCodes_10)
	rm(WHEREclause_9)
	rm(WHEREclause_10)
	rm(results_9)
	rm(results_10)
}
rm(SELECTclause_9)
rm(FROMclause_9)
rm(SELECTclause_10)
rm(FROMclause_10)

sqlSave(con, ICDSIDs, CJM_ICDSIDs)
write.csv(ICDSIDs, file = "P:/ORD_Conlin_201708011D/Celia/Comorb/ICDSIDs.csv")




