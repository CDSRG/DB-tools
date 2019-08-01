### PREPARATION FOR RETRIEVING ALL ICD DATA FROM VA VINCI CDW

### First draft: 7/29/2019
### Last edit: 7/31/2019

### The purpose of this script is to facilitate collecting ICD code information
### from all potential sources in the CDW.  
### Possible tables are identified in SQL Server with the following query:
### SELECT TABLE_NAME, COLUMN_NAME FROM [database].INFORMATION_SCEHMA.COLUMNS
### WHERE COLUMN_NAME LIKE '%ICD%'
### Manual inspection of each table is required to ascertain its usefulness.
### This script creates dataframes of the relevant columns in each ICD table
### to aid in the creation of queries against these tables.  In addition to the
### noted columns, all tables include PatientSID and Sta3n.

### prepare data frame to hold target table names and each table's relevant column names (for retrieving patient data)
### to make reusable: functionalize, with table and column names as parameters

table1 <- c("Inpat_Census501Diagnosis", "Census501DiagnosisSID", "AdmitDateTime")
table2 <- c("Inpat_CensusDiagnosis", "CensusDiagnosisSID", "AdmitDateTime")
table3 <- c("Inpat_Inpatient501TransactionDiagnosis", "Inpatient501TransactionDiagnosisSID", "MovementDateTime")
table4 <- c("Inpat_InpatientDischargeDiagnosis", "InpatientDischargeDiagnosisSID", "AdmitDateTime")
table5 <- c("Inpat_InpatientFeeDiagnosis", "InpatientFeeDiagnosisSID", "AdmitDateTime")
table6 <- c("Inpat_PatientTransferDiagnosis", "PatientTransferDiagnosisSID", "MovementDateTime")
table7 <- c("Inpat_PresentOnAdmission", "PresentOnAdmissionSID", "PresentOnAdmissionEnteredDateTime")
table8 <- c("Inpat_SpecialtyTransferDiagnosis", "SpecialtyTransferDiagnosisSID", "MovementDateTime")
table9 <- c("Outpat_ProblemList", "ProblemListSID", "LastModifiedDateTime")
table10 <- c("Outpat_VProcedureDiagnosis", "VProcedureDiagnosisSID", "VProcedureDateTime")
table11 <- c("Outpat_VSkinTestDiagnosis", "VSkinTestDiagnosisSID", "VisitDateTime")
table12 <- c("Outpat_WorkloadVDiagnosis", "VDiagnosisSID", "VisitDateTime")
table13 <- c("Outpat_WorkloadVProcedureDiagnosis", "VProcedureDiagnosisSID", "VisitDateTime")
table14 <- c("Rad_RadiologyNuclearMedicineOrder", "RadiologyNuclearMedicineOrderSID", "RequestEnteredDateTime")
targetTables <- rbind(table1, table2, table3, table4, table5, table6, table7, table8, table9, table10, table11, table12, table13, table14)
colnames(targetTables) <- c("TableName", "TableSID", "DateField")
rm(table1)
rm(table2)
rm(table3)
rm(table4)
rm(table5)
rm(table6)
rm(table7)
rm(table8)
rm(table9)
rm(table10)
rm(table11)
rm(table12)
rm(table13)
rm(table14)

### targets that require joins

joinTable1 <- c("Inpat_InpatientDiagnosis", "InpatientDiagnosisSID", "Inpat_Inpatient", "InpatientSID", "AdmitDateTime")
joinTable2 <- c("Outpat_VDiagnosis", "VDiagnosisSID", "Outpat_Visit", "VisitSID", "VisitDateTime")
joinTargetTables <- rbind(joinTable1, joinTable2)
colnames(joinTargetTables) <- c("PrimaryTableName", "PrimaryTableSID", "JoiningTableName", "JoiningTableSID", "JoiningTableDateField")
rm(joinTable1)
rm(joinTable2)

### create queries
### RIGHT NOW THESE LOOP ONLY CREATE THE QUERIES!!!!!
### See calcCharlson.R for attempts at using these queries.

query_9 <- c()
query_10 <- c()

for (sid in 1:nrow(targetTables)) {
	query_9[sid] <- paste("SELECT ICD9SID, PatientSID, Sta3n, DATEPART(YEAR, ", targetTables[sid,3], "), DATEPART(MONTH, ", 
				targetTables[sid,3], "), DATEPART(DAY, ", targetTables[sid,3], "), '", targetTables[sid,1], "', ", 
				targetTables[sid,2], " FROM ORD_Conlin_201708011D.Src.", targetTables[sid,1], sep = "")
	query_10[sid] <- paste("SELECT ICD10SID, PatientSID, Sta3n, DATEPART(YEAR, ", targetTables[sid,3], "), DATEPART(MONTH, ", 
				targetTables[sid,3], "), DATEPART(DAY, ", targetTables[sid,3], "), '", targetTables[sid,1], "', ", 
				targetTables[sid,2], " FROM ORD_Conlin_201708011D.Src.", targetTables[sid,1], sep = "")
}
rm(sid)

for (sid in 1:nrow(joinTargetTables)) {
	joinQuery_9 <- paste("SELECT a.ICD9SID, a.PatientSID, a.Sta3n, DATEPART(YEAR, ", joinTargetTables[sid,5], "), 
				DATEPART(MONTH, ", joinTargetTables[sid,5], "), DATEPART(DAY, ", joinTargetTables[sid,5], "), '", 
				joinTargetTables[sid,1], "', ", joinTargetTables[sid,2], " FROM ORD_Conlin_201708011D.Src.",
				joinTargetTables[sid,1], " a INNER JOIN ORD_Conlin_201708011D.Src.", joinTargetTables[sid,3], " b ON a.", 
				joinTargetTables[sid,4], " = b.", joinTargetTables[sid,4], " WHERE ICD9SID IN (", sep = "")
	joinQuery_10 <- paste("SELECT a.ICD10SID, a.PatientSID, a.Sta3n, DATEPART(YEAR, ", joinTargetTables[sid,5], "), 
				DATEPART(MONTH, ", joinTargetTables[sid,5], "), DATEPART(DAY, ", joinTargetTables[sid,5], "), '", 
				joinTargetTables[sid,1], "', ", joinTargetTables[sid,2], " FROM ORD_Conlin_201708011D.Src.",
				joinTargetTables[sid,1], " a INNER JOIN ORD_Conlin_201708011D.Src.", joinTargetTables[sid,3], " b ON a.", 
				joinTargetTables[sid,4], " = b.", joinTargetTables[sid,4], " WHERE ICD10SID IN (", sep = "")
}
rm(sid)
query_9 <- c(query_9, joinQuery_9)
query_10 <- c(query_10, joinQuery_10)
rm(joinQuery_9)
rm(joinQuery_10)
rm(targetTables)
rm(joinTargetTables)