### CDSRG

### This file holds all our tools together for sourcing.



################################################################################
################################################################################
################################################################################

### makeCon()

### function to install RODBC package and establish a connection to a VINCI CDW database
### input: server name, database name
### output: database connection
makeCon <- function(serverName, dbName) {
  
  ### install RODBC package
  
  suppressPackageStartupMessages(require("RODBC"))
  
  ### build connection name and establish connection
  
  conString <- paste("driver={SQL Server};server=", serverName, ";database=", dbName, ";trusted_connection=TRUE", sep = "")
  con <- odbcDriverConnect(connection = conString)
  
  ### error handling
  ###### invalid server name
  ###### invalid db name or db user doesn't have permission for
  
  ### return connection
  
  return(con)
  
}

################################################################################
################################################################################
################################################################################

### makeLog()
### function to install logger package and set up logging file

makeLog <- function(dbName, projectName) {
  
  ### install logger package
  
  suppressPackageStartupMessages(require("logger"))
  
  ### acquire current date and format for using in file name
  
  today <- Sys.Date()
  today <- format(today, format = "%Y%m%d")
  
  ### instantiate file
  
  logfile <- paste("P:/", dbName, "/", today, "_", projectName, ".log", sep = "")
  log_appender(appender_file(logfile), index=2)
  
  return(TRUE)
  
}

################################################################################
################################################################################
################################################################################
### prepQuery()
### function for properly setting up to retrieve data from a database using RODBC

prepQuery <- function(con, query=NULL, rows_at_time=attr(con, "rows_at_time")) {
  # test database connection and clear error log
  if (!RODBC:::odbcValidChannel(con)) {
    log_error("Invalid DB connection")
    return(FALSE)
  }
  tryCatch(
    odbcClearError(con),
    error=function(e) {
      log_error(e$message)
      return(FALSE)
    }
  )
  if (is.null(query)) {
    log_error("Missing value for 'query'")
    return(FALSE)
  }
  if (!is.character(query)) {
    log_warn("Converting from non-character value provided for 'query'")
    query <- as.character(query)
  }
  if (length(query) != 1) {
    log_error("Single value required for 'query'")
    return(FALSE)
  }
  if (nchar(query) < 1) {
    log_error("Empty 'query' provided")
    return(FALSE)
  }
  log_info("Prepping query: ", query)	
  if (.Call(RODBC:::C_RODBCQuery, attr(con, "handle_ptr"), query, as.integer(rows_at_time)) < 0) {
    log_error("Error evaluating query using provided DB connection")
    log_error(odbcGetErrMsg(con))
    return(FALSE)
  }
  return(TRUE)
}

################################################################################
################################################################################
################################################################################
### fetchQuery()
### function to retrieve data from a database in chunks and apply another function to the data

fetchQuery <- function(con, n=NULL, buffsize=1000, FUN=NULL, as.is=TRUE, ...) {
  # test database connection
  if (!RODBC:::odbcValidChannel(con)) {
    log_error("Invalid DB connection")
    return(FALSE)
  }
  cols <- .Call(RODBC:::C_RODBCNumCols, attr(con, "handle_ptr"))
  if (cols < 0L) {
    log_error("No data")
    return(FALSE)
  }
  cData <- .Call(RODBC:::C_RODBCColData, attr(con, "handle_ptr"))
  if (!is.numeric(n) | (length(n) != 1)) { 
    n <- 0
  }
  n <- max(0, floor(n), na.rm=TRUE)
  if (is.logical(as.is) & length(as.is) == 1) {
    as.is <- rep(as.is, length=cols)
  }
  else if (is.numeric(as.is)) {
    if (any(as.is < 1 | as.is > cols)) 
      log_warn("invalid numeric 'as.is' values: ", as.is[which(as.is < 1 | as.is > cols)])
    as.is <- as.is[which(as.is >= 1 & as.is <= cols)]
    i <- rep(FALSE, cols)
    i[as.is] <- TRUE
    as.is <- i
  }
  else if (is.character(as.is)) {
    as.is <- cData$names %in% as.is
  }
  if (length(as.is) != cols) {
    log_error("'as.is' has the wrong length ", length(as.is), " != cols = ", cols)
    return(FALSE)
  }
  as.is <- which(as.is)
  if (is.null(FUN)) {
    FUN <- return
    use.dots <- FALSE
  }
  else {
    tryCatch(
      FUN <- match.fun(FUN),
      error=function(e) {
        log_error(e$message)
        return(FALSE)				
      }
    )
    use.dots <- (...length() > 0L) & (length(formals(FUN)) > 1L)
  }
  counter <- 0
  nresults <- 0
  repeat {
    data <- tryCatch(.Call(RODBC:::C_RODBCFetchRows, attr(con, "handle_ptr"), n, buffsize, NA_character_, TRUE),
                     error=function(e) {
                       log_error(e)
                       return(list(stat=-3))
                     })
    if ((data$stat) < 0L) {
      if (data$stat == -3) {
        log_error(paste0("Interrupted Connection: ", odbcGetErrMsg(con)))
      }
      else if (data$stat == -2) {
        log_error(paste0("No Data: ", odbcGetErrMsg(con)))
      }
      else if (counter <= 0L) {
        log_error(odbcGetErrMsg(con))
      }
      else {
        log_info("Completed fetch (", nresults, " results)")
      }
      break
    }
    log_info("Fetching query results", 
             if (n > 0) { 
               paste(" (", floor(counter*n), "-", (counter+1)*n-1, ")", sep="") 
             }
             else {
               " (all)"
             }
    )
    counter <- counter + 1
    names(data$data) <- cData$names
    for (i in as.is) {
      tryCatch(
        switch(cData$type[i],
               int = data$data[[i]] <- as.integer(data$data[[i]]),
               smallint = data$data[[i]] <- as.integer(data$data[[i]]),
               decimal = data$data[[i]] <- as.numeric(data$data[[i]]),
               date = data$data[[i]] <- as.Date(data$data[[i]]),
               timestamp = data$data[[i]] <- as.POSIXct(data$data[[i]]),
               unknown = data$data[[i]] <- type.convert(data$data[[i]])
        ),
        error=function(e) {
          log_warn("Error converting ", cData$names[i], ": ", e$message)
        }
      )
    }
    tryCatch(
      if (use.dots) {
        forceAndCall(1, FUN, data$data, ...)
      }
      else {
        forceAndCall(1, FUN, data$data)
      },
      error=function(e) {
        log_error(e$message)
        log_error(odbcGetErrMsg(con))
        return(FALSE)
      }
    )
    nresults <- nresults + length(data$data[[1]])
  }
  return(TRUE)
}

################################################################################
################################################################################
################################################################################

### prepTargets()
### function to prepare a data frame holding table and column names to use in building a database query

prepTargets <- function(targetFile = NULL, dbName = NULL, con = NULL) {
  
  ### error handling
  ###### invalid con or no con
  ###### invalid file name or no file name
  ###### invalid db name or db user doesn't have permission for
  
  ### load file
  
  targets <- read.csv(targetFile, header = TRUE, stringsAsFactors = FALSE)
  
  ### change all instances of 'X' to null values (X was used as a placeholder in the file)
  
  for (x in 1:nrow(targets)) {
    targets[x, which(targets[x,] == 'X')] <- NA
  }
  
  ### determine which tables exist in the given database 
  
  for (target in 1:nrow(targets)) {
    query <- paste("SELECT NULLIF(COUNT(*), 0) AS TableExists
                 FROM ", dbName, ".INFORMATION_SCHEMA.TABLES
                 WHERE TABLE_SCHEMA = 'Src'
                 AND TABLE_NAME = '", targets[target,1], "'", sep = "")
    targets$TABLE.EXISTS[target] <- sqlQuery(con, query)
  }
  
  ### subset targets dataframe to contain only rows where TABLE.EXISTS == 1
  
  targets <- targets[which(targets$TABLE.EXISTS == 1),]
  rownames(targets) <- NULL
  
  return(targets)
  
}

################################################################################
################################################################################
################################################################################

### getCPT()
### function to retrieve CPT information from the VINCI CDW

getCPT <- function(con, dbName, targetFile, codeList = NULL, cohort = NULL, beginDate = NULL, endDate = NULL) {
  
  ### establish relevant tables to query in the specified database  
  
  targets <- prepTargets(targetFile, dbName, con)
  
  ### instantiate empty vector to hold target table queries
  
  query <- c()
  
  ### instantiate constants for target queries, if appropriate
  
  if (!is.null(beginDate) && !is.null(endDate)) {
    
    WHERE_dates <- paste(" BETWEEN '", beginDate, "' AND '", endDate, "'", sep = "")
    
  }
  
  ### build target table queries
  
  for (tar in 1:nrow(targets)) {
    
    SELECT <- "SELECT DISTINCT "
    
    FROM <- paste("FROM ", dbName, ".Src.", targets$TABLE_NAME[tar], sep = "")
    
    WHERE <- "WHERE "
    
    if (targets$REQUIRES_JOIN[tar] == 'Y') {
      
      JOIN <- paste(" a INNER JOIN ", dbName, ".Src.", targets$JOIN_TABLE[tar], " b", sep = "")
      JOIN <- paste(JOIN, " ON a.", targets$JOIN_FIELD[tar], " = b.", targets$JOIN_TABLE_JOIN_FIELD[tar], sep = "")
      
      FROM <- paste(FROM, JOIN, sep = "")
      
      SELECT <- paste(SELECT, "a.", targets$CPT_FIELD[tar], " AS CPTSID, a.PatientSID, b.", targets$DATE_FIELD[tar], " AS EventDateTime", sep = "")
      
      WHERE <- paste(WHERE, "b.", sep = "")
      
    } else {
      
      SELECT <- paste(SELECT, targets$CPT_FIELD[tar], " AS CPTSID, PatientSID, ", targets$DATE_FIELD[tar], "AS EventDateTime", sep = " ")
      
    }
    
    if (!is.null(beginDate) && !is.null(endDate)) {
      
      WHERE <- paste(WHERE, targets$DATE_FIELD[tar], WHERE_dates, sep = "")
      query[tar] <- paste(SELECT, FROM, WHERE, sep = " ")
      
    } else {
      
      query[tar] <- paste(SELECT, FROM, sep = " ")
      
    }
    
  }
  
  ### build total query
  
  SELECT <- "SELECT DISTINCT cc.PatientICN, x.EventDateTime, d.CPTCode, d.CPTName"
  inner_queries <- paste(query, collapse = " UNION ")
  FROM <- paste("FROM (", inner_queries, sep = "")
  JOIN <- paste(") x INNER JOIN ", dbName, ".Src.CohortCrosswalk cc ON x.PatientSID = cc.PatientSID", sep = "")
  JOIN <- paste(JOIN, " INNER JOIN CDWWork.Dim.CPT d ON x.CPTSID = d.CPTSID", sep = "")
  FROM <- paste(FROM, JOIN, sep = "")
  
  if (!is.null(codeList) && !is.null(cohort)) {
    
    WHERE <- paste("WHERE d.CPTCode IN (", codeList, ")", sep = "")
    WHERE <- paste(WHERE, " AND PatientICN IN (", cohort, ")", sep = "")
    query <- paste(SELECT, FROM, WHERE, sep = " ")
    
  } else if (!is.null(codeList)) {
    
    WHERE <- paste("WHERE d.CPTCode IN (", codeList, ")", sep = "")
    query <- paste(SELECT, FROM, WHERE, sep = " ")
    
  } else if (!is.null(cohort)) {
    
    WHERE <- paste("WHERE PatientICN IN (", cohort, ")", sep = "")
    query <- paste(SELECT, FROM, WHERE, sep = " ")
    
  } else {
    
    query <- paste(SELECT, FROM, sep = " ")
    
  }
  
  prepQuery(con, query)
  
  CPTdata <- as.data.frame(fetchQuery(con, query))
  
  CPTdata <- unique(CPTdata)
  
  return(CPTdata)
  
}

################################################################################
################################################################################
################################################################################

### getICD()
### function to retrieve ICD information from the VINCI CDW

getICD <- function(con, dbName, targetFile, codeList = NULL, cohort = NULL, beginDate = NULL, endDate = NULL) {
  
  
  
  ### establish relevant tables to query in the specified database 
  
  targets <- prepTargets(targetFile, dbName, con)
  
  ### instantiate empty vectors to hold target table queries
  
  query_9 <- c()
  query_10 <- c()
  
  ### instantiate constants for target queries, if appropriate
  
  
  if (!is.null(beginDate) && !is.null(endDate)) {
    
    WHERE_dates <- paste(" BETWEEN '", beginDate, "' AND '", endDate, "'", sep = "")
    
  }
  
  ### build target table queries
  
  for (tar in 1:nrow(targets)) {
    
    SELECT_9 <- paste("SELECT DISTINCT ", targets$ICD_9_FIELD[tar], " AS ICDSID,", sep = "")
    SELECT_10 <- paste("SELECT DISTINCT ", targets$ICD_10_FIELD[tar], " AS ICDSID,", sep = "")
    
    FROM <- paste("FROM ", dbName, ".Src.", targets$TABLE_NAME[tar], sep = "")
    
    WHERE <- "WHERE "
    
    if (targets$REQUIRES_JOIN[tar] == 'Y') {
      
      JOIN <- paste(" a INNER JOIN ", dbName, ".Src.", targets$JOIN_TABLE[tar], " b", sep = "")
      JOIN <- paste(JOIN, " ON a.", targets$JOIN_FIELD[tar], " = b.", targets$JOIN_TABLE_JOIN_FIELD[tar], sep = "")
      
      FROM <- paste(FROM, JOIN, sep = " ")
      
      SELECT <- paste(" b.PatientSID, b.", targets$DATE_FIELD[tar], " AS EventDateTime", sep = "")
      
      SELECT_9 <- paste(SELECT_9, SELECT, sep = "")
      SELECT_10 <- paste(SELECT_10, SELECT, sep = "")
      
      WHERE <- paste(WHERE, "b.", sep = "")
      
    } else {
      
      SELECT_9 <- paste(SELECT_9, "PatientSID,", targets$DATE_FIELD[tar], "AS EventDateTime", sep = " ")
      SELECT_10 <- paste(SELECT_10, "PatientSID,", targets$DATE_FIELD[tar], "AS EventDateTime", sep = " ")
      
    }
    
    if (!is.null(beginDate) && !is.null(endDate)) {
      
      WHERE <- paste(WHERE, targets$DATE_FIELD[tar], WHERE_dates, sep = "")
      query_9[tar] <- paste(SELECT_9, FROM, WHERE, sep = " ")
      query_10[tar] <- paste(SELECT_10, FROM, WHERE, sep = " ")
      
    } else {
      
      query_9[tar] <- paste(SELECT_9, FROM, sep = " ")
      query_10[tar] <- paste(SELECT_10, FROM, sep = " ")
      
    }
    
  }
  
  ### divide queries against target tables into batches of 5 and use loop to process in batches
  
  numTargets <- length(query_9)
  numBatches <- trunc(numTargets/5)
  if (numBatches < numTargets/5) { numBatches <- numBatches + 1 }
  lowerLimit <- as.integer(1)
  upperLimit <- as.integer(5)
  
  ### instantiate empty vectors to hold batched queries 
  
  batches_9 <- c()
  batches_10 <- c()
  
  ### instantiate constants for batched queries
  
  batchSELECT_9 <- "SELECT DISTINCT cc.PatientICN, x.EventDateTime, d1.ICD9Code AS ICDCode, d2.ICD9Description AS ICDDescription FROM ("
  batchSELECT_10 <- "SELECT DISTINCT cc.PatientICN, x.EventDateTime, d1.ICD10Code AS ICDCode, d2.ICD10Description AS ICDDescription FROM ("
  
  JOIN <- paste(") x INNER JOIN ", dbName, ".Src.CohortCrosswalk cc ON x.PatientSID = cc.PatientSID", sep = "")
  
  batchJOIN_9 <- paste(JOIN, "INNER JOIN CDWWork.Dim.ICD9 d1 ON x.ICDSID = d1.ICD9SID", sep = " ")
  batchJOIN_9 <- paste(batchJOIN_9, "INNER JOIN CDWWork.Dim.ICD9DescriptionVersion d2 ON d1.ICD9SID = d2.ICD9SID", sep = " ")
  
  batchJOIN_10 <- paste(JOIN, "INNER JOIN CDWWork.Dim.ICD10 d1 ON x.ICDSID = d1.ICD10SID", sep = " ")
  batchJOIN_10 <- paste(batchJOIN_10, "INNER JOIN CDWWork.Dim.ICD10DescriptionVersion d2 ON d1.ICD10SID = d2.ICD10SID", sep = " ")
  
  ### build batched queries
  
  for (batch in 1:numBatches) {
    
    if (upperLimit > numTargets) { upperLimit <- numTargets }
    
    this_9 <- paste(query_9[lowerLimit:upperLimit], collapse = " UNION ")
    this_10 <- paste(query_10[lowerLimit:upperLimit], collapse = " UNION ")
    
    if (!is.null(codeList) && !is.null(cohort)) {
      
      WHERE <- paste("WHERE PatientICN IN (", cohort, ")", sep = "")
      WHERE_9 <- paste(WHERE, " AND d1.ICD9Code IN (", codeList, ")", sep = "")
      WHERE_10 <- paste(WHERE, " AND d1.ICD10Code IN (", codeList, ")", sep = "")
      
      batches_9[batch] <- paste(batchSELECT_9, this_9, batchJOIN_9, WHERE_9, sep = " ")
      batches_10[batch] <- paste(batchSELECT_10, this_10, batchJOIN_10, WHERE_10, sep = " ")
      
    } else if (!is.null(codeList)) {
      
      WHERE_9 <- paste("WHERE d1.ICD9Code IN (", codeList, ")", sep = "")
      WHERE_10 <- paste("WHERE d1.ICD10Code IN (", codeList, ")", sep = "")
      
      batches_9[batch] <- paste(batchSELECT_9, this_9, batchJOIN_9, WHERE_9, sep = " ")
      batches_10[batch] <- paste(batchSELECT_10, this_10, batchJOIN_10, WHERE_10, sep = " ")
      
    } else if (!is.null(cohort)) {
      
      WHERE <- paste("WHERE PatientICN IN (", cohort, ")", sep = "")
      batches_9[batch] <- paste(batchSELECT_9, this_9, batchJOIN_9, WHERE, sep = " ")
      batches_10[batch] <- paste(batchSELECT_10, this_10, batchJOIN_10, WHERE, sep = " ")
      
    } else {
      
      batches_9[batch] <- paste(batchSELECT_9, this_9, batchJOIN_9, sep = " ")
      batches_10[batch] <- paste(batchSELECT_10, this_10, batchJOIN_10, sep = " ")
      
    }
    
    lowerLimit <- lowerLimit + 5
    upperLimit <- upperLimit + 5
    
  }
  
  ### tidy
  
  rm(query_9)
  rm(query_10)
  
  ### instantiate empty data frame to hold diagnostic data
  
  ICDdata <- data.frame(matrix(nrow = 0, ncol = 4, dimnames = list(NULL, c("PatientICN", "EventDateTime", "ICDCode", "ICDDescription"))))
  
  for (batch in 1:numBatches) {
    
    query9 <- batches_9[batch]
    query10 <- batches_10[batch]
    
    prepQuery(con, query9)
    
    data_9 <- as.data.frame(fetchQuery(con, query9))
    colnames(data_9) <- colnames(ICDdata)
    
    prepQuery(con, query10)
    
    data_10 <- as.data.frame(fetchQuery(con, query10))
    colnames(data_10) <- colnames(ICDdata)
    
    ICDdata <- rbind(ICDdata, data_9, data_10, stringsAsFactors = FALSE)
    
    rm(data_9)
    rm(data_10)
    
  }
  
  ### tidy
  
  rm(batches_9)
  rm(batches_10) 
  
  ICDdata <- unique(ICDdata)
  
  return(ICDdata)
  
}

################################################################################
################################################################################
################################################################################

### storeInHash()
### function to store data in an environment hash by given key
### parameter hash represents a previously instantiated environment hash
###### to do: add error checking, revise function to be able to handle other types of hashes?
### parameter variables is a vector that represents the names of the values (ie the column names of a SQL table or an R dataframe)
### parameter key represents the primary key or identifier AND MUST BE A MEMBER OF VARIABLES
###### to do: revise function to handle multiple keys per entity

storeInHash <- function(x, hash, key) {
  
  ### coerce input to data fram
  
  x <- as.data.frame(x)
  
  ### coerce all columns to character data
  
  for (cn in 1:length(colnames(x))) {
    
    x[, cn] <- as.character(x[, cn])
    
  }
  
  ### process data with inner function
  
  doProcess <- function(x) {
    
    thisData <- hash[[x[key]]]
    print(thisData)
                                                                                                                                                                                  
    if (is.null(thisData)) {
      
      hash[[x[key]]] <- as.data.frame(x[,])
      
    } else {
      
      newRow <- as.data.frame(x[,])

      hash[[x[key]]] <- rbind(thisData, newRow, stringsAsFactors = FALSE, make.row.names = FALSE)
      
    }
    
  }
  
  mapply(doProcess, x)
  
  return()  
  
}

################################################################################
################################################################################
################################################################################

### makeLUT()
### function to build framework to categorize inputs into outputs

### ADD NOTES ABOUT ASSUMED INPUT

makeLUT <- function(fileNameAndPath) {
 v
  ### prepare package dependencies

  suppressPackageStartupMessages(require("spatstat"))
  
  ### load file
  
  catFile <- read.csv(fileNameAndPath, header = TRUE, stringsAsFactors = FALSE)
  
  ### prepare lut function for categorizing inputs
  
  catLUT <- lut(inputs = catFile[,1], outputs = catFile[,2])
  
  return(catLUT)
  
}

################################################################################
################################################################################
################################################################################

### prepOutput()
### function to create an output file for generated results

prepOutput <- function(fileNameAndPath, fileHeader) {
  
  file.create(fileNameAndPath)
  fCon <- file(fileNameAndPath)
  fileHeader <- paste(fileHeader, " \n", sep = "")
  cat(fileHeader, file = fCon)
  return(fCon)
  
}

################################################################################
################################################################################
################################################################################

### getDemographics()
### function to retrieve patient demographic information from the VINCI CDW

### demographic data includes: date of birth, date of death, date of last follow up, sex, marital status, ...?
### The most reliable demographic data comes from the VitalStatus_Mini table, which uses SCRSSN as the patient identifier.

################################################################################
################################################################################
################################################################################

### makeCharlsonTables()
### function to amass all CDW identifiers that correspond to the ICD codes for comorbidities targeted by the Charlson Index, 
### in order to reduce the processing burden when assessing the comorbid conditions of a patient cohort.

makeCharlsonTables <- function(con) {
  
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
  
  ### prepare data frames to hold CDW identifiers (ICD9SID and ICD10SID values) for the ICD codes relevant to Charlson index
  ICD9SIDs <- data.frame(ICD9SID = numeric(), CharlsonCat = character())
  ICD10SIDs <- data.frame(ICD10SID = numeric(), CharlsonCat = character())
  
  ### retrieve all relevant CDW identifiers
  ### to make reusable: functionalize, with table and column names as parameters
  SELECTclause_9 <- "SELECT ICD9SID, '"
  FROMclause_9 <- "' AS CharlsonCat FROM CDWWork.Dim.ICD9 WHERE"
  SELECTclause_10 <- "SELECT ICD10SID, '"
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
    query_10 <- paste(query_10, WHEREclause_10, sep = "")
    
    prepQuery(con, query_9)
    results_9 <- as.data.frame(fetchQuery(con, query_9), stringsAsFactors = FALSE)
    ICD9SIDs <- rbind(ICD9SIDs, results_9)
    
    prepQuery(con, query_10)
    results_10 <- as.data.frame(fetchQuery(con, query_10), stringsAsFactors = FALSE)
    ICD10SIDs <- rbind(ICD10SIDs, results_10)
    
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
  
  sqlSave(con, ICD9SIDs, "CharlsonICD9SIDs")
  sqlSave(con, ICD10SIDs, "CharlsonICD10SIDs")
  
  return()
  
}

################################################################################
################################################################################
################################################################################

### calculateCharlsonScore()
### function to determine Charlson comorbidity score of patient(s) at any given time in the VA VINCI CDW

calculateCharlsonScore <- function(serverName, dbName, projectName, targetFile, cohortTable, cohortDate, outputFile, outputHeader) {
  
  ### instantiate logging file
  
  makeLog(dbName, projectName)
  
  ### instantiate database connection
  
  con <- makeCon(serverName, dbName)
  
  ### create temporary tables of SID values for ICD codes needed by Charlson index in database to ease processing burden
  
  makeCharlsonTables(con)
  
  ### create temporary table of patient identifiers and needed date of score to ease processing burden
  
  query <- paste("SELECT PatientICN, PatientSID, ", cohortDate, " INTO ComorbPats FROM ", cohortTable, " a INNER JOIN ", dbName, ".Src.CohortCrosswalk b", " ON a.PatientICN = b.PatientICN", sep = "")
  sqlQuery(con, query)
  
  ### establish relevant tables to query
  
  targets <- prepTargets(targetFile, dbName, con)
  
  ### instantiate environment hash
  
  envComorb <- new.env(hash = TRUE)
  
  ### instantiate storeInHash function
  
  storeInHash <- function(x) {
    
    x$PatientICN <- as.character(x$PatientICN)
    x$CharlsonCat  <- as.character(x$CharlsonCat)
    
    doProcess <- function(PatientICN, CharlsonCat) {
      
      if (is.null(envICD[[PatientICN]])) {
        
        envComorb[[PatientICN]] <- CharlsonCat
        
      } else if (!CharlsonCat %in% envComorb[[PatientICN]]) {
        
        envComorb[[PatientICN]] <- c(envComorb[[PatientICN]], CharlsonCat)
        
      }
      
    }
    
    mapply(doProcess, x$PatientICN, x$CharlsonCat)
    return()
    
  }
  
  ### retrieve patient data and populate hash environment
  
  ### build target table queries
  
  for (tar in 1:nrow(targets)) {
    
    SELECT <- "SELECT DISTINCT PatientICN, CharlsonCat,"
    
    FROM <- paste(" FROM ", dbName, ".Src.", targets$TABLE_NAME[tar], " a", sep = "")
    
    JOIN_ICD9 <- paste(" INNER JOIN CharlsonICD9SIDs c ON c.ICD9SID = a.", targets$ICD_9_FIELD[tar], sep = "")
    JOIN_ICD10 <- paste(" INNER JOIN CharlsonICD10SIDs c ON c.ICD10SID = a.", targets$ICD_10_FIELD[tar], sep = "")
    
    JOIN_Xwalk <- paste(" INNER JOIN ComorbPats d ON d.PatientSID = ", sep = "")
    
    if (targets$REQUIRES_JOIN[tar] == 'Y') {
      
      JOIN <- paste(" INNER JOIN ", dbName, ".Src.", targets$JOIN_TABLE[tar], " b", sep = "")
      JOIN <- paste(JOIN, " ON a.", targets$JOIN_FIELD[tar], " = b.", targets$JOIN_TABLE_JOIN_FIELD[tar], sep = "")
      
      FROM <- paste(FROM, JOIN, sep = "")
      
      JOIN_Xwalk <- paste(JOIN_Xwalk, "b.", sep = "")
      
    } 
    
    JOIN_Xwalk <- paste(JOIN_Xwalk, "PatientSID AND ", targets$DATE_FIELD[tar], " <= d.", cohortDate, sep = "")
    
    query_9 <- paste(SELECT, FROM, JOIN_ICD9, JOIN_Xwalk, sep = "")
    query_10 <- paste(SELECT, FROM, JOIN_ICD10, JOIN_Xwalk, sep = "")
    
    query <- paste(query_9, query_10, sep = " UNION ")
    
    ### tidy
    
    rm(query_9, query_10)   
    
    prepQuery(con, query)
    
    fetchQuery(con, FUN = storeInHash)
    
  }
  
  ### process patient data from environment hash
  
  ### prepare output file
  
  writeResults <- prepOutput(outputFile, outputHeader)
  
  ### instantiate list holding weight values for each Charlson category
  
  weights <- list(MI = 1, CHF = 1, PVasc = 1, CVasc = 1, Dem = 1, Pulm = 1, Rheum = 1, PUD = 1, MilLiv = 1, DiabCompNO = 1, DiabCompYES = 2, Plegia = 2, Renal = 2, Malig = 2, ModSevLiv = 3, Mets = 6, AIDS_HIV = 6)
  
  ### identify individual patients in environment
  
  pats <- ls(envir = envComorb)
  
  ### calculate score for each patient
  
  for (pat in pats) {
    
    ### start score at zero
    
    patScore <- 0
    
    ### retrieve this patient's data from environment hash
    
    thisPat <- envComorb[[pat]]
    
    if (!is.null(thisPat)) {
      
      for (dx in thisPat) {
        
        patScore <- patScore + weights[[dx]]
        
      }
      
    }
    
    ### output score
    
    vals <- paste(pat, patScore, sep = ",")
    vals <- paste(vals, " \n", sep = "")
    
    cat(vals, writeResults)
    
  }
  
  ### remove temporary tables
  
  query <- "DROP TABLE CharlsonICD9SIDs, CharlsonICD10SIDs, ComorbPats"
  sqlQuery(con, query)
  
  return()
  
}

