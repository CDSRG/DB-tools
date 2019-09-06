#playing with some ODBC code to see if can access table structures, etc.

describeTable <- function(con, table, columns) {
#	odbcGetErrMsg(con)
#	odbcGetInfo(con)
	col_types <- sqlTypeInfo(con)
	columns1 <- sqlColumns(con, table)
#	columns2 <- sqlQuery(con,
#		paste(
#			"SELECT COLUMN_NAME, COLUMN_DEFAULT, DATA_TYPE, IS_NULLABLE ",
#			"FROM INFORMATION_SCHEMA.COLUMNS ",
#			"WHERE TABLE_NAME=N'", table, "'",
#			sep="")
#		)
#	keys <- sqlPrimaryKeys(con, table)
	colcheck <- tryCatch(
		match.arg(columns, columns1[,"COLUMN_NAME"], several.ok=TRUE),
		error=function(e) {
				warning(paste("error evaluating query '", columns, "' using provided DB connection", sep=""))
				warning(e)
				return()			
		}
	)
	print(colcheck)
}