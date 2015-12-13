# SQLite

sqliteGetTable <- function(database, table) {
  require(DBI)
  require(RSQLite)
  con <- dbConnect(RSQLite::SQLite(), dbname = database)
  query <- dbSendQuery(con, paste("SELECT * FROM ", table, ";", sep="")) 
  result <- fetch(query, n = -1)
  dbClearResult(query)
  dbDisconnect(con)
  return(result)
}

sqliteListTables <- function(database, table) {
  require(DBI)
  require(RSQLite)
  con <- dbConnect(RSQLite::SQLite(), dbname = database)
  return(dbListTables(con))
}

sqliteWriteTable <- function(database, table_name, df) {
  require(DBI)
  require(RSQLite)
  con <- dbConnect(RSQLite::SQLite(), dbname = database)
  dbWriteTable(con, table_name, df)
}





