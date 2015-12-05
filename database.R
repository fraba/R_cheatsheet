# SQLite

## Database connection

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

## Database search



