##
# Connection
##

##
# Connect to SQL server using connection string
##
ConnSqlServer <- function(db_obj){
  conn <- DBI::dbConnect(drv = odbc::odbc(),
                         Driver = "ODBC Driver 17 for SQL Server",
                         Server = db_obj$srv,
                         Database = db_obj$dbn,
                         UID = db_obj$id,
                         PWD = db_obj$pwd)
  return(conn)
}

##
# Connect to MariaDB using connection string
##
ConnMySql <- function(db_obj){
  conn <- DBI::dbConnect(drv = RMariaDB::MariaDB(),
                         user = db_obj$id,
                         password = db_obj$pwd,
                         dbname = db_obj$dbn,
                         host = db_obj$srv,
                         port = db_obj$prt)
  return(conn)
}

##
# Read a table from sel server db
##
ReadDataFromSS <- function(db_obj, tbl_name){
  conn <- ConnMySql(db_obj)
  df <- DBI::dbReadTable(conn, tbl_name)
  DBI::dbDisconnect(conn)  
  return(df)
}

##
# Write a table to sql server db
##
WriteDataToSS <- function(db_obj, data, tbl_name, apd = FALSE){
  conn <- ConnMySql(db_obj)
  df <- DBI::dbWriteTable(conn, name = tbl_name, value = data,
                          append = apd, overwrite = !apd, row.names = FALSE)
  DBI::dbDisconnect(conn) 				  
  return(df)
}

##
# List all tables and queries
##
ListTblsFromSS <- function(db_obj){
  conn <- ConnMySql(db_obj)
  dfs_tn <- DBI::dbListTables(conn, scheme = "dbo")
  DBI::dbDisconnect(conn)
  return(dfs_tn)
}

##
# Send query to db
##
GetQueryResFromSS <- function(db_obj, qry_str){
  conn <- ConnMySql(db_obj)
  qry_conn <- DBI::dbSendQuery(conn, qry_str)
  res <- DBI::dbFetch(qry_conn)
  DBI::dbClearResult(qry_conn)
  DBI::dbDisconnect(conn)
  return(res)
}