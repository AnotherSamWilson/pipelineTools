#' @title getQuery
#' @description Placeholder
#' @param email Whatever UID you sign into SQL server with.
#' @param driver Try leaving empty first.
#' You can use ```odbc::odbcListDrivers()``` to see which
#' drivers you have ifthe default doesn't work.
#' @param server The server
#' @param database Need to specify a connection to the database.
#' @param sql SQL string to run.
#' @return data.table results from sql
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbClearResult dbDisconnect
#' @importFrom data.table as.data.table
#' @importFrom odbc odbc
#' @importFrom crayon green
#' @export
getQuery <- function(
    email
  , driver = "ODBC Driver 17 for SQL Server"
  , server
  , database
  , sql
) {

  cat(green("If prompted, please enter your password in the Microsoft login window...\n"))

  # Create odbcConnection object
  con <- dbConnect(
    odbc()
    , driver = driver
    , server = server
    , database = database
    , authentication = "ActiveDirectoryInteractive"
    , uid = email
    , encrypt = "Yes"
  )

  cat(green("Query started executing at",as.character(Sys.time()),"\n"))

  # Create odbcResult object. This executes the query in SQL server but does not return any results.
  rs <- dbSendQuery(con, sql)

  # Run fetch result object and store result in fetchedrows
  fetchedrows <- dbFetch(rs)

  # Delete result object
  dbClearResult(rs)

  # Disconnect
  dbDisconnect(con)

  return(as.data.table(fetchedrows))

}


#' @title sendTable
#' @description Upload a table to SQL Server
#' @param email Whatever UID you sign into SQL server with.
#' @param driver Try leaving empty first.
#' You can use ```odbc::odbcListDrivers()``` to see which
#' drivers you have ifthe default doesn't work.
#' @param server The server
#' @param database Need to specify a connection to the database.
#' @param schema The schema you are sending the table to.
#' @param tableName The name the table will have in the database
#' @param dt The table in your R session you want to upload.
#' @param newDataTypes A named vector passed to field.types in DBI::dbWriteTable
#' @param append Should it be unioned to a current table?
#' @param overwrite Should you overwrite an existing table. USE WITH CAUTION.
#' @return Nothing, this uploads a table to SQL server.
#' @importFrom odbc odbc
#' @importFrom DBI dbWriteTable Id dbDisconnect dbConnect
#' @importFrom data.table data.table
#' @importFrom crayon green
#' @export
sendTable <- function(
    email
  , driver = "ODBC Driver 17 for SQL Server"
  , server
  , database
  , schema
  , tableName
  , dt
  , append = TRUE
  , overwrite = FALSE # Oh lawd you better know what you're doing
  , newDataTypes = NULL
) {

  if(!is.null(newDataTypes)) {
    if(any(sort(names(newDataTypes)) != sort(names(dt)))) stop("names of newDataTypes must match names of dt")
  }

  cat(green("If prompted, please enter your password in the Microsoft login window...\n"))

  con <- dbConnect(
    odbc()
    , driver = driver
    , server = server
    , database = database
    , authentication = "ActiveDirectoryInteractive"
    , uid = email
    , encrypt = "Yes"
  )

  cat(green("Uploading table started at",as.character(Sys.time()),"\n"))

  DBI::dbWriteTable(
    conn = con
    , name = DBI::Id(schema = schema, table = tableName)
    , value = dt
    , append = append
    , overwrite = overwrite
    , field.types = newDataTypes
  )

  dbDisconnect(con)

}
