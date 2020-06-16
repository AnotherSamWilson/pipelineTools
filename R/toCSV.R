#' @title toCSV
#' @description writes a data.table(frame) to a temp file
#' @param x the data.table(frame) to write
#' @param name if you want to give it a custom name. Otherwise, 16 byte string is used.
#' @return Opens a csv with whatever default program is selected in a new window
#' @importFrom utils write.table
#' @export
toCSV <- function (x, name = paste0(sample(LETTERS,16,replace = TRUE), collapse = "")) {

  # Need a temp file to write to
  tfp = paste(tempfile(), ".csv")

  # Make it a path
  tp = dirname(tfp)

  # Make a random collection of letters for the file path
  pf = paste0(name, ".csv")

  # Make it a path
  pfp = file.path(tp, pf)

  WriteAttempt = try(
    write.table(
      x
      , file=pfp
      , quote=TRUE
      , sep=","
      , na=""
      , row.names=FALSE
      , qmethod="double")
    , silent = TRUE
  )

  if ("try-error" %in% class(WriteAttempt)) {
    write.table(
      x
      , file=tfp
      , quote=TRUE
      , sep=","
      , na=""
      , row.names=FALSE
      , qmethod="double"
    )
    shell.exec(tfp)
  } else {
    shell.exec(pfp)
  }
}
