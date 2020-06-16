#' @title frequencyEncode
#' @description Re-encodes categorical data as its frequency in the dataset. Useful for gradient boosting.
#' Does NOT return dataset, but an object that can be applied to a dataset with the \code{applyEncoding} function.
#' If your data contains missing values, be very careful with the \code{encodeNA} and \code{allowNewLevels} parameters.
#' @param dt data.frame(table) to create the object on
#' @param vars vector of variables you want to frequency-encode
#' @param encodeNA Boolean. Should NAs be encodes as a frequency, or kept as NA when the transformation is applied? If
#' there are no NAs in your original data, new NAs will still be encoded as 1. Risky, but easy.
#' @param allowNewLevels Should any new levels be encoded as -1? Details:
#' \itemize{
#'   \item \code{TRUE}   Encodes new levels as -1. This is dangerous if your levels can change in the future, because you won't notice, and the model may not be tuned correctly.
#'   \item \code{FALSE}  Throws an error. You'll need to figure out how you want to proceed if \code{allowNewLevels = TRUE} is not good enough.
#' }
#' @return Frequency Encoded Object. This needs to be applied to a dataset, it will not actually return a dataset.
#' @importFrom utils stack
#' @importFrom data.table data.table setnames :=
#' @export
frequencyEncode <- function(
    dt
  , vars
  , encodeNA = FALSE
  , allowNewLevels = FALSE
) {

  # Testing
  # require(data.table)
  # dt <- catEncoding
  # vars <- c("Foundation","FireplaceQu","GarageCars","Street","FoundationFactr")
  # encodeNA = FALSE
  # allowNewLevels <- FALSE

  if (any(!vars %in% names(dt))) stop(paste0(vars[!vars %in% names(dt)]," are not names in dt."))
  if (encodeNA) table <- function(x) base::table(x,useNA = "ifany")

  dt <- copy(dt[,vars, with = F])

  encodList <- list()
  encodList$vars <- vars
  encodList$encodeNA <- encodeNA
  encodList$allowNewLevels <- allowNewLevels
  encodList$tables <- list()

  for (i in vars) {

    # i <- vars[[2]]

    # Needs to be handle all 4 cases of encodeNA and allowNewLevels,
    # As well as when NA is a new level.
    # FALSE:FALSE
    # FALSE:TRUE
    # TRUE:FALSE
    # TRUE:TRUE

    # Create a frequency table
    freqTab <- data.table(stack(table(dt[[i]])))[order(-get("values"))]

    # Do we need to add NA, or has it already been added?
    addNA <- !any(is.na(freqTab$ind))

    # table manipulations
    freqTab$ind <- as.character(freqTab$ind)
    setnames(freqTab, c("freq",i))
    freqTab[,"enc" := nrow(freqTab):1]

    # Add NA and new level specifications
    if (addNA & encodeNA) freqTab <- rbindlist(list(freqTab,list(0,NA,0)))
    if (addNA & !encodeNA) freqTab <- rbindlist(list(freqTab,list(0,NA,NA)))
    if (allowNewLevels) freqTab <- rbindlist(list(freqTab,list(0,"__NEWLEVEL__",-1)))
    if (!allowNewLevels) freqTab <- rbindlist(list(freqTab,list(0,"__NEWLEVEL__",NA)))

    encodList$tables[[i]] <- freqTab

  }

  class(encodList) <- "freqDefs"

  return(encodList)

}

#' @export
applyEncoding.freqDefs <- function(
    obj
  , dt
  , inPlace = TRUE
  , ...
) {

  # Testing
  # dt <- catEncoding
  # obj <- freqEncod_TRUE
  # inPlace = TRUE
  # dt <- rbindlist(list(dt,list("New","New","-1","Street","New",0)))

  # Specify columns to return
  if (inPlace) {
    dt <- copy(dt)
  } else {
    if (any(!obj$vars %in% names(dt))) stop(paste0(obj$vars[!obj$vars %in% names(dt)]," are not names in dt."))
    dt <- copy(dt[,obj$vars, with = FALSE])
  }

  for (i in obj$vars) {

    # i <- obj$vars[[2]]

    if(!class(dt[[i]]) %in% c("character")) dt[,(i) := as.character(get(i))]


    if(any(!unique(dt[[i]]) %in% obj$tables[[i]][[i]])) {
      if(obj$allowNewLevels) {
        warning(paste0("WARNING: NEW LEVEL DETECTED IN VARIABLE ",i,". allowNewLevels IS SET TO TRUE, SO THESE WILL BE ENCODED AS newString or -1."))
        dt[[i]][which(!dt[[i]] %in% obj$tables[[i]][[i]])] <- "__NEWLEVEL__"
      } else {
        stop(paste0("NEW LEVEL DETECTED IN VARIABLE ",i,". allowNewLevels IS SET TO FALSE, PROCESS STOPPING."))
      }
    }

    enc <- merge(
      x = dt[,(i),  with = FALSE]
      , y = obj$tables[[i]][,-"freq"]
      , sort = FALSE
      , all.x = TRUE
    )

    dt[,(i) := enc$enc]

  }

  return(dt)

}
