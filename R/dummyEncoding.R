
#' @title applyEncoding
#' @param obj an encoding object created by one of the encoding functions in this package
#' @param ... other arguments dependent on object class
#' @rdname applyEncoding
#' @export
applyEncoding <- function(obj, ...) UseMethod("applyEncoding")


#' @title dummyEncode
#' @description Collects data about levels and user preferences to transform dataset into dummy variables.
#' @param dt data.frame(table) to create the object on
#' @param vars vector of variables you want to dummify
#' @param treatNA A string that specifies what you want to do with NA values. It is basically never a good
#' idea to have NA dummy variables, so that is not an option. Options are:
#' \itemize{
#'   \item \code{"newLevel"}   Simply creates a new level, which will be set to 1 when the variable is NA
#'   \item \code{"ghost"}      Sets all levels to 0. The information that this variable was NA is encoded into the data by the fact that none of the other levels are equal to 1. Bad idea for linear models.
#' }
#' @param sep The seperator between variable and level in the new column names. "." is safe usually.
#' @param setNA What to replace NA with if \code{treatNA = "newLevel"}.
#' @param fullRank Boolean. Copies carat syntax. If TRUE, the least common level is dropped so that linear models won't return blown up (yet valid) coefficients. A good conversation: https://stats.stackexchange.com/questions/231285/dropping-one-of-the-columns-when-using-one-hot-encoding
#' @param levelCountThresh Did you try to one-hot encode a floating point column? If your level count exceeds this value, the process stops and let you know which column it was.
#' @param values What to encode the values as. Should be a numeric vector of the form c(False value,Positive value).
#' Default one-hot encoding values are c(0,1). Sometimes, it is useful to encode as c(-1,1) for certain
#' NN activation functions.
#' @return Frequency Encoded Object. This needs to be applied. It will not actually return a dataset.
#' @importFrom utils stack head
#' @importFrom data.table data.table setnames :=
#' @export
dummyEncode <- function(
    dt
  , vars
  , treatNA = c("newLevel","ghost")
  , sep = "."
  , setNA = "na"
  , fullRank = TRUE
  , levelCountThresh = 50
  , values = c(0,1)
) {

  # Testing
  # require(data.table)
  # require(FDhelperFuncs)
  # dt <- readRDS("Support/Data/catEncodings.RDS")
  # dt$untouched <- rnorm(nrow(dt))
  # vars <- c("Foundation","FireplaceQu","GarageCars","Street","FoundationFactr")
  # treatNA <-  c("newLevel","ghost")
  # setNA = "na"
  # levelCountThresh = 50
  # fullRank = TRUE
  # sep = "."
  #
  # dum <- dummyEncode(dt,vars)
  # applyEncoding(dum,dt,TRUE)
  # FDhelperFuncs:::applyEncoding(dum,dt,TRUE)

  # data.table > data.frame
  dt <- copy(dt[,vars, with = FALSE])

  # The initialized vector is just to easily show the available options. Take the first one.
  treatNA <- treatNA[[1]]

  # Error handling. Add to this as you hit common user errors
  if (any(!vars %in% names(dt))) stop(paste0(vars[!vars %in% names(dt)]," are not names in dt."))

  # Useful Info
  lens <- sapply(dt[,vars,with=FALSE],function(x) length(unique(x)))

  # Check for levelCountThresh
  tooBig <- which(lens > levelCountThresh)
  if (length(tooBig) > 0) stop(paste0("The following variables have unique level counts exceeding levelCountThresh: ",paste0(names(tooBig),collapse = ", ")))

  # Don't dummy a variable with only 1 level.
  tooSmall <- which(lens < 2)
  if (length(tooSmall) > 0) stop(paste0("The following variables have only 1 level, please fix: ",paste0(names(tooSmall),collapse = ", ")))

  # Factors make my life a living hell
  dt[,(vars) := lapply(.SD, as.character), .SDcols = vars]

  # Impute missing vars.
  if (treatNA == "newLevel") for(j in vars) set(dt,which(is.na(dt[[j]])),j,setNA) # Ridiculously fast https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table

  # Get distinct values and counts
  freqs <- frequencyEncode(dt, vars = names(dt), encodeNA = TRUE)

  # Edit freq object - this setup is not too bad, small adjustment needed for what dummyVars expects
  # Need to filter out anything that doesn't actually exist.
  freqs$tables <- lapply(freqs$tables, function(x) x[freq > 0,])

  # If using fullRank, remove the least common level.
  if (fullRank == TRUE) freqs$tables <- lapply(freqs$tables, function(x) head(x,-1))

  # Get unique names - store as a vector.
  unqs <- lapply(freqs$tables, function(x) x[[2]])

  # If ghosting the NAs, just remove them from the possible values.
  if (treatNA == "ghost") unqs <- lapply(unqs, function(x) x[!is.na(x)])

  # Get new col names
  lvlNames <- lapply(names(unqs), function(x) paste(x, unqs[[x]], sep = sep))
  names(lvlNames) <- names(unqs)
  newNames <- as.character(unlist(lvlNames))

  # Define and return dummyDefs object
  ret <- list(
    newNames = newNames
    , lvlNames = lvlNames
    , oldNames = vars
    , sep = sep
    , Uniques = unqs
    , treatNA = treatNA
    , setNA = setNA
    , values = values
  )
  class(ret) <- "dummyDefs"
  return(ret)
}

#' @export
applyEncoding.dummyDefs <- function(
    obj
  , dt
  , inPlace = TRUE
  , ...
) {

  # Testing
  # obj <- dummyEnc2
  # dt <- copy(catEncWithNewLevels)
  # inPlace <- FALSE


  dt <- copy(dt)

  # Data fidelity
  if (any(!obj$oldNames %in% names(dt))) stop(paste0(obj$oldNames[!obj$oldNames %in% names(dt)]," were included in the dummy object, but are not column names in dt."))

  # Factors make my life a living hell
  dt[,(obj$oldNames) := lapply(.SD, as.character), .SDcols = obj$oldNames]

  if (obj$treatNA == "newLevel") {

    for(j in obj$oldNames) set(dt,which(is.na(dt[[j]])),j,obj$setNA) # Ridiculously fast https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table

  }

  colOver <- lapply(obj$newNames, function(x) {
    loc <- regexpr(obj$sep, x, fixed = TRUE)
    return(c(substr(x,1,loc-1),substr(x,loc+1,nchar(x))))
  })

  # Ghosting the NAs just means every dummy column will be 0. No new level
  # is created, the information is just encoded as "not true" (0) for all columns.
  if (obj$treatNA == "ghost") {
    cols <- lapply(colOver, function(x) {
      j <- ifelse(dt[[x[[1]]]] == x[[2]],obj$values[[2]],obj$values[[1]])
      return(ifelse(is.na(j),obj$values[[1]],j))
    })
  } else {
    cols <- lapply(colOver, function(x) {
      ifelse(dt[[x[[1]]]] == x[[2]],obj$values[[2]],obj$values[[1]])
    })
  }

  setDT(cols)
  setnames(cols, obj$newNames)

  if (inPlace) {
    return(cbind(dt[,!obj$oldNames,with=FALSE],cols))
  } else {
    return(cols)
  }
}
