#' gaussianEncode
#' @param dt Dataset to create object on.
#' @param vars variables you want to include in the encoding.
#' @param newMean The mean of the transformed distribution
#' @param newSD The SD of the transformed distribution
#' @param encodeNA You can impute NAs as a manual value, or leave them as NA.
#' @return Rare Encoded Object
#' @export
gaussianEncode <- function(
  dt
  , vars
  , newMean = 0
  , newSD = 1
  , encodeNA = NA
) {

  # Testing
  # require(data.table)
  # dt <- readRDS("Data/numericEncodings.RDS")
  # vars <- c("LotFrontage","LotArea","GarageCars","BsmtFinSF2")

  oops <- which(!sapply(dt[,vars,with=FALSE],is.numeric))
  if(length(oops) > 0) stop(paste0("The following variables are not numeric or integer: ",names(oops)))


  # data fidelity
  if (any(!vars %in% names(dt))) stop(paste0(vars[!vars %in% names(dt)]," are not names in dt."))

  params <- lapply(vars,function(x) return(list(oldMean = mean(dt[[x]], na.rm = TRUE),oldSD = sd(dt[[x]], na.rm = TRUE))))
  names(params) <- vars

  gaussianObj <- list()
  gaussianObj$vars <- vars
  gaussianObj$newMean <- newMean
  gaussianObj$newSD <- newSD
  gaussianObj$params <- params
  gaussianObj$encodeNA <- encodeNA
  class(gaussianObj) <- "gaussianDefs"

  return(gaussianObj)

}

#' @export
applyEncoding.gaussianDefs <- function(
    obj
  , dt
  , inPlace = TRUE
  , ...
) {

  # Testing
  # obj <- gaussEnc
  # dt <- readRDS("Data/numericEncodings.RDS")
  # inPlace <- TRUE

  if (any(!obj$vars %in% names(dt))) stop(paste0(obj$vars[!obj$vars %in% names(dt)]," are not names in dt."))

  # Specify columns to return
  if (inPlace) {
    dt <- copy(dt)
  } else {
    dt <- copy(dt[,obj$vars, with = FALSE])
  }

  dt[,(obj$vars) := lapply(obj$vars,function(x) (dt[[x]] - obj$params[[x]]$oldMean)/(obj$params[[x]]$oldSD / obj$newSD) + obj$newMean)]

  return(dt)

}
