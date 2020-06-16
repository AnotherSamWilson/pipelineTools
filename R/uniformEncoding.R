#' @title uniformEncode
#' @description Puts data from 1 range into another using a linear mapping. i.e. map data in {0-1} to {0-100}
#' @param dt Dataset to create object on.
#' @param vars variables you want to include in the encoding.
#' @param oldRange The old range of the variable. This can be user specified if you believe your data does not encompass the entire true range.
#' @param newRange the new range the map the values to. This can either be a vector, or a named list of vectors if you want different ranges for each variable (not typical).
#' @param encodeNA Values to set NA to. Can be a scalar or a named list of scalars (different values for each variable) if you are doing something complicated.
#' If TRUE, NAs will be grouped, but only if they are 'rare'. If they are not rare, NAs will be untouched.
#' @return Rare Encoded Object
#' @export
uniformEncode <- function(
  dt
  , vars
  , oldRange = lapply(dt[,vars,with=FALSE],function(x) return(c(min(x,na.rm=TRUE),max(x,na.rm=TRUE))))
  , newRange = c(0,1)
  , encodeNA = NA
) {

  # Testing
  # require(data.table)
  # dt <- readRDS("Data/numericEncodings.RDS")
  # vars <- c("LotFrontage","LotArea","GarageCars","BsmtFinSF2")
  # oldRange2 = c(10,20)
  # newRange = c(0,1)
  # encodeNA = NA
  # encodeNA = 0

  # data fidelity
  if (any(!vars %in% names(dt))) stop(paste0(vars[!vars %in% names(dt)]," are not names in dt."))
  testRanges <- function(x) {
    #x <- oldRange
    if(!class(unlist(x)) %in% c("numeric","integer")) stop("Range must be numeric")
    if(!length(unlist(x)) == 2) {
      if(!any(lapply(x,length) == 2)) stop("Range must be a list of vectors of length 2")
      if(!any(names(x) %in% names(dt))) stop(paste0(names(x)[!names(x) %in% names(dt)]," are not names in dt."))
      if(!setequal(names(x),vars)) stop("Names in Range are not the same as vars")
    }
  }
  testRanges(oldRange)
  testRanges(newRange)


  # Make into a list if they are not already.
  if(length(unlist(oldRange)) == 2) {
    oldRange <- lapply(vars,function(x) oldRange)
    names(oldRange) <- vars
  }
  if(length(unlist(newRange)) == 2) {
    newRange <- lapply(vars,function(x) newRange)
    names(newRange) <- vars
  }

  # Janky, but this needs to ocur after we blow up oldRange and newRange
  if (!setequal(vars,names(oldRange))) stop("Names in vars and oldRange not the same.")
  if (!setequal(vars,names(newRange))) stop("Names in vars and newRange not the same.")

  outsideRange <- sapply(vars,function(x) length(which(dt[[x]] < oldRange[[x]][[1]] | dt[[x]] > oldRange[[x]][[2]]))) > 0
  if(any(outsideRange)) warning(paste0("The following variables had values in the data outside the oldRange. This will result in values outside newRange: ",paste0(names(outsideRange)[outsideRange],collapse = ", ")))

  uniformObj <- list()
  uniformObj$vars <- vars
  uniformObj$oldRange <- oldRange
  uniformObj$newRange <- newRange
  uniformObj$encodeNA <- encodeNA
  uniformObj$params <- list()

  class(uniformObj) <- "uniformDefs"

  return(uniformObj)

}

#' @export
applyEncoding.uniformDefs <- function(
    obj
  , dt
  , inPlace = TRUE
  , ...
) {

  # Testing
  # obj <- uniEncod
  # dt <- numericEncodings
  # inPlace <- TRUE
  # inPlace <- FALSE

  dt <- copy(dt)

  if (any(!obj$vars %in% names(dt))) stop(paste0(obj$oldNames[!obj$oldNames %in% names(dt)]," were included in the uniform object, but are not column names in dt."))

  # Make the obvious warning
  outsideRange <- sapply(obj$vars,function(x) length(which(dt[[x]] < obj$oldRange[[x]][[1]] | dt[[x]] > obj$oldRange[[x]][[2]]))) > 0
  if(any(outsideRange)) warning(paste0("The following variables had values in the data outside the oldRange. This will result in values outside newRange: ",paste0(names(outsideRange)[outsideRange],collapse = ", ")))

  # Apply linear transformation - This could _definitely_ be made prettier.
  dt[,(obj$vars) := lapply(obj$vars,function(x) {
    or <- obj$oldRange[[x]]
    nr <- obj$newRange[[x]]
    orr <- or[[2]]-or[[1]]
    nrr <- nr[[2]]-nr[[1]]
    (dt[[x]] - or[[1]]) / orr * nrr + nr[[1]]
  }
  )]

  if(inPlace) return(dt) else return(dt[,obj$vars,with=FALSE])

}
