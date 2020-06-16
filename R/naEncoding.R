#' @title naEncode
#' @description Replaces NA and NaN values in \code{dt[vars]}. Will add a new level to factors.
#' @param dt dataset of class `data.table` or `data.frame`
#' @param vars The variables in dt you want to impute. If NULL, uses all columns.
#' @param naReplaceNumber replacement number for numerics.
#' @param naReplaceCharacter replacement string for characters and factors.
#' @param inPlace should the entire dataset be returned, or just the variables in \code{vars}
#' @param verbose Print results?
#' @export
naEncode <- function(
  dt
  , vars = names(dt)
  , naReplaceNumber = NULL
  , naReplaceCharacter = NULL
  , inPlace = TRUE
  , verbose = T
){

  # Testing
  # require(data.table)
  # dt <- readRDS("Data/catEncoding.RDS")
  # dt$untouched <- rnorm(nrow(dt))
  # vars <- c("Foundation","FireplaceQu","GarageCars","Street","FoundationFactr")
  # i <- "FoundationFactr"
  # i <- "Foundation"
  # dt$FireplaceQu <- factor(dt$FireplaceQu)


  # Returning vars or all columns.
  if (inPlace) {
    dt <- copy(dt)
  } else {
    dt <- copy(dt[,names(obj), with = FALSE])
  }

  # Make sure vars actually exist in dt.
  if (any(!vars %in% names(dt))) stop(paste0(vars[!vars %in% names(dt)]," are not names in dt."))

  # Replace NaN with NA
  dt[,(vars) := lapply(.SD,function(x){x[is.nan(x)] <- NA;return(x)}), .SDcols = vars]

  # Do this iteratively
  for(i in vars) {

    # Check for NAs
    misNum <- sum(is.na(dt[[i]]))
    if(misNum == 0) {
      if(verbose) cat("No missing values in",i,"no changes made to this variable.")
      next
    } else {if(verbose) cat("Imputing",misNum,"values in",i)}

    # If everything looks good, perform the character imputation
    if(class(dt[[i]]) %in% c("character","factor")) {
      if(is.null(naReplaceCharacter)) stop(paste0("Tried to impute ",i," but no naReplaceCharacter supplied."))
      dt[,(i) := ifelse(is.na(get(i)),naReplaceCharacter,as.character(get(i)))]
    } else if(class(dt[[i]]) %in% c("numeric","integer")) {
      if(is.null(naReplaceNumber)) stop(paste0("Tried to impute ",i," but no naReplaceNumber supplied."))
      dt[,(i) := ifelse(is.na(get(i)),naReplaceNumber,get(i))]
    }

  }

  return(dt)

}
