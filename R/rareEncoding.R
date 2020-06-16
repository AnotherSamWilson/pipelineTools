#' @title rareEncode
#' @description Groups rare levels together, where 'rare' is defined by either minPerc or minCount.
#' @param dt Dataset to create object on.
#' @param vars variables you want to include in the encoding.
#' @param minPerc Minimum percentage of the total population required for a group to be considered 'rare'.
#' For example, if \code{minPerc = 0.1}, and \code{Gender = 'Female'} only makes up 5% of the dataset, then
#' \code{Female} would be replaced with the string in \code{newString}.
#' @param minCount Can be provided instead of minPerc if you have a better idea of credibility in terms of total
#' samples required. Levels with counts below this value will be grouped into \code{newstring}.
#' @param newString Value you want to group rare levels together as.
#' @param encodeNA If FALSE, NAs will remain NA even if they would have been grouped.
#' If TRUE, NAs will be grouped, but only if they are 'rare'. If they are not rare, NAs will be untouched.
#' @param allowNewLevels Similar to frequencyEncode parameter. This groups all new levels in with \code{newString}.
#' @return Rare Encoded Object
#' @export
rareEncode <- function(
  dt
  , vars
  , minPerc = NULL
  , minCount = NULL
  , newString = "rareGroup"
  , encodeNA = FALSE
  , allowNewLevels = FALSE
) {

  # Testing
  # require(data.table)
  # dt <- catEncoding
  # vars <- c("Foundation","FireplaceQu","GarageCars","Street","FoundationFactr")
  # minPerc = 0.05
  # minCount = NULL
  # encodeNA = FALSE
  # allowNewLevels = TRUE
  # newString = "rareGroup"


  rareObj <- list()
  rareObj$vars <- vars
  rareObj$minPerc <- minPerc
  rareObj$minCount <- minCount
  rareObj$newString <- newString
  rareObj$encodeNA <- encodeNA
  rareObj$allowNewLevels <- allowNewLevels
  rareObj$tables <- list()

  # Cannot provide both minPerc and minCount
  if((is.null(minPerc) & is.null(minCount)) | (!is.null(minPerc) & !is.null(minCount))) stop("Provide only 1 of minPerc and minCount")

  # Create minPerc from minCount
  if (is.null(minPerc)) minPerc <- minCount/nrow(dt)

  # Need sensible bounds
  if(minPerc <= 0 | minPerc >= 1) stop("minPerc represents a percentage, and should be between 0 and 1.")

  # A convenient shortcut.
  freqObj <- frequencyEncode(dt, vars, encodeNA = encodeNA, allowNewLevels = allowNewLevels)

  rareObj$tables <- lapply(freqObj$tables, function(x) {
    x$freq <- x$freq/sum(x$freq)
    if(encodeNA) x$enc <- ifelse(x$freq < minPerc, newString, as.character(x[[2]]))
    if(!encodeNA) x$enc <- ifelse(x$freq < minPerc & !is.na(x[[2]]), newString, as.character(x[[2]]))
    return(x)
  }
  )

  class(rareObj) <- "rareDefs"

  return(rareObj)

}

#' @export
applyEncoding.rareDefs <- function(
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
