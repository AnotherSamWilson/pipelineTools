#' @title ordinalEncode
#' @description Performs ordinal encoding. I haven't seen this trick anywhere on the internet, but I haven't really looked.
#' Ordinal encoding is similar to dummy variables, but it 'stacks' the positive encoding at each succissive layer. Therefore,
#' If you are encoding education, and the options are "High School", "College", "PhD", someone has a PhD, then High School,
#' College, and PhD will all be 1. If someone went to college, then High School and College will be 1, PhD will be 0. This allows
#' models to succissively stack the effect of each new level, instead of treating it as a unique thing. This trick was shown to
#' me by a wise old data mage in Spain by the name of 'Blind Ape'.
#' @param orderings a named list containing vectors of the ordered levels (from least to greatest) that you wish to encode.
#' Names must be column names in dt, and elements in the vectors _should_ be valid levels in the data. If any element in the
#' vectors is not a valid level, the function will throw a warning and treat any unspecified levels according to newLevelHandling.
#' If you are using treatNA = 'newLevel' then you must specify where in the ordering NA values fall by including NA in each vector.
#' @param newLevelHandling How should we treat new levels in the future?
#' \itemize{
#'   \item \code{'fail'}   Throws an error.
#'   \item \code{'ghost'}  Encodes everything as the negative encoding (0, unless specified otherwise in values)
#' }
#' @param treatNA How should NAs be dealt with?
#' \itemize{
#'   \item \code{'newLevel'} Creates a new level (setNA). Any future NA values will be set to this. You must specify where in
#'   the ordering NA values fall if using this option. See orderings documentation.
#'   \item \code{'ghost'}    Encodes everything as the negative encoding (0, unless specified otherwise in values).
#'   \item \code{'fail'}     Fails the process if it encounters NA values.
#' }
#' @param setNA The value to set NAs to if treatNA = 'newLevel'
#' @param sep The seperator used in creating column names.
#' @param values The values to use in the encoding. Default is 0-1, because those are the most commonly used. Other values
#' may be desired based on activation functions, distance requirements, etc etc.
#' @return Ordinal Encoded Object. This needs to be applied to a dataset, it will not actually return a dataset.
#' @importFrom utils stack
#' @importFrom data.table data.table setnames :=
#' @export
ordinalEncode <- function(
  orderings
  , newLevelHandling = c("fail","ghost")
  , treatNA = c("newLevel","ghost","fail")
  , setNA = "na"
  , sep = "."
  , values = c(0,1)
) {

  # The initialized vector is just to easily show the available options. Take the first one.
  treatNA <- treatNA[[1]]
  newLevelHandling <- newLevelHandling[[1]]

  orderVars <- names(orderings)

  # Error handling. Add to this as you hit common user errors
  if (!is.character(setNA)) stop("setNA must be a character.")
  if (!newLevelHandling %in% c("fail","ghost")) stop("newLevelHandling not recognized")
  if (!treatNA %in% c("newLevel","ghost","fail")) stop("treatNA not recognized")

  # Define and return dummyDefs object
  ret <- list(
    orderings = orderings
    , orderVars = orderVars
    , newLevelHandling = newLevelHandling
    , treatNA = treatNA
    , setNA = setNA
    , sep = sep
    , values = values
  )
  class(ret) <- "ordinalDefs"
  return(ret)
}


#' @export
applyEncoding.ordinalDefs <- function(
    obj
  , dt
  , inPlace = TRUE
  , ...
) {

  if (any(!obj$orderVars %in% names(dt))) stop(paste0(obj$orderVars[!obj$orderVars %in% names(dt)]," were specified in orderings but are not names in dt."))

  # Keep these if we need to return them.
  if (inPlace) extraCols <- dt[,!obj$orderVars,with=FALSE]

  # data.table > data.frame
  dt <- copy(dt[,obj$orderVars, with = FALSE])

  # Factors make my life a living hell
  dt[,(obj$orderVars) := lapply(.SD, as.character), .SDcols = obj$orderVars]

  #obj$orderings$Foundation <- obj$orderings$Foundation[-1]
  dataLevels <- sapply(
    obj$orderVars
    , function(x) {
      a <- unique(dt[,get(x)])
      b <- obj$orderings[[x]]
      return(list(extraDT = setdiff(a,b),extraOrderings = setdiff(b,a)))
    }
    , simplify = FALSE
    , USE.NAMES = TRUE
  )
  extraDT <- sapply(dataLevels,function(x) x$extraDT)
  extraOrderings <- sapply(dataLevels,function(x) x$extraOrderings)

  if(obj$treatNA == 'newLevel' & any(is.na(extraDT))) {
    if (obj$newLevelHandling != "fail") cat(red("WARNING: Missing values found in data, but not ordering list. You have set treatNA = 'newLevel', so missing values will be treated according to newLevelHandling.\n"))
    if (obj$newLevelHandling == "fail") stop("Missing values were found in data, but not specified in the orderings. treatNA = 'newLevel', so they are being treated as new levels.. and newLevelHandling is set to 'fail'")
  }
  if(sum(lengths(extraDT)) > sum(is.na(extraDT))) {
    if (obj$newLevelHandling != "fail") cat(red("WARNING: At least 1 non-NA level in the data is missing from orderings list. These levels will be treated according to newLevelHandling.\n"))
    if (obj$newLevelHandling == "fail") stop("WARNING: At least 1 non-NA level in the data is missing from orderings list. Stopping process because newLevelHandling is set to 'fail'")
  }

  rm.na <- function(x) x[which(!is.na(x))]
  switchNA <- function(x,y) {
    x[which(is.na(x))] <- y
    return(x)
  }

  if (obj$treatNA == "newLevel") {
    # If setting NAs to a new level, simply replace all instances of NA with setNA.
    for(j in obj$orderVars) set(dt,which(is.na(dt[[j]])),j,obj$setNA) # Ridiculously fast https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
    obj$orderings <- sapply(
      obj$orderings
      , switchNA
      , obj$setNA
      , simplify = FALSE
      , USE.NAMES = TRUE
    )
  } else if (obj$treatNA == "ghost") {
    # If ghosting NAs, let the process below
    obj$orderings <- lapply(obj$orderings,rm.na)
  }

  # One of the rare times a for-loop is actually a good idea:
  for(x in obj$orderVars) {
    #x <- "FireplaceQu"
    dt[,(x):=switchNA(as.numeric(ordered(get(x),levels=obj$orderings[[x]])),-1)]
  }

  dat <- as.data.table(
    lapply(
      names(dt)
      , function(x) {
        encodedDT <- as.data.table(
          lapply(
            1:length(obj$orderings[[x]])
            , function(i) {
              ifelse(dt[,get(x)] >= i,obj$values[[2]],obj$values[[1]])
            }
          )
        )
        # Lowest ordinal variable will always be 1, so get rid of it.
        encodedDT[,V1 := NULL]
        setnames(encodedDT,paste0(x,obj$sep,obj$orderings[[x]][-1]))
        return(encodedDT)
      }
    )
  )

  if(inPlace) return(cbind(dat,extraCols)) else return(dat)

}
