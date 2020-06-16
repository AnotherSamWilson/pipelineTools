#' boxCoxEncode
#' @param dt Dataset to create object on.
#' @param vars variables you want to include in the encoding.
#' @param lambda You can pass custom lambdas if you want. Not recommended.
#' @param minNormalize Box-Cox is a _risky_ transformation because it will fail if
#' it encounters a number <= 0. You can reduce this _riskyness_ by adding a certain amount of
#' 'space' between your expected range and 0. \code{minNormalize} represents the number of
#' standard deviations you want between 0 and the minimum number (lower bound) in the distribution.
#' This is set higher to ensure the variable never experiences a future number <= 0. Usually
#' safe being set pretty low if you have lots of data. If you have done some engineering
#' yourself to ensure this never happens, can be set to 0. All variables are automatically re-scaled,
#' Can either be a scalar or a named list of values, with names equal to vars.
#' @param capNegPredOutliers If you weren't careful enough with minNormalize and some
#' negative values end up coming through, do you want to cap them before they hit boxCox, or throw an error?
#' Safer to throw an error, so it's set to 0 by default. Then results in \code{applyEncoding}
#' trying to perform boxCox on 0, which will fail. If not 0, this number represents the number
#' of standard deviations above 0 that the numbers will be (min) capped at. Should be lower than minNormalize,
#' or the results will no longer be in the same order, since negative values will now be greater than the
#' minimum sample this encoding was created on.
#' @importFrom stats optim sd
#' @return BoxCox Encoded Object
#' @export
boxCoxEncode <- function(
  dt
  , vars
  , lambda = NULL
  , minNormalize = 0.05
  , capNegPredOutliers =0
) {

  # Testing
  # require(data.table)
  # dt <- readRDS("Data/numericEncodings.RDS")
  # vars <- c("LotFrontage","LotArea","GarageCars","BsmtFinSF2")
  # lambda <- 1
  # lambda <- NULL
  # minNormalize = 0.05
  # capNegPredOutliers <- 0.01

  # Data fidelity - damn there's a lot of checks for this one.
  oops <- which(!sapply(dt[,vars,with=FALSE],is.numeric))
  if(length(oops) > 0) stop(paste0("The following variables are not numeric or integer: ",names(oops)))

  # minNormalize Checks
  if(length(minNormalize) == 1 & !class(minNormalize) %in% c("numeric","integer")) stop("varMin must be a scalar or a named list of numbers.")
  if(!class(unlist(minNormalize)) %in% c("numeric","integer")) stop("minNormalize must be a scalar or a named list of numbers.")
  if(length(minNormalize) > 1) if(any(!names(minNormalize) %in% names(dt))) stop("names of minNormalize must be variables in dt.")
  if (length(minNormalize) == 1){
    if (!class(minNormalize) %in% c("numeric","integer")) stop("minNormalize needs to be a number.")
    minNormalize <- lapply(dt[,vars,with=FALSE],function(x) minNormalize)
  }
  if(any(unlist(minNormalize) < 0.001)) warning("minNormalize shouldn't be too low, this may cause wild transformations if values are close to 0.")


  if(!class(unlist(capNegPredOutliers)) %in% c("numeric","integer")) stop("capNegPredOutliers must be a scalar or a named list of numbers.")
  if(length(capNegPredOutliers) > 1) if(any(!names(capNegPredOutliers) %in% names(dt))) stop("names of capNegPredOutliers must be variables in dt.")
  if (length(capNegPredOutliers) == 1){
    if (!class(capNegPredOutliers) %in% c("numeric","integer")) stop("capNegPredOutliers needs to be a number.")
    capNegPredOutliers <- lapply(dt[,vars,with=FALSE],function(x) capNegPredOutliers)
  }
  if(any(unlist(minNormalize) < unlist(capNegPredOutliers))) stop(paste0("minNormalize needs to be greater than capNegPredOutliers."))


  # Calculate sd of distribution for each variable
  vecSD <- lapply(dt[,vars,with=FALSE],sd,na.rm=TRUE)

  # Calculate addition to range before it hits Box-Cox transformation.
  vecAdd <- lapply(vars,function(x) minNormalize[[x]]*vecSD[[x]] - min(dt[[x]],na.rm = TRUE))
  names(vecAdd) <- vars


  # If no lambda was passed, find lambda that results in 0 skewness of the variable's distribution
  if(is.null(lambda)) {

    boxCox <- function(x,lam,add) {
      if(lam==0) return(log(x+add)) else return(((x+add)^lam-1)/lam)
    }

    optimize <- function(lam,var) {
      e1071::skewness(boxCox(dt[[var]],lam,vecAdd[[var]]), na.rm = TRUE)^2
    }

    # var <- "LotFrontage"
    lambdas <- lapply(
      vars
      , function(var) optim(
        par = 0
        , fn = optimize
        , var = var
        , method = "L-BFGS-B"
        , lower = -5
        , upper = 5
      )$par
    )

    names(lambdas) <- vars

  } else if(length(lambda) == 1){

    if(!class(lambda) %in% c("numeric","integer")) stop("lambda must be a number")
    lambdas <- lapply(vars,function(x) lambda)
    names(lambdas) <- vars

  } else {

    if(length(lambda) != length(vars)) stop("What did you pass to lambda?????")
    if(!class(unist(lambda)) %in% c("numeric","integer")) stop("Lambda must be NULL, a named list of numbers, or a single number to use as lambda for all vars.")

    lambdas <- lambda

  }

  boxCoxObj <- list()
  boxCoxObj$vars <- vars
  boxCoxObj$vecAdd <- vecAdd
  boxCoxObj$vecSD <- vecSD
  boxCoxObj$lambdas <- lambdas
  boxCoxObj$minNormalize <- minNormalize
  boxCoxObj$capNegPredOutliers <- capNegPredOutliers

  class(boxCoxObj) <- "boxCoxDefs"
  return(boxCoxObj)

}

applyBoxCox <- function(
  dt
  , obj
  , inPlace
) {

  # Testing
  # obj <- boxCoxEnc
  # dt <- numericEncodings
  # inPlace = FALSE

  # Data fidelity
  if (any(!obj$vars %in% names(dt))) stop(paste0(obj$vars[!obj$vars %in% names(dt)]," are not names in dt."))

  # Specify columns to return
  if (inPlace) {
    dt <- copy(dt)
  } else {
    dt <- copy(dt[,obj$vars, with = FALSE])
  }

  boxCox <- function(y,lam,add,SD,cap,var) {

    # y <- dt[[x]]
    # lam <- obj$lambdas[[x]]
    # add <- obj$vecAdd[[x]]
    # SD <- obj$vecSD[[x]]
    # cap <- obj$capNegPredOutliers[[x]]
    # var <- x

    y1 <- pmax(y + add,cap*SD)

    if(any(y1 <= 0,na.rm = TRUE)) stop(paste0("Tried to apply box-cox on value <= 0 for variable ",var))

    if(lam==0) return(log(y1)) else return((y1^lam-1)/lam)

  }

  # Perform boxcox on vars.
  # x <- names(dt)[[1]]
  dt[,(obj$vars) := lapply(obj$vars,function(x) {

    boxCox(dt[[x]],obj$lambdas[[x]],obj$vecAdd[[x]],obj$vecSD[[x]],obj$capNegPredOutliers[[x]],x)

  })]

  return(dt)

}
