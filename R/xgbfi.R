#' @title xgbFI
#' @description take an xgboost model and perform feature importance with xgbfi.
#' @param model the data.table(frame) to write
#' @param xgbfiDirectory The directory that xgbfi lives in. You can just copy the fxgfi-master
#' directory into the same directory as your project and this will detect it.
#' @return Opens a csv with whatever default program is selected in a new window
#' @importFrom xgboost xgb.dump
#' @export
xgbFI <- function(
    model
  , xgbfiDirectory = paste0(getwd(),"/xgbfi-master")
) {

  #xgbfi https://github.com/Far0n/xgbfi

  # model <- readRDS("support/data/xgbTestModel.RDS")

  fNames <- sub("/",".",sub(" ",".",model$feature_names))
  if(!dir.exists(xgbfiDirectory)) stop("xgbfi could not be found, please put xgbfi-master in your project directory or specify the xgbfiDirectory parameter.")

  featureVector <- c()
  for (i in 1:length(fNames)) {
    featureVector[i] <- paste(i-1, fNames[i], "q", sep="\t")
  }
  fmp <- paste0(xgbfiDirectory,"/bin/fmap.txt")
  write.table(featureVector,fmp, row.names=FALSE, quote = FALSE, col.names = FALSE)
  xgb.dump(model = model, fname = paste0(xgbfiDirectory,"/bin/xgb.dump"), fmap = fmp, with_stats = TRUE)

  cat("Looks like it ran successfuly, cd to xgbfi-master/bin and run XgbFeatureIteractions.exe in terminal to get excel file.")

}
