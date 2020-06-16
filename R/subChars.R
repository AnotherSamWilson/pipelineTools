#' @title subChars
#' @description removes a list of characters and replaces them with another.
#' @param pattern The character(s) you want to replace.
#' @param sub The string you want to replace them with.
#' @param x The character vector you want to do the replacement on.
#' @return Opens a csv with whatever default program is selected in a new window
#' @importFrom xgboost xgb.dump
#' @export
subChars <- function(pattern,sub,x) {
  if (length(sub) > 1) stop("Use multiple subChar functions for multiple replacement strings.")
  for (i in pattern) x <- gsub(i,sub,x)
  return(x)
}
