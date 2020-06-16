#' @title plot2dContour
#' @description Plots a 2D density plot, with optional grouping. Use with caution, this can get messy.
#' @param dat the data.table(frame) that contains the data.
#' @param x The variable on the x axis. Should be a character.
#' @param y The variable on the y axis. Should be a character.
#' @param grouping Will create multiple contours broken up by this variable. Should be a character name of the column you want to use.
#' @return a ggplot.
#' @importFrom ggplot2 aes_string ggplot geom_point geom_density_2d ggtitle
#' @export
plot2dContour <- function(dat,x,y,grouping) {
  if(missing(grouping)) {
    newDat <- dat[get(x) != -1 & get(y) != -1 & !is.na(get(x)) & !is.na(get(y)),]
    p <- ggplot(
      newDat
      , aes_string(x=x,y=y)
    ) +
      geom_point(alpha = min(1/nrow(dat)*5000,1)) +
      geom_density_2d(size = 1) +
      ggtitle(paste0("2D density of ",x," and ",y,"."))
  } else {
    newDat <- dat[get(x) != -1 & get(y) != -1  & !is.na(get(x)) & !is.na(get(y)) & !is.na(get(grouping)),]
    p <- ggplot(
      newDat
      , aes_string(x=x,y=y,colour=as.factor(newDat[,get(grouping)]))
    ) +
      geom_point(alpha = min(1/nrow(dat)*5000,1)) +
      geom_density_2d(size = 1) +
      ggtitle(paste0("2D density of ",x," and ",y," by ",grouping))
    p$labels$colour <- grouping
  }
  return(p)
}

#' @title plot2dContour
#' @description Plots the density of a variable, with optional grouping.
#' @param dat the data.table(frame) that contains the data.
#' @param x The variable on the x axis. Should be a character.
#' @param grouping Will create multiple contours broken up by this variable. Should be a character name of the column you want to use.
#' @return a ggplot.
#' @importFrom ggplot2 aes_string ggplot geom_density ggtitle
#' @export
plot1dDensity <- function(dat,x,grouping) {
  if(missing(grouping)) {
    newDat <- dat[get(x) != -1 & !is.na(get(x)),]
    p <- ggplot(
      newDat
      , aes_string(x=x,y=y)
    ) +
      geom_density(size = 1) +
      ggtitle(paste0("Density of ",x,"."))
  } else {
    newDat <- dat[get(x) != -1 & !is.na(get(x)) & !is.na(get(grouping)),]
    p <- ggplot(
      newDat
      , aes_string(x=x,colour=as.factor(newDat[,get(grouping)]))
    ) +
      geom_density(size = 1) +
      ggtitle(paste0("Density of ",x," by ",grouping))
    p$labels$colour <- grouping
  }
  return(p)
}
