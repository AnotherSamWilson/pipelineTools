% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/densityPlotting.R
\name{plot2dContour}
\alias{plot2dContour}
\title{plot2dContour}
\usage{
plot2dContour(dat, x, y, grouping)
}
\arguments{
\item{dat}{the data.table(frame) that contains the data.}

\item{x}{The variable on the x axis. Should be a character.}

\item{y}{The variable on the y axis. Should be a character.}

\item{grouping}{Will create multiple contours broken up by this variable. Should be a character name of the column you want to use.}
}
\value{
a ggplot.
}
\description{
Plots a 2D density plot, with optional grouping. Use with caution, this can get messy.
}
