% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xgbfi.R
\name{xgbFI}
\alias{xgbFI}
\title{xgbFI}
\usage{
xgbFI(model, xgbfiDirectory = paste0(getwd(), "/xgbfi-master"))
}
\arguments{
\item{model}{the data.table(frame) to write}

\item{xgbfiDirectory}{The directory that xgbfi lives in. You can just copy the fxgfi-master
directory into the same directory as your project and this will detect it.}
}
\value{
Opens a csv with whatever default program is selected in a new window
}
\description{
take an xgboost model and perform feature importance with xgbfi.
}
