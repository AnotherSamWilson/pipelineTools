#' @title makeFeatEngTools
#' @description Creates and stores functions for feature engineering.
#' @param dat The data
#' @param holdOut A second dataset to test performance. Not yet implemented.
#' @param response The response column
#' @param runs How many times to repeat
#' @param plot Plot the results
#' @param ... Other arguments to pass to ranger.
#' @importFrom ranger ranger
#' @importFrom data.table melt copy setDT
#' @importFrom ggplot2 ggplot geom_boxplot ylab ggtitle aes
#' @importFrom stats reorder
#' @return A matrix of prediction.errors for each run and variable.
#' @export
rangerFeatureImportance <- function(
    dat
  , holdOut
  , response
  , runs = 10
  , plot = TRUE
  , ...
) {

  # library(data.table)
  # library(ranger)
  # library(ggplot2)
  # dat <- iris
  # response <- "Sepal.Width"

  if (runs <= 5 & plot) stop("runs must be at least 5 to plot")

  dat <- copy(dat)
  setDT(dat)
  vars <- setdiff(names(dat),response)

  varTypes <- sapply(dat[,vars,with=FALSE],class)

  for (v in names(varTypes[varTypes == "character"])) {
    set(dat,j=v,value=as.factor(dat[,v,with=FALSE]))
  }

  baseModelList <-lapply(
    1:runs
    , function(r) {
      baseModel <- ranger(
        data = dat
        , dependent.variable.name = response
        , splitrule = "extratrees"
        , ...
      )
    }
  )

  varModelList <- lapply(
    1:runs
    , function(b) {
      lapply(
        vars
        , function(v) {
          ranger(
              data = dat[,-v,with=FALSE]
            , dependent.variable.name = response
            , splitrule = "extratrees"
            , ...
          )
        }
      )
    }
  )

  OOBerrors <- t(sapply(varModelList,function(x) sapply(x,function(m) m$prediction.error)))
  colnames(OOBerrors) <- vars
  OOBerrors <- cbind(
    baseModel = sapply(baseModelList,function(m) m$prediction.error)
    , OOBerrors
  )

  if (plot) {
    plotDT <- melt(data.table(OOBerrors),measure.vars = colnames(OOBerrors))
    print(
      ggplot(plotDT,aes(x=reorder(variable,value,mean),y=value)) +
        geom_boxplot() +
        ylab("prediction.error") +
        ggtitle("Prediction Error when Each Variable was Removed") +
        theme(axis.text.x = element_text(angle = 45)) +
        xlab("Variable")
    )
  }

  return(OOBerrors)

}
