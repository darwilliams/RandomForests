#Accuracy Assessment for merged building, trees and other classes tiles

#### INIT ------------------------------------------------------
rm(list=ls())  ## remove all variables

start.message <- sprintf("Metro Vancouver landcover classification, started running on %s", Sys.time())
print(start.message)

setwd("D:\\RandomForests")

#### LOAD PACKAGES ----------------------------------------------------------

list.of.packages <- c("caret",
                      "raster",
                      "rgeos",
                      "rgdal",
                      "sp",
                      "spdep",
                      "spatstat",
                      "gplots",
                      "ggplot2",
                      "plyr",
                      "dplyr", ## to be loaded before foreach to avoid "assertion failed" errors
                      "magrittr",
                      "rlist",
                      "lazyeval",
                      "randomForest",
                      "rgl",
                      "vegan",
                      "snow",
                      "lubridate", 
                      "doParallel", 
                      "foreach",
                      "data.table",
                      "tidyverse",
                      "stringr"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]   ## named vector members whose name is "Package"
if(length(new.packages)) install.packages(new.packages)   ## install only unavailable packages
for (pack in list.of.packages){
  library(pack, character.only=TRUE)  ## call all the packages in list.of.packages
}


## returns F-measure for each class and global Kappa statistic
classif.metrics <- function(predicted, observed) {
  if ( length(levels(observed)) > 2 & length(unique(observed)) > 2) { ## handle the multi-class case
    RES <- confusionMatrix(predicted, observed)
    PA <- RES$byClass[, "Sensitivity"] ## Producer's accuracy, 1 - omission error, TP/(observed total)
    UA <- RES$byClass[, "Pos Pred Value"] ## User's accuracy, 1 - commission error, TP/(predicted total)
  } else if ( length(levels(observed)) == 2 | length(unique(observed)) == 2 ) { ## handle the binary class case
    RES <- confusionMatrix(predicted, observed, positive=levels(predicted)[1]) ## first run assessment setting first class as positive
    PA <- RES$byClass[["Sensitivity"]] ## save producers accuracy...
    UA <- RES$byClass[["Pos Pred Value"]] ## ...and users accuracy
    RES <- confusionMatrix(predicted, observed, positive=levels(predicted)[2]) ## then repeat with second class
    PA <- c(PA, RES$byClass[["Sensitivity"]]) ## and complete PA vector
    UA <- c(UA, RES$byClass[["Pos Pred Value"]]) ## and UA vector
  } else {
    stop("Observed values: less than 2 levels or levels to be updated") 
  }
  return(list( Kappa=as.vector(RES$overall[2]), Fmeas=(2*PA*UA)/(PA+UA), ConfMat=RES$table))
}