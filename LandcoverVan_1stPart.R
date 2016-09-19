#### INIT --------------------------------------------------------------------

rm(list=ls())

start.message <- sprintf("Vancouver landuse/landcover classification, started running on %s", Sys.time())
print(start.message)

#### PARAMETERS ---------------------------------------------

params <- list()

## General
params$GT.type <- c("one")   ## type of GT (to be put in a loop to see both results)
# params$GT.type <- c("onefivematch")
params$predictor.types <- c("all", "spectral", "LiDAR")

## RF
params$predictors.all <- c("Border_ind", "Bright_vis", "Building", "Coef_Var_n", "Compactnes", "Density", "Elliptic_F", "GLCM_Contr", "GLCM_Con_1", "GLCM_Homog", "GLCM_Hom_1",
                           "Imagery_Br", "LengthThic", "LengthWidt", "MaxHtMinHt", "Mean_nDSM", "Mean_nDSMS", "Mean_slope", "Mean_zDev", "NDRE", "NDVI", "NDVIRE", "NIRRE",
                           "Rectangula", "Rel_border", "Rel_bord_1", "Rel_bord_2", "Roundness", "SAVI", "sd_ndsm", "sd_slope", "Shrub", "Standard_d", "Thickness_", "Trees")   ## list of all starting predictors 
# params$predictors.spectral <- 
# params$predictors.LiDAR <- 
params$targ <- "CLASS"     ## target variable of the classification (column name of dataframe)
params$seed <- 2016        ## seed to have same RF result
params$parallel.RF <- T    ## whether to run RF in parallel or not
params$ntree <- 100     ## RF nr of trees
params$mtry <- 'sqrt_nr_var'  ## how to set RF mtry: 'sqrt_nr_var' or 'nr_var_div_3'
params$nodesize <- 1   ## RF nodesize: default for classification is 1
params$plot.importance <- F  ## whether to plot RF variable importance

base.dir <- 'D:/Research/ANALYSES/LandcoverVan'    ## base working directory

points.filename <- "Van_unclassified_SJ_unambig"
points.folder <- "Van_unclassified_spatialJoin"
objects.filename <- "Vancouver_unclassified_final_v8_SMALL"
objects.folder <- "Van_unclassified_objects"


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
                      "data.table"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]   ## named vector members whose name is "Package"
if(length(new.packages)) install.packages(new.packages)   ## install only unavailable packages
for (pack in list.of.packages){
  library(pack, character.only=TRUE)  ## call all the packages in list.of.packages
}

#### FUNCTIONS ----------------------------------------------------------

## returns F-measure for each class and global Kappa statistic
classif.metrics <- function(predicted, observed) {
  RES <- confusionMatrix(predicted, observed)
  sens <- RES$byClass[, "Sensitivity"]
  spec <- RES$byClass[, "Specificity"]
  return(data.frame(Fmeas=(2*sens*spec)/(sens+spec), Kappa=as.vector(RES$overall[2])))
}

#### START --------------------------------------------------------------

tic <- proc.time() ## start clocking global time

data.dir <- file.path(base.dir, "Data", fsep = .Platform$file.sep)   ## data directory (nothing should be written here)
results.dir <- file.path(base.dir, "Results", fsep = .Platform$file.sep)   ## results directory (outputs go here)
figures.dir <- file.path(base.dir, "Figures", fsep = .Platform$file.sep)   ## figures directory (figures go here)
# temp.dir <- file.path(data.dir, "temp")  ## directory for temporary files like the segmentation shps (overwritten each time)
# if (!file.exists(temp.dir)) {dir.create(temp.dir, showWarnings=F, recursive=T)}  ## create it

## read shapefiles
objects.path <- file.path(data.dir, objects.folder, fsep = .Platform$file.sep) 
points.path <- file.path(data.dir, points.folder, fsep = .Platform$file.sep) 
objects.raw <- readOGR(dsn=objects.path, layer=objects.filename) ## smallest polyg subset, the one with only 9 plots/polyg
points.raw <- readOGR(dsn=points.path, layer=points.filename) ## smallest polyg subset, the one with only 9 plots/polyg

## filter points to keep only the desired GT level ("one" or "onefivematch")
if (params$GT.type == "one") {
  points <- subset(points.raw, Shape_Area < 10)
  class.col <- "VanSubset1"
} else if (params$GT.type == "onefivematch") {
  points <- subset(points.raw, VanSubset1 == VanSubse_6)  ## watch out for names (to be changed)!
  class.col <- "VanSubse_6"
}

## solution with union() from Raster package which has more options (not working!)
# objects <- gBuffer(objects, byid=TRUE, width=0)
# points <- gBuffer(points, byid=TRUE, width=0)
# union.res <- union(objects, points)

points.w.values <- over(points, objects.raw)  # [, "ECOZONE_NA"]  ## over() outputs a df with the values of the fields in ecozones.UTMreproj at the locations of lidar

points.merged <- cbind(points@data, points.w.values)  ## stack together GT columns and predictor values
compl.dataset <- points.merged %>% filter(!is.na(Border_ind))  ## remove NAs (points falling outiside of polygons)

# XXXXXXXXXXXXXX
# points@data already contains predictor values: they will not be there with real data, right?
# XXXXXXXXXXXXXX

#### PRINT LOGS ---------------------------------------------------------

## clock global time
toc <- proc.time()-tic[3]
end.message <- sprintf("Total elapsed time: %s, finished running on %s", seconds_to_period(toc[3]), Sys.time())
print(end.message)




