#### INIT ------------------------------------------------------
rm(list=ls())  ## remove all variables

start.message <- sprintf("Vancouver landuse/landcover classification, started running on %s", Sys.time())
print(start.message)

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

#### PARAMETERS ---------------------------------------------

params <- list()

## General
params$GT.types <- c("one", "onefivematch")   ## type of GT (to be put in a loop to see both results)
# params$predictor.types <- c("all", "spectral", "LiDAR")
params$predictor.types <- c("all")
params$predictors.all <- c("Border_ind", "Bright_vis", "Building", "Coef_Var_n", "Compactnes", "Density", "Elliptic_F", "GLCM_Contr", "GLCM_Con_1", "GLCM_Homog", "GLCM_Hom_1",
                           "Imagery_Br", "LengthThic", "LengthWidt", "MaxHtMinHt", "Mean_nDSM", "Mean_nDSMS", "Mean_slope", "Mean_zDev", "NDRE", "NDVI", "NDVIRE", "NIRRE",
                           "Rectangula", "Rel_border", "Rel_bord_1", "Rel_bord_2", "Roundness", "SAVI", "sd_ndsm", "sd_slope", "Shrub", "Standard_d", "Thickness_", "Trees")   ## list of all starting predictors 
# params$predictors.spectral <- 
# params$predictors.LiDAR <-

## RF
params$nfold <- 4
params$seed <- 2016        ## seed to have same RF result
params$parallel.RF <- T    ## whether to run RF in parallel or not
params$ntree <- 100     ## RF nr of trees
params$mtry <- 'sqrt_nr_var'  ## how to set RF mtry: 'sqrt_nr_var' or 'nr_var_div_3'
params$nodesize <- 1   ## RF nodesize: default for classification is 1
params$plot.importance <- F  ## whether to plot RF variable importance

base.dir <- 'D:/RandomForests'    ## base working directory

points.filename <- "VanSubsetPoints_Buffer_SJ_unambig"
points.folder <- "Vancouver/shp"
objects.filename <- "Vancouver_unclassified_final_v9"
objects.folder <- "Vancouver/shp"

#### FUNCTIONS ----------------------------------------------------------

## returns F-measure for each class and global Kappa statistic
classif.metrics <- function(predicted, observed) {
  RES <- confusionMatrix(predicted, observed)
  sens <- RES$byClass[, "Sensitivity"]
  spec <- RES$byClass[, "Specificity"]
  return(list( Kappa=as.vector(RES$overall[2]), Fmeas=(2*sens*spec)/(sens+spec), ConfMat=RES$table))
}

#### READ DATA --------------------------------------------------------------

tic <- proc.time() ## start clocking global time

data.dir <- file.path(base.dir, "Data", fsep = .Platform$file.sep)   ## data directory (nothing should be written here)
results.dir <- file.path(base.dir, "Results", fsep = .Platform$file.sep)   ## results directory (outputs go here)
figures.dir <- file.path(base.dir, "Figures", fsep = .Platform$file.sep)   ## figures directory (figures go here)
# temp.dir <- file.path(data.dir, "temp")  ## directory for temporary files like the segmentation shps (overwritten each time)
# if (!file.exists(temp.dir)) {dir.create(temp.dir, showWarnings=F, recursive=T)}  ## create it

## read shapefiles
objects.path <- file.path(data.dir, objects.folder, fsep = .Platform$file.sep) 
points.path <- file.path(data.dir, points.folder, fsep = .Platform$file.sep) 
# objects.raw <- readOGR(dsn=objects.path, layer=objects.filename) ## smallest polyg subset, the one with only 9 plots/polyg
points.raw <- readOGR(dsn=points.path, layer=points.filename) ## smallest polyg subset, the one with only 9 plots/polyg

plot(objects.raw)

# clip artefacts away from unclassified object edge using lidar boundary
van_lidar_boundary <- readOGR(dsn ="D:\\RandomForests\\Data\\Vancouver\\shp", layer ="Vancouver_nDSM_domain")
plot(van_lidar_boundary)
# now to clip using sp::over
objects_backup <- objects.raw
objects_clip <- objects_backup[van_lidar_boundary,] # this is equivalent to...
# sel <- over(objects_backup, van_lidar_boundary)
# objects_clip <- stations_backup[!is.na(sel[,1]),]
plot(objects_clip)

# change column names to be meaningful for points and objects
names (points.raw) [11:24]
c <- c("Point_Number","Onem_Class_1_1st_choice","Onem_Class_1_2nd_choice","Onem_Class_2_1st_choice",
       "Onem_Class_2_2nd_choice", "Onem_Class_3_1st_choice", "Onem_Class_3_2nd_choice",
       "Fivem_Class1_1st_choice","Fivem_Class1_2nd_choice","Fivem_Class_2_1st_choice",
       "Fivem_Class_2_2nd_choice","Fivem_Class_3_1st_choice","Fivem_Class_3_2nd_choice",
       "Classifier_notes")
names (points.raw) [11:24] <- c
names(points.raw)

names(objects.raw) #this one is fine

# fix any broken names - similar to below but use the other classes
# change shrub to trees and correct other spelling errors
# t1_ref_1m_long$class  <- gsub(x = t1_ref_1m_long$class,pattern = "Tree_canopy", replacement = "Trees")
# t1_ref_1m_long$class  <- gsub(x = t1_ref_1m_long$class,pattern = "tree_Canopy", replacement = "Trees")
# t1_ref_1m_long$class  <- gsub(x = t1_ref_1m_long$class,pattern = "Tree_Canopy", replacement = "Trees")
# t1_ref_1m_long$class  <- gsub(x = t1_ref_1m_long$class,pattern = "Shrub", replacement = "Trees")
# t1_ref_1m_long$class  <- gsub(x = t1_ref_1m_long$class,pattern = "shrub", replacement = "Trees")
# t1_ref_1m_long$class  <- gsub(x = t1_ref_1m_long$class,pattern = "Buildings", replacement = "Building")
# t1_ref_1m_long
# t1_ref_1m_long$class
# # remove null values and name column
# t1_ref_1m_long <- t1_ref_1m_long %>% select(-name) %>% filter(class != "<NA>", class != " ", class != "")
# t1_ref_1m_long
# 
# t1_ref_5m_long$class  <- gsub(x = t1_ref_5m_long$class,pattern = "Tree_canopy", replacement = "Trees")
# t1_ref_5m_long$class  <- gsub(x = t1_ref_5m_long$class,pattern = "tree_Canopy", replacement = "Trees")
# t1_ref_1m_long$class  <- gsub(x = t1_ref_1m_long$class,pattern = "Tree_Canopy", replacement = "Trees")
# t1_ref_5m_long$class  <- gsub(x = t1_ref_5m_long$class,pattern = "Shrub", replacement = "Trees")
# t1_ref_1m_long$class  <- gsub(x = t1_ref_5m_long$class,pattern = "shrub", replacement = "Trees")
# t1_ref_5m_long$class  <- gsub(x = t1_ref_5m_long$class,pattern = "Buildings", replacement = "Building")
# t1_ref_5m_long
# t1_ref_5m_long$class


# ## initialize empty factor matrix with appropriate levels to store final class predictions at each round of the Leave-one-out cross-validation loop for each scenario
# Y.predicted <- data.table(
#   replicate(
#     length(params$GT.types)*length(params$predictor.types),
#     factor( 
#       rep(NA, nrow(compl.dataset)),
#       levels=levels(compl.dataset[, class.col]) 
#     )
#   )
# )


RES <- list()  ## initialize list object to store results
for (gt.type in params$GT.types) {  ## loop over the predictor types
  
  ## filter points to keep only the desired GT level ("one" or "onefivematch")
  if (gt.type == "one") {
    points <- subset(points.raw, Shape_Area < 10)
    class.col <- "VanSubse_2"
  } else if (gt.type == "onefivematch") {
    points <- subset(points.raw, VanSubset1 == VanSubse_6)  ## watch out for names (to be changed)!
    class.col <- "VanSubse_8"
  }
  
#### SPATIAL JOIN --------------------------------------------------------------
  
  ## solution with union() from Raster package which has more options (not working!)
  # objects <- gBuffer(objects, byid=TRUE, width=0)
  # points <- gBuffer(points, byid=TRUE, width=0)
  # union.res <- union(objects, points)
  
  ## -------- TO UNCOMMENT
  ## spatial join the polygons with over()
  # points.w.values <- over(points, objects.raw)  # [, "ECOZONE_NA"]  ## over() outputs a df with the values of the fields in ecozones.UTMreproj at the locations of lidar
  # points.merged <- cbind(points@data, points.w.values)  ## stack together GT columns and predictor values
  # compl.dataset <- points.merged %>% filter(!is.na(Border_ind))  ## remove NAs (points falling outiside of polygons)
  ## -------- TO UNCOMMENT
  
  # ---------- TO DEL
  compl.dataset <- points@data %>% filter(!is.na(VanSubset1))  ## remove NAs (points falling outiside of polygons)
  # ---------- TO DEL
  
  compl.dataset.dt <- as.data.table(compl.dataset)
  rm(compl.dataset)
  
  ## Create n-fold CV indicators
  set.seed(params$seed)
  folds <- createFolds(compl.dataset.dt[[class.col]], k=params$nfold, list=F)  ## [[]] to access column of dt as vector
  
  ## Loop over the predictor types
  for (pred.type in params$predictor.types) {  

    if (pred.type == 'all') {
      predictors <- params$predictors.all
    } else if (pred.type == 'spectral') {
      predictors <- params$predictors.spectral 
    } else if (pred.type == 'LiDAR') {
      predictors <- params$predictors.LiDAR
    }
    
    ## Initialize empty vector to store N-fold predictions at each round of the loop
    Y.predicted <- factor( rep(NA, nrow(compl.dataset.dt)), levels=levels(compl.dataset.dt[[class.col]]) )

    ## n-fold CV loop
    for (f in 1:params$nfold) {
      
      segments.out <- folds == f    ## get logical indices of in and out segments in the object-level dt
      segments.in <- !segments.out
      
#### RF TRAINING & PREDICTION ----------------------------------------------
      
      ## Set mtry parameter according to params$mtry
      nr.vars <- length(predictors) 
      if (params$mtry == 'sqrt_nr_var') {
        mtries <- floor(sqrt(nr.vars))
      } else if (params$mtry == 'nr_var_div_3') {
        mtries <- floor(nr.vars/3)
      }
      
      ## Training on kept-in segments
      set.seed(params$seed)  ## set seed to always have same results
      RF <- randomForest(x=compl.dataset.dt[segments.in, predictors, with=FALSE], y=droplevels(compl.dataset.dt[[class.col]][segments.in]),   ## y has to be a vector and the syntax for data.table is first getting the vector with [[]] then subsetting it from outside by adding [segments.in] 
                         ntree=params$ntree, mtry=mtries, nodesize=params$nodesize, importance=params$plot.importance)  ## apply RF on dt with object-level values using as predictors the columns listed in "predictors" and with response variable the column specified by "class.col"
      
      ## Prediction on left-out segments
      Y.predicted.segments.out <- predict(RF, compl.dataset.dt[segments.out, predictors, with=FALSE], type="response", predict.all=F, nodes=F)
      
      ## Fill vector with predicted labels
      Y.predicted[segments.out] <- Y.predicted.segments.out
      
    }  ## end of n-fold loop
    
#### ASSESSMENT -----------------------------------------------------------
    
    ## overall assessment
    metrics <- classif.metrics(Y.predicted, compl.dataset.dt[[class.col]])
    
    ## store results by GT type and by predictors type in RES list
    cmd <- sprintf("RES$%s$%s <- metrics", gt.type, pred.type)
    eval(parse(text=cmd))
    
  } ## end of loop over predictor types
   
}  ## end of loop over GT types

RES.file = file.path(results.dir, 'RESULTS.Rdata', fsep = .Platform$file.sep) 
save(RES, file = RES.file)


#### PREDICT ON FULL MAP ------------------------------------------------

# XXXXXXXXXXXXXXXXX
# paste RF on FULL dataset
# XXXXXXXXXXXXXXXXX
# 
# ## Save Segments
# 
# ## Save shp with predicted class
# ## build a dt with the predicted class for each segment (Y.predicted.segments.out) and the associated segment ID (single.scale.obj.df[segments.out, "segID"])
# y.pred.segID.dt <- data.table(segID=single.scale.obj.dt[segments.out, segID], ypred=Y.predicted.segments.out)
# 
# ## assign the predicted class to the "data" df of the segments shp by merging by segment ID ("dn" or "segID"), all.x=T is used to keep the segments for which there is no prediction (outside of fire)
# segments@data <- merge(segments@data, y.pred.segID.dt, by.x="dn", by.y="segID", all.x=T)  
# segments@data$ypred[is.na(segments@data$ypred)] <- params$mort.class.labels[1]    ## if NA are assigned to polygons outside of fire, assign the lowest mortality class instead
# writeOGR(segments, results.dir, sprintf("%s_pred_map_%s", fire.out, best.colname), driver="ESRI Shapefile", overwrite_layer=TRUE)   ## write prediction map shapefile for the left-out fire
# 
# ## Join the predicted labels for the segments to the corresponding segID in the complete vector of all images
# setkey(y.pred.segID.dt, segID) ## set key as the segment IDs column
# segID.allpixels.out.dt <- data.table(segID=allpixels.out.dt[,segID], key = "segID") #
# Y.predicted.object.single[idx.pix.out] <- segID.allpixels.out.dt[y.pred.segID.dt, ypred]  ## join to data.table based on a common key with this command allpixels.out.segID.dt[y.pred.segID.dt], then select only ypred as a column


#### PRINT LOGS ---------------------------------------------------------

## clock global time
toc <- proc.time()-tic[3]
end.message <- sprintf("Total elapsed time: %s, finished running on %s", seconds_to_period(toc[3]), Sys.time())
print(end.message)




