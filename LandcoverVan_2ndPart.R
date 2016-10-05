#### INIT ------------------------------------------------------
rm(list=ls())  ## remove all variables

start.message <- sprintf("Vancouver landuse/landcover classification, started running on %s", Sys.time())
print(start.message)

## !!! GIONA !!!
setwd("D:\\RandomForests")

## I ADDED THIS LINE AND I WANT TO KEEP IT


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
                      "tidyverse"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]   ## named vector members whose name is "Package"
if(length(new.packages)) install.packages(new.packages)   ## install only unavailable packages
for (pack in list.of.packages){
  library(pack, character.only=TRUE)  ## call all the packages in list.of.packages
}

#### PARAMETERS ---------------------------------------------

# initialize params as list
params <- list()
## General
params$GT.types <- c("One_m", "Five_m")   ## type of GT (to be put in a loop to see both results)
params$predictor.types <- c("all","spectral","LiDAR","geometric")
params$run.ShpRead <- F # set to T if shapefiles have not been read in yet, set to F if they have, so that code can be run from start
## list of all starting predictors
 
## !!! GIONA !!!
# added "BLABLA", removed "Bright_vis"
params$predictors.spectral <- c("BLABLA", "GLCMCon_NIR", "GLCMHomNIR", "Imag_Brightness", 
                                "Mean_Blue", "Mean_Green", "Mean_Red","Mean_RE","Mean_NIR","NDRE", 
                                "NDVI", "NDVIRE","NIR_div_RE","SAVI","sd_red", 
                                "sd_blue","sd_green", "sd_RE", "sd_NIR")
## !!! GIONA !!!

params$predictors.LiDAR <- c("CoefVar_nD", "GLCMCon_nDSM", "GLCMHom_nDSM", 
                             "MaxHtMinHt", "Mean_nDSM", "Mean_slope", "Mean_zDev", 
                             "nDSM_div_SD_nDSM", "sd_ndsm", "sd_slope", "sd_zdev") 
params$predictors.geometric <-  c("Border_ind", "Compactnes", "Density", "EllipticFi",
                                  "Len_div_Width", "RectangFit","RelBord_bldg",
                                  "RelBord_trees", "RelBord_unclass", "Roundness")

params$predictors.all <- c(params$predictors.spectral,params$predictors.LiDAR,params$predictors.geometric)                                                                    


## RF
params$nfold <- 4
params$seed <- 2016        ## seed to have same RF result
params$parallel.RF <- T    ## whether to run RF in parallel or not
params$ntree <- 100     ## RF nr of trees
params$mtry <- 'sqrt_nr_var'  ## how to set RF mtry: 'sqrt_nr_var' or 'nr_var_div_3'
params$nodesize <- 1   ## RF nodesize: default for classification is 1
params$plot.importance <- F  ## whether to plot RF variable importance
params$prediction.maps <- T

base.dir <- 'D:/RandomForests'    ## base working directory

points.filename <- "VanSubsetPoints_Buffer_SJ_unambig"
points.folder <- "Vancouver/shp"
objects.filename <- "Vancouver_unclassified_final_v9"
objects.folder <- "Vancouver/shp"

#### FUNCTIONS ----------------------------------------------------------

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

#### READ DATA --------------------------------------------------------------

tic <- proc.time() ## start clocking global time

data.dir <- file.path(base.dir, "Data", fsep = .Platform$file.sep)   ## data directory (nothing should be written here)
dir.create(data.dir)
results.dir <- file.path(base.dir, "Results", fsep = .Platform$file.sep)   ## results directory (outputs go here)
dir.create(results.dir)
figures.dir <- file.path(base.dir, "Figures", fsep = .Platform$file.sep)   ## figures directory (figures go here)
dir.create(figures.dir)
# temp.dir <- file.path(data.dir, "temp")  ## directory for temporary files like the segmentation shps (overwritten each time)
# if (!file.exists(temp.dir)) {dir.create(temp.dir, showWarnings=F, recursive=T)}  ## create it

# set up data directories
objects.path <- file.path(data.dir, objects.folder, fsep = .Platform$file.sep) 
points.path <- file.path(data.dir, points.folder, fsep = .Platform$file.sep)

if(params$run.ShpRead){

  ## read shapefiles
  objects.raw <- readOGR(dsn=objects.path, layer=objects.filename) 
  points.raw <- readOGR(dsn=points.path, layer=points.filename) 
  
  # clip artefacts away from unclassified object edge using lidar boundary
  van_lidar_boundary <- readOGR(dsn ="D:\\RandomForests\\Data\\Vancouver\\shp", layer ="Vancouver_nDSM_domain")
  plot(van_lidar_boundary)
  # now to clip using sp::over
  objects_backup <- objects.raw
  objects_clip <- objects_backup[van_lidar_boundary,] 
  
  #### Changing column names and data wrangling ----------------------------------------
  
  # remove buildings, trees and other columns from objects_clip
  names(objects_clip)
  drops <- c("Building","Trees","Len_divThi","Thick_pxl")
  objects_clip_short <- objects_clip[,!(names(objects_clip) %in% drops)]
  names(objects_clip_short)
  # use names from objects_table to rename objects.raw attribute table
  
  #read in data
  objects_table <- fread("D:\\RandomForests\\Data\\Vancouver\\shp\\Vancouver_unclassified_final_v9.txt") #read in data table
  
  # compare names of objects_table to objects_clip_short
  names(objects_clip_short)
  names(objects_table)
  names(objects_clip_short) %in% names(objects_table)
  names(objects_clip_short)[!names(objects_clip_short) %in% names(objects_table)]
  names(objects_table) %in% names(objects_clip_short)
  names(objects_table)[!names(objects_table) %in% names(objects_clip_short)]
  
  # change data table names to make them more readable
  oldnames1 <- c("GLCMCon_nD", "GLCM_Contr", "GLCMHom_nD", "GLCMHomNIR", "Imag_Brigh", "Len_divThi", "Len_divWid")
  newnames1 <- c("GLCMCon_nDSM", "GLCMCon_NIR", "GLCMHom_nDSM", "GLCMHomNIR", "Imag_Brightness", "Len_div_Thick", "Len_div_Width")
  setnames(objects_table, oldnames1, newnames1)
  
  oldnames2 <- "Mean_Red_E"
  newnames2 <- "Mean_RE"
  setnames(objects_table, oldnames2, newnames2)
  
  oldnames3 <- "nDSM_divDS"
  newnames3 <- "nDSM_div_SD_nDSM"
  setnames(objects_table, oldnames3, newnames3)
  
  oldnames4 <- "NIR_div_RE"
  newnames4 <- "NIR_div_RE"
  setnames(objects_table, oldnames4, newnames4)
  
  oldnames5 <- c("RelBord_bl", "RelBord_tr", "RelBord_un")
  newnames5 <- c("RelBord_bldg", "RelBord_trees", "RelBord_unclass")
  setnames(objects_table, oldnames5, newnames5)
  
  names(objects_table)
  
  # remove FID, building and trees, and two useless geom params columns from data table
  objects_table_short <- objects_table[,c("FID","Building","Trees","Len_div_Thick","Thick_pxl"):=NULL]
 
  # compare to make sure the names are the same length
  names(objects_clip_short) %in% names(objects_table_short)
  names(objects_clip_short)[!names(objects_clip_short) %in% names(objects_table_short)]
  names(objects_table_short) %in% names(objects_clip_short)
  names(objects_table_short)[!names(objects_table_short) %in% names(objects_clip_short)]
  
  names(objects_clip_short)
  names(objects_table_short)
  head(names(objects_clip_short))
  head(names(objects_table_short))
  tail(names(objects_clip_short))
  tail(names(objects_table_short))
  length(names(objects_clip_short))
  length(names(objects_table_short))
  
  # set names of objects_clip_short to object_table short
  names(objects_table_short) <-  make.names(names(objects_table_short), unique = TRUE)
  names(objects_clip_short) <- names(objects_table_short)
  # note that "/" had been changed to "."
  names(objects_clip_short)
  
  #make sure the names of objects_clip matches the predictors defined above (except Thick_pxl and Len.Thick which should be dropped anyway)
  names(objects_clip_short) %in% params$predictors.all
  names(objects_clip_short)[!names(objects_clip_short) %in% params$predictors.all]
  
  #save object names to a file that can be loaded outside loop
  object_names <- names(objects_clip_short)
  write.csv(x = object_names,file = paste0(objects.path,"/object_names.csv"),row.names = FALSE,col.names = FALSE)
#   can't get file save/read to work
#   object_names.file = file.path(objects.path, 'object_names.Rdata', fsep = .Platform$file.sep) 
#   save(object_names, file = object_names.file)
  
  writeOGR(objects_clip_short, objects.path, "unclass_objects_clip", driver="ESRI Shapefile", overwrite_layer=TRUE)   ## write prediction map shapefile for the left-out fire
  
  } else {
    objects_clip_short <- readOGR(dsn =objects.path, layer ="unclass_objects_clip")
    new_names <- read_csv(file = paste0(objects.path,"/object_names.csv")) #col_names = c("row","names")
    new_names <- (new_names$x)
    names(objects_clip_short)
    names(objects_clip_short) <- new_names
    names(objects_clip_short) #should be the right names!
    # names(objects_clip_short) <- read_file(object_names.file) ####didn't work
    points.raw <- readOGR(dsn=points.path, layer=points.filename) 
  }


# change column names to be meaningful for points and objects
names (points.raw)
names (points.raw) [11:24]
c <- c("Point_Number","Onem_Class_1_1st_choice","Onem_Class_1_2nd_choice","Onem_Class_2_1st_choice",
       "Onem_Class_2_2nd_choice", "Onem_Class_3_1st_choice", "Onem_Class_3_2nd_choice",
       "Fivem_Class1_1st_choice","Fivem_Class1_2nd_choice","Fivem_Class_2_1st_choice",
       "Fivem_Class_2_2nd_choice","Fivem_Class_3_1st_choice","Fivem_Class_3_2nd_choice",
       "Classifier_notes")
names (points.raw) [11:24] <- c
names(points.raw)

# drop previous spatial join info
names(points.raw)
drops2 <- c("Shape_Leng","distance","Join_Count", "TARGET_FID", "JOIN_FID", "CID", "ORIG_FID", "CID_1", "PointID")
points.raw.short <- points.raw[,!(names(points.raw) %in% drops2)]
names(points.raw.short)

# remove NA rows
points.raw.short@data$Onem_Class_2_1st_choice
indices <- !is.na(points.raw.short@data$Onem_Class_2_1st_choice)
indices
points.short <- (points.raw.short[which(indices),])
dim((points.raw.short)) #should be 400 or 0-399
dim((points.short)) #should be less #hooray
points.short@data$Onem_Class_2_1st_choice

#fix any mispelled class names for the ground truth points 

# choose the columns you want to use
change <- grep("Class_2", names(points.short))
change #use columns 5 and 11

# figure out which unique values you have
unique(points.short@data[,5])

points.short@data[,5] <- gsub(
  x = points.short@data[,5], 
  pattern = "Grass_Herb", 
  replacement = "Grass-Herb")

points.short@data[,5] <- gsub(
  x = points.short@data[,5], 
  pattern = "Tree_Canopy", 
  replacement = "Trees")

points.short@data[,5] <- gsub(
  x = points.short@data[,5], 
  pattern = "Shrub", 
  replacement = "Trees")

# check to make sure there aren't any more unique values you missed
unique(points.short@data[,5])

# now for row 13
unique(points.short@data[,11])

points.short@data[,11] <- gsub(
  x = points.short@data[,11], 
  pattern = "tree_canopy", 
  replacement = "Trees")

points.short@data[,11] <- gsub(
  x = points.short@data[,11], 
  pattern = "tree_Canopy", 
  replacement = "Trees")

points.short@data[,11] <- gsub(
  x = points.short@data[,11], 
  pattern = "Shrub", 
  replacement = "Trees")

unique(points.short@data[,11])

### checking to make sure points numbers are respected when filtering
a <- points.short@data %>% 
  select(Point_Number,Shape_Area,Onem_Class_2_1st_choice)
a

b <- points.short@data %>% 
  select(Point_Number,Shape_Area,Onem_Class_2_1st_choice) %>% 
  filter(!(Onem_Class_2_1st_choice == "Trees" | Onem_Class_2_1st_choice == "Building"))
b  

length(a[,2])
length(b[,2])

a[,1] %in% b[,1]
a[,2] %in% b[,2]

head(a,15)
head(b)
tail(a,15)
tail(b)

filter(points.short@data, !(Onem_Class_2_1st_choice == "Trees" | Onem_Class_2_1st_choice == "Building"))

#remove building or tree points
points.short@data <- filter(points.short@data, !(Onem_Class_2_1st_choice == "Trees" | Onem_Class_2_1st_choice == "Building"))
unique(points.short@data[,5])
unique(points.short@data[,11])



##### START OF THE LOOP, choosing which group to include ----------------------
RES <- list()  ## initialize list object to store results
for (gt.type in params$GT.types) {  ## loop using one or five m ground truth polys
  
  ## filter points to keep only the desired GT level ("One_m" or "Five_m")
  if (gt.type == "One_m") {
    points <- subset(points.short, Shape_Area < 10)
    class.col <- "Onem_Class_2_1st_choice"
  } else if (gt.type == "Five_m") {
    points <- subset(points.short, Shape_Area > 10) # see if this change improves things somehow
    # points <- subset(points.short, Shape_Area > 10 & Onem_Class_2_1st_choice == Fivem_Class_2_1st_choice) ## watch out for names (to be changed)!
    class.col <- "Fivem_Class_2_1st_choice"
  }
  

  
#### SPATIAL JOIN --------------------------------------------------------------
  
  ## solution with union() from Raster package which has more options (not working!)
  # objects <- gBuffer(objects, byid=TRUE, width=0)
  # points <- gBuffer(points, byid=TRUE, width=0)
  # union.res <- union(objects, points)
  
 
  # spatial join the polygons with over()
  points.w.values <- over(points, objects_clip_short)
  points.merged <- cbind(points@data, points.w.values)  ## stack together GT columns and predictor values
  compl.dataset <- points.merged %>% filter(!is.na(Border_ind))  ## remove NAs (points falling outiside of polygons)
  compl.dataset.dt <- as.data.table(compl.dataset)
  rm(compl.dataset)
  compl.dataset.dt
  
  #delete any na, nan or inf values from predictor columns
  if (sum((compl.dataset.dt[, colSums(sapply(compl.dataset.dt, is.infinite))])) >= 1){
    compl.dataset.dt <- do.call(data.table,lapply(compl.dataset.dt, 
                                                  function(x) replace(x, is.infinite(x),NA)))}
  
  if (sum(is.na(compl.dataset.dt)) >= 1){
    compl.dataset.dt <- do.call(data.table,lapply(compl.dataset.dt, 
                                                  function(x) replace(x, is.na(x),0)))}
  
#### Create n-fold CV indicators------------------------------------
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
    
    # set class label column as factor
    compl.dataset.dt[, (class.col) := lapply(.SD, as.factor),.SDcols=class.col]
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
      RF <- randomForest(x=compl.dataset.dt[segments.in, predictors, with=FALSE], y=compl.dataset.dt[[class.col]][segments.in],   ## y has to be a vector and the syntax for data.table is first getting the vector with [[]] then subsetting it from outside by adding [segments.in] 
                         ntree=params$ntree, mtry=mtries, nodesize=params$nodesize, importance=params$plot.importance)  ## apply RF on dt with object-level values using as predictors the columns listed in "predictors" and with response variable the column specified by "class.col"
      
      ## Prediction on left-out segments
      Y.predicted.segments.out <- predict(RF, compl.dataset.dt[segments.out, predictors, with=FALSE], type="response", predict.all=F, nodes=F)
      
      ## Fill vector with predicted labels
      Y.predicted[segments.out] <- Y.predicted.segments.out
      
    }  ## end of n-fold loop
    
#### ASSESSMENT ----------------------------------------------------------- #will this work here?
    
    ## overall assessment
    metrics <- classif.metrics(Y.predicted, compl.dataset.dt[[class.col]])
    
    ## store results by GT type and by predictors type in RES list
    cmd <- sprintf("RES$%s$%s <- metrics", gt.type, pred.type)
    eval(parse(text=cmd))
	
    
    RES.file = file.path(results.dir, 'RESULTS_Van.Rdata', fsep = .Platform$file.sep) 
    save(RES, file = RES.file)    
    
#### PREDICTION ON FULL MAP ------------------------------------------------


    if (params$prediction.maps) {
      
      RF_complete <- randomForest(x=compl.dataset.dt[, predictors, with=FALSE], y=compl.dataset.dt[[class.col]],   ## y has to be a vector and the syntax for data.table is first getting the vector with [[]] then subsetting it from outside by adding [segments.in] 
                                  ntree=params$ntree, mtry=mtries, nodesize=params$nodesize, importance=params$plot.importance)  ## apply RF on dt with object-level values using as predictors the columns listed in "predictors" and with response variable the column specified by "class.col"
      
      # remove na/nan/inf from objects_clip_short@data[, predictors]
      #delete any na, nan or inf values from predictor columns
      
      obj.dt <- as.data.table(objects_clip_short@data[, predictors])
      
      if ( any(obj.dt[, colSums(sapply(.SD, is.infinite))]!=0) ) {
        objects_clip_short@data <- do.call( data.frame,lapply(objects_clip_short@data[, predictors], 
                                                              function(x) replace(x, is.infinite(x), 0)) )
      }
      
      if (sum(is.na(objects_clip_short@data[, predictors])) >= 1){
        compl.dataset.dt <- do.call( data.table,lapply(objects_clip_short@data[, predictors], 
                                                       function(x) replace(x, is.na(x),0)) )
      }
      
      Y.predicted.map <- predict(RF_complete, objects_clip_short@data[, predictors], type="response", predict.all=F, nodes=F)
      ## add foreach() to run in parallel over tiles
      
      params$cols.to.keep <- params$predictors.all
      objects.map <- objects_clip_short
      objects.map@data[, !colnames(objects.map@data) %in% params$cols.to.keep] <- list(NULL)
      objects.map@data$pred_class <- Y.predicted.map
      
      writeOGR(objects.map, results.dir, sprintf("Prediction_map_unclass2_%s_%s", gt.type, pred.type), driver="ESRI Shapefile", overwrite_layer=TRUE)   ## write prediction map shapefile
      
    }  ## end if params$predictors.all

  } ## end of loop over predictor types

}  ## end of loop over GT types

RES.file = file.path(results.dir, 'RESULTS_Van.Rdata', fsep = .Platform$file.sep) 
save(RES, file = RES.file)

## !!!!!!!!!! TO DELETE: FROM HERE !!!!!!!!!!
#### PREDICT ON FULL MAP ------------------------------------------------

# XXXXXXXXXXXXXXXXX
# paste RF on FULL dataset
# XXXXXXXXXXXXXXXXX
# 

compl.dataset.df <- as.data.frame(compl.dataset.dt)
RF_comp <- randomForest(x=compl.dataset.df[, predictors], y=as.factor(compl.dataset.df[[class.col]]),   ## y has to be a vector and the syntax for data.table is first getting the vector with [[]] then subsetting it from outside by adding [segments.in] 
                   ntree=params$ntree, mtry=mtries, nodesize=params$nodesize, importance=params$plot.importance)  ## apply RF on dt with object-level values using as predictors the columns listed in "predictors" and with response variable the column specified by "class.col"


#this is what testing out a github push pull system looks like


## Prediction on left-out segments
# Y.predicted.segments.out <- predict(RF, compl.dataset.dt[segments.out, predictors, with=FALSE], type="response", predict.all=F, nodes=F)

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
## !!!!!!!!!! TO DELETE: TO HERE !!!!!!!!!!



#### PRINT LOGS ---------------------------------------------------------

## clock global time
toc <- proc.time()-tic[3]
end.message <- sprintf("Total elapsed time: %s, finished running on %s", seconds_to_period(toc[3]), Sys.time())
print(end.message)




