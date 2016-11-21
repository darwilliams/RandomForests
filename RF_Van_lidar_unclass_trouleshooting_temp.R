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

#### Set up file paths and object names ----------------------------------------------------------------------------------------

base.dir <- "E:/MetroVancouverData"    ## base working directory
objects.path <- file.path(base.dir, "eCog_output/LiDAR_areas/", fsep = .Platform$file.sep)   ## data directory (nothing should be written here)

points.path <- file.path(base.dir, "Training_Validation_Points/", fsep = .Platform$file.sep)   ## data directory (nothing should be written here)
results.dir <- file.path(base.dir, "eCog_output/LiDAR_areas/LiDAR_Results", fsep = .Platform$file.sep)   ## results directory (outputs go here)
dir.create(results.dir)
figures.dir <- file.path(base.dir, "eCog_output/LiDAR_areas/LiDAR_Figures", fsep = .Platform$file.sep)   ## figures directory (figures go here)
dir.create(figures.dir)
temp.dir <- file.path(results.dir, "temp")  ## directory for temporary files like the segmentation shps (overwritten each time)
if (!file.exists(temp.dir)) {dir.create(temp.dir, showWarnings=F, recursive=T)}  ## create it
objects.tmp.path <- "E:/MetroVancouverData/eCog_output/LiDAR_areas/LiDAR_Results/temp"

#create list of object names
source("shplist.R")
shplist(objects.path)
objectslist <- str_subset(shplist$V1, "unclas")
objects.filename.list <- str_replace(objectslist,pattern = "E:/MetroVancouverData/eCog_output/LiDAR_areas/",replacement = "")
objects.filename.list <- str_replace(objects.filename.list,pattern = ".shp",replacement = "")

# set points filename
points.filename <- "MetroVan_gt_Bins1_16_tidy_unambig"

#### PARAMETERS ---------------------------------------------

# initialize params as list
params <- list()

## General
params$GT.types <- c("One_m", "Five_m")   ## type of GT (to be put in a loop to see both results)
params$predictor.types <- c("all","spectral","LiDAR","geometric")
params$run.ShpRead <- F # set to T if shapefiles have never been read in, set to F if they have, so that code can be run from start
## list of all starting predictors 

params$predictors.spectral <- c("Bright_vis", "GLCMCon_NIR", "GLCMHomNIR", "Imag_Brightness", 
                                "Mean_Blue", "Mean_Green", "Mean_Red","Mean_RE","Mean_NIR","NDRE", 
                                "NDVI", "NDVIRE","NIR_div_RE","SAVI","sd_red", 
                                "sd_blue","sd_green", "sd_RE", "sd_NIR")
params$predictors.LiDAR <- c("CoefVar_nDSM", "GLCMCon_nDSM", "GLCMHom_nDSM", 
                             "MaxHtMinHt", "Mean_nDSM", "Mean_slope", "Mean_zDev", 
                             "sd_ndsm", "sd_slope", "sd_zdev") 
params$predictors.geometric <-  c("Border_ind", "Compactnes", "Density", "EllipticFi",
                                  "Len_div_Width", "RectangFit","RelBord_bldg",
                                  "RelBord_trees", "RelBord_unclass", "Roundness")

params$predictors.all <- c(params$predictors.spectral,params$predictors.LiDAR,params$predictors.geometric)                                                                    

params$tile.names <- objects.filename.list
rm(shplist)

## RF
params$nfold <- 4
params$seed <- 2016        ## seed to have same RF result
params$parallel.RF <- F    ## whether to run RF in parallel or not
params$ntree <- 100     ## RF nr of trees
params$mtry <- 'sqrt_nr_var'  ## how to set RF mtry: 'sqrt_nr_var' or 'nr_var_div_3'
params$nodesize <- 1   ## RF nodesize: default for classification is 1
params$plot.importance <- F  ## whether to plot RF variable importance
params$prediction.maps <- T

## Parallel Processing clusters
params$nr.clusters <- 10

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


#### Read in points and fill na values in data table

#read in cleaned up points
points.clean <- readOGR(dsn=points.path, layer=points.filename) 

# change column names to be meaningful for points and objects
names (points.clean)
points.names <- read_csv(file.path(points.path,"points_variables_tidy.csv"),col_names = FALSE)
names (points.clean) <- points.names$X1 
names(points.clean)

#### Start clipping loop ---------------------------------------
if(params$run.ShpRead){

#### Set up parallel processing ####
# uncomment stopCluster(cl) below if uncommenting this 
cl <- makeCluster(params$nr.clusters) ## to uncomment when running in parallel, after successful debugging
registerDoParallel(cl)
  
#### Clip input objects to nDSM boundaries ####  
  foreach (tile.idx = 1:length(params$tile.names), .packages=list.of.packages) %dopar% {
  # foreach (tile.idx = 42:51, .packages=list.of.packages) %dopar% { running in stages ended up working - why?
  # for (tile.idx in 1:length(params$tile.names)) {   

  	tile.name <- params$tile.names[tile.idx]

    ## read shapefiles
    objects.raw <- readOGR(dsn=objects.path, layer=tile.name)
    if(is.na(sp::is.projected(objects.raw))){
      proj4string(objects.raw) <- CRS(proj4string(points.clean))
    }
    # clip artefacts away from unclassified object edge using lidar boundary
    nDSM_bound_direc <- "E:/MetroVancouverData/nDSM_boundaries"
    source("shplist.R")
    shplist(nDSM_bound_direc)
    nDSM_boundary_list <- shplist
    # given tile name aaa_bbb select ndsm_bound that is partial string match
    pattern <- unlist(strsplit(tile.name, "_"))[1]
    boundary.file.name <- str_subset(nDSM_boundary_list$V1,pattern)
    boundary.file.name <- str_replace(boundary.file.name,pattern = "E:/MetroVancouverData/nDSM_boundaries/",replacement = "")
    boundary.file.name <- str_replace(boundary.file.name,pattern = ".shp",replacement = "")
    if(length(boundary.file.name)>1){
      stop("Multiple lidar boundaries selected:", print(tile.name), print(boundary.file.name))
    }
    lidar_boundary <- readOGR(dsn = nDSM_bound_direc, layer = boundary.file.name)
    if(is.na(sp::is.projected(lidar_boundary))){
      proj4string(lidar_boundary) <- CRS(proj4string(points.clean))
    }
    rm(boundary.file.name)
    # now to clip using sp::over
    objects_backup <- objects.raw
    objects_clip <- objects_backup[lidar_boundary,] 
    
    #### Changing column names and data wrangling ----------------------------------------
    
    # remove buildings, trees from objects_clip
    names(objects_clip)
    drops <- c("Building","Trees")
    objects_clip_short <- objects_clip[,!(names(objects_clip) %in% drops)]
    names(objects_clip_short)
    
    # use names from objects_table to rename objects.raw attribute table
    #read in data
    objects_table <- fread("D:\\RandomForests\\LidarObjectFeatureNames.csv") #read in data table
    objects_table <- objects_table$RNames
    drop <- which(objects_table %in% drops)
    objects_table <- objects_table[-drop]
    # set objects_table to names of objects_clip_short
    names(objects_clip_short) <- objects_table
    
    #save object names to a file that can be loaded outside loop
    object_names <- names(objects_clip_short)
    write.csv(x = object_names,file = paste0(objects.path,"/object_names_unclass.csv"),row.names = FALSE,col.names = FALSE)
    
    #write out clipped objects
    writeOGR(objects_clip_short, objects.tmp.path, sprintf("unclass_objects_clip_%s", tile.name), driver="ESRI Shapefile", overwrite_layer=TRUE)  
    
  }

stopCluster(cl)

}

##### START OF THE LOOP, choosing which group to include -------------------------

RES <- list()  ## initialize list object to store results
for (gt.type in params$GT.types) {  ## loop using one or five m ground truth polys
  
	first.tile <- T ## to execute specific code for the first tile only (initialize the big shapefiles)
	
	source("shplist.R")
	shplist(objects.tmp.path)
	objectlist <- shplist
	objectlist <- str_subset(shplist$V1, "unclas")
	objectlist <- str_replace(objectlist,pattern = "E:/MetroVancouverData/eCog_output/LiDAR_areas/LiDAR_Results/temp/",replacement = "")
	objectlist <- str_replace(objectlist,pattern = ".shp",replacement = "")
	
#	  uncomment stopCluster(cl) below if uncommenting this and vice versa 
	cl <- makeCluster(params$nr.clusters)
	registerDoParallel(cl)
	foreach (tile.idx = 1:length(params$tile.names), .packages=list.of.packages) %dopar% {   
	# for (tile.idx in 1:length(objectlist)) {   
	 
	    
		tile.name <- objectlist[tile.idx]
		objects_clip_short <- readOGR(dsn = objects.tmp.path, layer = tile.name)
		if(is.na(sp::is.projected(objects_clip_short))){
		  proj4string(objects_clip_short) <- CRS(proj4string(points.clean))
		}
	# remove buildings, trees from objects_clip
		names(objects_clip_short)
		drops <- c("Building","Trees")
		objects_clip_short <- objects_clip_short[,!(names(objects_clip_short) %in% drops)]
		names(objects_clip_short)
		
		# use names from objects_table to rename objects.raw attribute table
		#read in data
		objects_table <- fread("D:\\RandomForests\\LidarObjectFeatureNames.csv") #read in data table
		objects_table <- objects_table$RNames
		drop <- which(objects_table %in% drops)
		objects_table <- objects_table[-drop]
		# set objects_table to names of objects_clip_short
		names(objects_clip_short) <- objects_table
		##### ensure object variable names match parameter names ####################
		new_names <- read_csv(file = paste0(objects.path,"/object_names_unclass.csv")) #col_names = c("row","names")
		new_names <- (new_names$x)
		# names(objects_clip_short)
		names(objects_clip_short) <- new_names
		# names(objects_clip_short) #should be the right names!
		#write out clipped objects
		# writeOGR(objects_clip_short, objects.tmp.path, sprintf("unclass_objects_clip_%s", tile.name), driver="ESRI Shapefile", overwrite_layer=TRUE)  
		
		## filter points to keep only the desired GT level ("One_m" or "Five_m")
		if (gt.type == "One_m") {
  		points <- subset(points.clean, Shape_Area < 10)
  		class.col <- "One_m_Class_2_1st_choice"
		} else if (gt.type == "Five_m") {
  		points <- subset(points.clean, Shape_Area > 10) # see if this change improves things somehow
  		# points <- subset(points.clean, Shape_Area > 10 & Onem_Class_2_1st_choice == Fivem_Class_2_1st_choice) ## watch out for names (to be changed)!
  		class.col <- "Five_m_Class_2_1st_choice"
		}
	  
		#### SPATIAL JOIN --------------------------------------------------------------

		# spatial join the polygons with over()
		points.w.values <- over(points, objects_clip_short)
		points.merged <- cbind(points@data, points.w.values)  ## stack together GT columns and predictor values
		compl.dataset <- points.merged %>% filter(!is.na(Border_ind))  ## remove NAs (points falling outiside of polygons)
		
		if (first.tile) {
			compl.dataset.dt <- as.data.table(compl.dataset)
			compl.dataset.dt <<- as.data.table(compl.dataset)
			assign("compl.dataset.dt",compl.dataset.dt,envir = .GlobalEnv)
			assign("compl.dataset.dt",compl.dataset.dt,envir = parent.frame())
			first.tile <- F   ## change logical to FALSE so that 2nd part of if-else block is used from now on
		} else {
			compl.dataset.dt <- rbind(compl.dataset.dt, as.data.table(compl.dataset))
			compl.dataset.dt <<- rbind(compl.dataset.dt, as.data.table(compl.dataset)) #need <<- ?
			#delete any inf and na values from predictor columns
			if (sum(is.infinite(compl.dataset.dt$CoefVar_nDSM)) >= 1){
			  compl.dataset.dt$CoefVar_nDSM <- replace(compl.dataset.dt$CoefVar_nDSM, 
			                                            is.infinite(compl.dataset.dt$CoefVar_nDSM), 0)}
			assign("compl.dataset.dt",compl.dataset.dt,envir = .GlobalEnv) # can also try this: envir = parent.frame()
			assign("compl.dataset.dt",compl.dataset.dt,envir = parent.frame())
		}
		
		rm(compl.dataset)
		assign("compl.dataset.dt",compl.dataset.dt,envir = .GlobalEnv)
		assign("compl.dataset.dt",compl.dataset.dt,envir = parent.frame())
		
	}
	stopCluster(cl)
	


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
    #compl.dataset.dt[, (class.col) := lapply(.SD, as.factor),.SDcols=class.col]
    
    ## Initialize empty vector to store N-fold predictions at each round of the loop
    Y.predicted <- factor(rep(NA, nrow(compl.dataset.dt)), levels=levels(compl.dataset.dt[[class.col]]))

    ## n-fold CV loop
    for (f in 1:params$nfold) {
      
      segments.out <- folds == f    ## get logical indices of in and out segments in the object-level dt
      segments.in <- !segments.out
      compl.dataset.dt[[class.col]][segments.in]
      #drop unused levels from class.col
      y <- compl.dataset.dt[[class.col]][segments.in]
      #levels(y) %in% unique(y)
      y <- forcats::fct_drop(y)
      #levels(y) %in% unique(y)
  
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
      RF <- randomForest(x=compl.dataset.dt[segments.in, predictors, with=FALSE], y,   ## y has to be a vector and the syntax for data.table is first getting the vector with [[]] then subsetting it from outside by adding [segments.in] 
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

#### PREDICTION ON FULL MAP ------------------------------------------------


    if (params$prediction.maps) {
      y <- compl.dataset.dt[[class.col]]
      y <- forcats::fct_drop(y)
		  RF_complete <- randomForest(x=compl.dataset.dt[, predictors, with=FALSE], y,   ## y has to be a vector and the syntax for data.table is first getting the vector with [[]] then subsetting it from outside by adding [segments.in] 
								  ntree=params$ntree, mtry=mtries, nodesize=params$nodesize, importance=params$plot.importance)  ## apply RF on dt with object-level values using as predictors the columns listed in "predictors" and with response variable the column specified by "class.col"

  		#### LOOP ON TILES FOR PREDICTION -----------------------------------------
#       #uncomment stopCluster(cl) below if uncommenting this 
#   		cl <- makeCluster(params$nr.clusters)
#   		registerDoParallel(cl)
#   		foreach (tile.idx = 1:length(params$tile.names), .packages=list.of.packages) %dopar% {   
  		for (tile.idx in 1:length(objectlist)) {
  		
  		  source("shplist.R")
  		  shplist(objects.tmp.path)
  		  objectlist <- shplist
  		  objectlist <- str_subset(shplist$V1, "unclas")
  		  objectlist <- str_replace(objectlist,pattern = "E:/MetroVancouverData/eCog_output/LiDAR_areas/LiDAR_Results/temp/",replacement = "")
  		  objectlist <- str_replace(objectlist,pattern = ".shp",replacement = "")
  
  			tile.name <- objectlist[tile.idx]
  
  			objects_clip_short <- readOGR(dsn = objects.tmp.path, layer = tile.name)
  			new_names <- read_csv(file = paste0(objects.path,"/object_names_unclass.csv")) #col_names = c("row","names")
  			new_names <- (new_names$x)
  			names(objects_clip_short)
  			names(objects_clip_short) <- new_names
  			
  			# remove inf from objects_clip_short@data[, predictors]
  			if (sum(is.infinite(objects_clip_short@data$CoefVar_nDSM)) >= 1){
  			  objects_clip_short@data$CoefVar_nDSM <- replace(objects_clip_short@data$CoefVar_nDSM, 
  			                                                  is.infinite(objects_clip_short@data$CoefVar_nDSM), 0)}
  
  			#RF prediction on full map
  			Y.predicted.map <- predict(RF_complete, objects_clip_short@data[,predictors], type="response", predict.all=F, nodes=F)
  
  			params$cols.to.keep <- params$predictors.all
  			objects.map <- objects_clip_short
  			objects.map@data[, !colnames(objects.map@data) %in% params$cols.to.keep] <- list(NULL)
  			objects.map@data$pred_class <- Y.predicted.map
  
  			writeOGR(objects.map, results.dir, sprintf("Prediction_map_%s_%s_%s", gt.type, pred.type, tile.name), driver="ESRI Shapefile", overwrite_layer=TRUE)   ## write prediction map shapefile
  			
  		}
		  
		  # stopCluster(cl)
	  
    }  ## end if params$prediction.maps

  } ## end of loop over predictor types

}  ## end of loop over GT types

RES.file = file.path(results.dir, 'RESULTS_LiDAR_unclass.Rdata', fsep = .Platform$file.sep) 
save(RES, file = RES.file)
#Dave dislikes this Rdata file, so I'm going to use rds instead
saveRDS(RES, paste0(results.dir,"/results_LiDAR_unclass.RDS"))


#### PRINT LOGS ---------------------------------------------------------

## clock global time
toc <- proc.time()-tic[3]
end.message <- sprintf("Total elapsed time: %s, finished running on %s", seconds_to_period(toc[3]), Sys.time())
print(end.message)


