# create ideal points for classification

#this means amalgamating some class 3 classes into class 2 classes

#this also means using the unambiguous points cloud where shadow and other built have been prioritized

#### create Unambiguous Point file
#### Load Package ######
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
                      "forcats",
                      "stringr"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]   ## named vector members whose name is "Package"
if(length(new.packages)) install.packages(new.packages)   ## install only unavailable packages
for (pack in list.of.packages){
  library(pack, character.only=TRUE)  ## call all the packages in list.of.packages
}


#### Read in Data and set names ####
points.path <- "E:\\MetroVancouverData\\Training_Validation_Points"
points.filename <- "MetroVan_gt_Bins1_16_ideal"
points.clean <- readOGR(dsn=points.path, layer=points.filename)
points.names <- read_csv(file.path(points.path,"points_variables_tidy.csv"),col_names = FALSE)
names(points.clean@data) <- points.names$X1
points.backup <- points.clean

#### Drop Bin 15 water ####
unique(points.clean@data$Bin)
Bin15 <- which(points.clean@data$Bin == 15 & points.clean@data$Five_m_Class_3_1st_choice == "Water")
points.clean@data[Bin15,]
length(Bin15)
points.clean <- points.clean[-Bin15,]

which(points.clean@data$Five_m_Class_3_1st_choice == "Water")
waterindex <- which(points.clean@data$Five_m_Class_3_1st_choice == "Water")
points.clean@data[waterindex,] %>% 
  select(Bin, Five_m_Class_3_1st_choice)


#### write out new points ##############################################
points.path <- "E:\\MetroVancouverData\\Training_Validation_Points"
points.filename <- "MetroVan_gt_Bins1_16_ideal_lesswater"
writeOGR(points.clean, points.path, points.filename, driver="ESRI Shapefile", overwrite_layer=TRUE)


##### End ###############

