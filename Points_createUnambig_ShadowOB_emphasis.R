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
points.filename <- "MetroVan_gt_Bins1_16_tidy_shadow_OtherBuilt"
points.clean <- readOGR(dsn=points.path, layer=points.filename)
points.names <- read_csv(file.path(points.path,"points_variables_tidy.csv"),col_names = FALSE)
names(points.clean@data) <- points.names$X1


#### Remove 2nd choice points #####

#check Class 1 columns
which(!is.na(points.clean@data$One_m_Class_1_2nd_choice))
drops <- which(!is.na(points.clean@data$One_m_Class_1_2nd_choice))
lengthone <- length(drops.onem)
points.unambig <- points.clean[-drops,]
which(!is.na(points.unambig@data$One_m_Class_1_2nd_choice))

drops.fivem <- which(!is.na(points.unambig@data$Five_m_Class1_2nd_choice))
lengthfive <- length(drops.fivem)
points.unambig <- points.unambig[-drops.fivem,]
points.unambig

#check that all second choices have been removed from other columns
which(!is.na(points.unambig@data$One_m_Class_2_2nd_choice))
which(!is.na(points.unambig@data$One_m_Class_3_2nd_choice))
drops <- which(!is.na(points.unambig@data$One_m_Class_3_2nd_choice))
points.unambig <- points.unambig[-drops,]

which(!is.na(points.unambig@data$Five_m_Class1_2nd_choice))
which(!is.na(points.unambig@data$Five_m_Class_2_2nd_choice))
drops <- which(!is.na(points.unambig@data$Five_m_Class_2_2nd_choice))
points.unambig <- points.unambig[-drops,]

which(!is.na(points.unambig@data$Five_m_Class_3_2nd_choice))
drops <- which(!is.na(points.unambig@data$Five_m_Class_3_2nd_choice))
points.unambig <- points.unambig[-drops,]


#### write out new points ##############################################
points.path <- "E:\\MetroVancouverData\\Training_Validation_Points"
points.filename <- "MetroVan_gt_Bins1_16_tidy_shadow_OtherBuilt_unambig"
writeOGR(points.unambig, points.path, points.filename, driver="ESRI Shapefile", overwrite_layer=TRUE)


##### End ###############
