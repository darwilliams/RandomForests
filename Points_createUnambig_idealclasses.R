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

#so basically what you want to do is add the levels of class 2 to class 3
#then change the names of paved, barren and soil entried to their class 2 name
#then drop unused levels in class 3

#### One m class 3 #######
(a <- levels(points.clean@data$One_m_Class_3_1st_choice))
(b <- levels(points.clean@data$One_m_Class_2_1st_choice))
length(a)+length(b)
fct_expand(points.clean@data$One_m_Class_3_1st_choice,points.clean@data$One_m_Class_2_1st_choice)

#change paved names
points.clean@data$One_m_Class_3_1st_choice <- points.clean@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear_paved$", replacement = "Paved")

points.clean@data$One_m_Class_3_1st_choice <- points.clean@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear_paved_elevated$", replacement = "Paved")

points.clean@data$One_m_Class_3_1st_choice <- points.clean@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Non-linear_paved$", replacement = "Paved")

#change barren names
points.clean@data$One_m_Class_3_1st_choice <- points.clean@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Modified_barren$", replacement = "Barren")

points.clean@data$One_m_Class_3_1st_choice <- points.clean@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Natural_barren$", replacement = "Barren")

points.clean@data$One_m_Class_3_1st_choice <- points.clean@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear_unpaved$", replacement = "Barren")

#change soil names
points.clean@data$One_m_Class_3_1st_choice <- points.clean@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Modified_soil$", replacement = "Soil")

points.clean@data$One_m_Class_3_1st_choice <- points.clean@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Natural_Soil$", replacement = "Soil")

points.clean@data$One_m_Class_3_1st_choice <- as.factor(points.clean@data$One_m_Class_3_1st_choice)
levels(points.clean@data$One_m_Class_3_1st_choice)

#### Five m class 3 #######
c <- levels(points.clean@data$Five_m_Class_3_1st_choice)
d <- levels(points.clean@data$Five_m_Class_2_1st_choice)
length(c)+length(d)
fct_expand(points.clean@data$Five_m_Class_3_1st_choice,points.clean@data$Five_m_Class_2_1st_choice)

points.clean@data$Five_m_Class_3_1st_choice <- points.clean@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear_paved$", replacement = "Paved")

points.clean@data$Five_m_Class_3_1st_choice <- points.clean@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear_paved_elevated$", replacement = "Paved")

points.clean@data$Five_m_Class_3_1st_choice <- points.clean@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Non-linear_paved$", replacement = "Paved")

#change barren names
points.clean@data$Five_m_Class_3_1st_choice <- points.clean@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Modified_barren$", replacement = "Barren")

points.clean@data$Five_m_Class_3_1st_choice <- points.clean@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Natural_barren$", replacement = "Barren")

points.clean@data$Five_m_Class_3_1st_choice <- points.clean@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear_unpaved$", replacement = "Barren")

#change soil names
points.clean@data$Five_m_Class_3_1st_choice <- points.clean@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Modified_soil$", replacement = "Soil")

points.clean@data$Five_m_Class_3_1st_choice <- points.clean@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Natural_Soil$", replacement = "Soil")

points.clean@data$Five_m_Class_3_1st_choice <- as.factor(points.clean@data$Five_m_Class_3_1st_choice)
levels(points.clean@data$Five_m_Class_3_1st_choice)


#why is there vegetation in class 3?
vegindex <- which(points.clean@data$Five_m_Class_3_1st_choice == "Vegetation")
points.clean@data[vegindex,]

points.clean@data$Five_m_Class_3_1st_choice <- points.clean@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Vegetation$", replacement = "Modified_G-H")

points.clean@data$Five_m_Class_3_1st_choice <- as.factor(points.clean@data$Five_m_Class_3_1st_choice)
levels(points.clean@data$Five_m_Class_3_1st_choice)

str(points.clean@data)

#### write out new points ##############################################
points.path <- "E:\\MetroVancouverData\\Training_Validation_Points"
points.filename <- "MetroVan_gt_Bins1_16_ideal"
writeOGR(points.clean, points.path, points.filename, driver="ESRI Shapefile", overwrite_layer=TRUE)


##### End ###############


