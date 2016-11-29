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
points.filename <- "MetroVan_gt_Bins1_16_tidy_noNA"
points.clean <- readOGR(dsn=points.path, layer=points.filename)
points.names <- read_csv(file.path(points.path,"points_variables_tidy.csv"),col_names = FALSE)
names(points.clean@data) <- points.names$X1


#### Remove 2nd choice points #####

#find shadow points in class 1 1st choice column
levels(points.clean@data$One_m_Class_1_1st_choice)
shadow.1stchoice_onem <- which(points.clean@data$One_m_Class_1_1st_choice == "Shadow")

shadow.1stchoice_fivem <- which(points.clean@data$Five_m_Class1_1st_choice == "Shadow")

# shadow.1stchoice_fivem %in% shadow.1stchoice_onem 

#find shadow points in class 1 2nd choice column

shadow.2ndchoice_onem <- which(points.clean@data$One_m_Class_1_2nd_choice == "Shadow")

shadow.2ndchoice_fivem <- which(points.clean@data$Five_m_Class1_2nd_choice == "Shadow")

# shadow.2ndchoice_fivem %in% shadow.2ndchoice_onem


# #how many water points are there?
# water.1stchoice <- which(points.clean@data$One_m_Class_1_1st_choice == "Water")
# #over a thousand - many, many more. 
# 
# #which bins is water represented in?
# points.clean@data %>% 
#   select(One_m_Class_1_1st_choice,One_m_Class_1_2nd_choice,Bin) %>% 
#   filter(One_m_Class_1_1st_choice == "Water" | One_m_Class_1_2nd_choice == "Water")
# #mostly bin 15

#which bins is shadow represented in?
points.clean@data %>% 
  select(One_m_Class_1_1st_choice,One_m_Class_1_2nd_choice,Bin) %>% 
  filter(One_m_Class_1_1st_choice == "Shadow" | One_m_Class_1_2nd_choice == "Shadow")
#all sorts

Onem_class2_1stchoice_backup <- points.clean@data$One_m_Class_2_1st_choice
Onem_class3_1stchoice_backup <- points.clean@data$One_m_Class_3_1st_choice
Fivem_class2_1stchoice_backup <- points.clean@data$Five_m_Class_2_1st_choice
Fivem_class3_1stchoice_backup <- points.clean@data$Five_m_Class_3_1st_choice

points.clean@data$One_m_Class_2_1st_choice[shadow.2ndchoice_onem] <- "Shadow"
points.clean@data$One_m_Class_3_1st_choice[shadow.2ndchoice_onem] <- "Shadow"

points.clean@data$Five_m_Class_2_1st_choice[shadow.2ndchoice_fivem] <- "Shadow"
points.clean@data$Five_m_Class_3_1st_choice[shadow.2ndchoice_fivem] <- "Shadow"

points.clean@data %>% 
  select(One_m_Class_2_1st_choice,Five_m_Class_2_1st_choice,Bin) %>% 
  filter(One_m_Class_2_1st_choice == "Shadow" | Five_m_Class_2_1st_choice == "Shadow")


points.path <- "E:\\MetroVancouverData\\Training_Validation_Points"
points.filename <- "MetroVan_gt_Bins1_16_tidy_shadow"
writeOGR(points.clean, points.path, points.filename, driver="ESRI Shapefile", overwrite_layer=TRUE)
