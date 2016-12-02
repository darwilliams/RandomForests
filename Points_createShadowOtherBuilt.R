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


#### Read in Data and set names #########################################################
points.path <- "E:\\MetroVancouverData\\Training_Validation_Points"
points.filename <- "MetroVan_gt_Bins1_16_tidy_noNA"
points.clean <- readOGR(dsn=points.path, layer=points.filename)
points.names <- read_csv(file.path(points.path,"points_variables_tidy.csv"),col_names = FALSE)
names(points.clean@data) <- points.names$X1


#### Create 1st choice back-ups #######################################################
Onem_class2_1stchoice_backup <- points.clean@data$One_m_Class_2_1st_choice
Onem_class3_1stchoice_backup <- points.clean@data$One_m_Class_3_1st_choice
Fivem_class2_1stchoice_backup <- points.clean@data$Five_m_Class_2_1st_choice
Fivem_class3_1stchoice_backup <- points.clean@data$Five_m_Class_3_1st_choice


#### Shadow Exploration #################################

#are there multiple spellings of Shadow?
levels(points.clean@data$One_m_Class_1_1st_choice)
levels(points.clean@data$One_m_Class_1_2nd_choice)
levels(points.clean@data$One_m_Class_2_1st_choice)
levels(points.clean@data$One_m_Class_2_2nd_choice)
levels(points.clean@data$One_m_Class_3_1st_choice)
levels(points.clean@data$One_m_Class_3_2nd_choice)
levels(points.clean@data$Five_m_Class1_1st_choice)
levels(points.clean@data$Five_m_Class1_2nd_choice)
levels(points.clean@data$Five_m_Class_2_1st_choice)
levels(points.clean@data$Five_m_Class_2_2nd_choice)
levels(points.clean@data$Five_m_Class_3_1st_choice)
levels(points.clean@data$Five_m_Class_3_2nd_choice)
#no, but there are multiple spelling of Other Built - see below

#which bins is shadow represented in?
(points.clean@data %>% 
  select(One_m_Class_1_1st_choice,One_m_Class_1_2nd_choice,Bin) %>% 
  filter(One_m_Class_1_1st_choice == "Shadow" | One_m_Class_1_2nd_choice == "Shadow"))
#many different bins

#### Create Shadow Indexes ###################

#find shadow points in class 1 1st choice column
levels(points.clean@data$One_m_Class_1_1st_choice)
shadow.1stchoice_onem <- which(points.clean@data$One_m_Class_1_1st_choice == "Shadow")

shadow.1stchoice_fivem <- which(points.clean@data$Five_m_Class1_1st_choice == "Shadow")

# shadow.1stchoice_fivem %in% shadow.1stchoice_onem 

#find shadow points in class 1 2nd choice column

shadow.2ndchoice_onem <- which(points.clean@data$One_m_Class_1_2nd_choice == "Shadow")

shadow.2ndchoice_fivem <- which(points.clean@data$Five_m_Class1_2nd_choice == "Shadow")

# shadow.2ndchoice_fivem %in% shadow.2ndchoice_onem

#### Assign Second Choice Shadow to First Choice ################################

#remove second choices for 1st choice shadow poionts
points.clean@data$One_m_Class_2_1st_choice[shadow.1stchoice_onem] #should be all shadow
points.clean@data$One_m_Class_2_2nd_choice[shadow.1stchoice_onem] <- NA
points.clean@data$One_m_Class_3_1st_choice[shadow.1stchoice_onem] #should be all shadow
points.clean@data$One_m_Class_3_2nd_choice[shadow.1stchoice_onem] <- NA
points.clean@data$Five_m_Class_2_1st_choice[shadow.1stchoice_fivem] # all shadow?
points.clean@data$Five_m_Class_2_2nd_choice[shadow.1stchoice_fivem] <- NA
points.clean@data$Five_m_Class_3_1st_choice[shadow.1stchoice_fivem]
points.clean@data$Five_m_Class_3_2nd_choice[shadow.1stchoice_fivem] <- NA



points.clean@data$One_m_Class_1_2nd_choice[shadow.2ndchoice_onem] #should be all shadow
points.clean@data[shadow.2ndchoice_onem,]
points.clean@data$One_m_Class_2_1st_choice[shadow.2ndchoice_onem] <- "Shadow"
points.clean@data$One_m_Class_3_1st_choice[shadow.2ndchoice_onem] <- "Shadow"
#remove second choices if necessary
points.clean@data$One_m_Class_2_2nd_choice[shadow.2ndchoice_onem] <- NA
points.clean@data$One_m_Class_3_2nd_choice[shadow.2ndchoice_onem] <- NA


points.clean@data$Five_m_Class1_2nd_choice[shadow.2ndchoice_fivem] #should be all shadow
points.clean@data[shadow.2ndchoice_fivem,]
points.clean@data$Five_m_Class_2_1st_choice[shadow.2ndchoice_fivem] <- "Shadow"
points.clean@data$Five_m_Class_3_1st_choice[shadow.2ndchoice_fivem] <- "Shadow"
#remove second choices if necessary
points.clean@data$Five_m_Class_2_2nd_choice[shadow.2ndchoice_fivem] <- NA
points.clean@data$Five_m_Class_3_2nd_choice[shadow.2ndchoice_fivem] <- NA

#sanity check
points.clean@data %>% 
  select(One_m_Class_2_1st_choice,One_m_Class_2_2nd_choice,Five_m_Class_2_1st_choice,Five_m_Class_2_2nd_choice,Bin) %>% 
  filter(One_m_Class_2_1st_choice == "Shadow" | Five_m_Class_2_1st_choice == "Shadow")
#good

#### Other Built exploration ###############################

#how many other built points are there in total and which bins are they in?

# one m
(OB_1m <- points.clean@data %>% 
  select(One_m_Class_2_1st_choice,One_m_Class_2_2nd_choice,Bin) %>% 
  filter(One_m_Class_2_1st_choice == "Other_Built" | One_m_Class_2_2nd_choice == "Other_Built" | 
           One_m_Class_2_1st_choice == "Other_built" | One_m_Class_2_2nd_choice == "Other_built"))
#60 points in total, with 6 second choices
# 1st choice spelling "Other_Built" while 2nd choice spelling Other_built

# five m
(OB_5m <- points.clean@data %>% 
  select(Five_m_Class_2_1st_choice,Five_m_Class_2_2nd_choice,Bin) %>% 
  filter(Five_m_Class_2_1st_choice == "Other_Built" | Five_m_Class_2_2nd_choice == "Other_Built" | 
           Five_m_Class_2_1st_choice == "Other_built" | Five_m_Class_2_2nd_choice == "Other_built" ))
#66 points in total
# confusingly, 1st choice spelling is Other_built, 2nd choice spelling is Other_Built

#### Create other built indexes ###############################
OB_1m_1choice_index <- which(points.clean@data$One_m_Class_2_1st_choice == "Other_Built")

OB_1m_2choice_index <- which(points.clean@data$One_m_Class_2_2nd_choice == "Other_built")

OB_5m_1choice_index <- which(points.clean@data$Five_m_Class_2_1st_choice == "Other_built")

OB_5m_2choice_index <- which(points.clean@data$Five_m_Class_2_2nd_choice == "Other_Built")

#any common points?
OB_5m_1choice_index %in% OB_1m_1choice_index
#all
OB_1m_1choice_index %in% OB_5m_1choice_index
#most

OB_1m_2choice_index %in% OB_5m_2choice_index
#most
OB_5m_2choice_index %in% OB_1m_2choice_index
#few

#### Remove Second Choice for other Built ########################################
#decided against putting second choice as first choice in this case
#it makes more sense for shadow

#for one m points
points.clean@data$One_m_Class_2_1st_choice[OB_1m_1choice_index] #should be all other built
points.clean@data$One_m_Class_2_2nd_choice[OB_1m_1choice_index] <- NA
points.clean@data$One_m_Class_3_1st_choice[OB_1m_1choice_index] #also should be all other built
points.clean@data$One_m_Class_3_2nd_choice[OB_1m_1choice_index] <- NA

#for five m points
points.clean@data$Five_m_Class_2_1st_choice[OB_5m_1choice_index] #should be all other built
points.clean@data$Five_m_Class_2_2nd_choice[OB_5m_1choice_index] <- NA
points.clean@data$Five_m_Class_3_1st_choice[OB_5m_1choice_index]
points.clean@data$Five_m_Class_3_2nd_choice[OB_5m_1choice_index] <- NA

#### make other_built names consistent ####################################################
#is there a consistent set of other built points now?
which(points.clean@data$One_m_Class_2_1st_choice == "Other_Built")
which(points.clean@data$One_m_Class_3_1st_choice == "Other_Built")
which(points.clean@data$Five_m_Class_2_1st_choice == "Other_built")
which(points.clean@data$Five_m_Class_3_1st_choice == "Other_built")

#yes. So choose a spelling and stick with it. 

points.clean@data$One_m_Class_2_1st_choice <- points.clean@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "^Other_Built$", replacement = "Other_built")
points.clean@data$One_m_Class_2_1st_choice <- as.factor(points.clean@data$One_m_Class_2_1st_choice)

points.clean@data$One_m_Class_3_1st_choice <- points.clean@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Other_Built$", replacement = "Other_built")
points.clean@data$One_m_Class_3_1st_choice <- as.factor(points.clean@data$One_m_Class_3_1st_choice)

#### Write out points ####
points.path <- "E:\\MetroVancouverData\\Training_Validation_Points"
points.filename <- "MetroVan_gt_Bins1_16_tidy_shadow_OtherBuilt"
writeOGR(points.clean, points.path, points.filename, driver="ESRI Shapefile", overwrite_layer=TRUE)
