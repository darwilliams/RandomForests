# Read in raw points and clean up column names, values, and export again
#note that you'll need to fix truncated column names on reimport

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


#### Read in Data ##### 

points.path <- "E:\\MetroVancouverData\\Training_Validation_Points"
points.filename <- "MetroVan_gt_Bins1_16"
points.raw <- readOGR(dsn=points.path, layer=points.filename) 


# change column names to be meaningful for points and objects
new_names <- read_csv("E:\\MetroVancouverData\\Training_Validation_Points\\Bufs_joined\\Bin1point_buf_joinfieldnames.csv",col_names = FALSE)
new_names$X1
names(points.raw) <- new_names$X1

# drop previous spatial join info
names(points.raw)
drops2 <- c("CID.x","ORIG_FID","Shape_Leng","distance","CID.y","coords.x1","coords.x2")
points.raw.short <- points.raw[,!(names(points.raw) %in% drops2)]
names(points.raw.short)
head(points.raw.short@data)

# remove NA rows
indices <- !is.na(points.raw.short@data$One_m_Class_1_1st_choice)
indices
points.short <- (points.raw.short[which(indices),])
dim((points.raw.short))#should be 7506
dim((points.short)) #should be equal for Class 1

#note that for clouds/ice and shadow only class 1 exists.
#These values will have to be duplicated into class 2 and 3 slots for use in RF




#fix any mispelled class names for the ground truth points 

#the fact that the attribute table data are imported as factors is really helpful
#I can see all the different permutations of the same entries in the dataset
(str(points.short@data))
summary(points.short)
levels(points.short@data$One_m_Class_1_1st_choice)
levels(points.short@data$One_m_Class_2_1st_choice)
levels(points.short@data$One_m_Class_3_1st_choice)

levels(points.short@data$One_m_Class_1_2nd_choice)
levels(points.short@data$One_m_Class_2_2nd_choice)
levels(points.short@data$One_m_Class_3_2nd_choice)


##### ID unique values and tidy #####

##### One_m_Class_1_1st_choice ####
unique(points.short@data$One_m_Class_1_1st_choice)

points.short@data$One_m_Class_1_1st_choice <- points.short@data$One_m_Class_1_1st_choice %>% 
  gsub(pattern = "bare", replacement = "Bare")

points.short@data$One_m_Class_1_1st_choice <- points.short@data$One_m_Class_1_1st_choice %>% 
  gsub(pattern = "bare_$", replacement = "Bare")

points.short@data$One_m_Class_1_1st_choice <- points.short@data$One_m_Class_1_1st_choice %>% 
  gsub(pattern = "Bare_", replacement = "Bare")

points.short@data$One_m_Class_1_1st_choice <- points.short@data$One_m_Class_1_1st_choice %>% 
  gsub(pattern = "built-up", replacement = "Built-up")

points.short@data$One_m_Class_1_1st_choice <- points.short@data$One_m_Class_1_1st_choice %>% 
  gsub(pattern = "Built_up", replacement = "Built-up")

points.short@data$One_m_Class_1_1st_choice <- points.short@data$One_m_Class_1_1st_choice %>% 
  gsub(pattern = "Vegeation", replacement = "Vegetation")

points.short@data$One_m_Class_1_1st_choice <- points.short@data$One_m_Class_1_1st_choice %>% 
  gsub(pattern = "vegetation", replacement = "Vegetation")

points.short@data$One_m_Class_1_1st_choice <- points.short@data$One_m_Class_1_1st_choice %>%
  gsub(pattern = "^Veg$", replacement = "Vegetation")

points.short@data$One_m_Class_1_1st_choice <- points.short@data$One_m_Class_1_1st_choice %>%
  gsub(pattern = "^veg$", replacement = "Vegetation")

points.short@data$One_m_Class_1_1st_choice <- points.short@data$One_m_Class_1_1st_choice %>%
  gsub(pattern = "shadow", replacement = "Shadow")

points.short@data$One_m_Class_1_1st_choice <- points.short@data$One_m_Class_1_1st_choice %>%
  gsub(pattern = "^Ice$", replacement = "Clouds/Ice")

a <- points.short
a <- which(a@data$One_m_Class_1_1st_choice == "non-photosynthetic_veg")
points.short@data[a,]
levels(points.short@data$One_m_Class_2_1st_choice)
points.short@data$One_m_Class_2_1st_choice[a] <- "Non-photosynthetic"
points.short@data$One_m_Class_2_1st_choice[a]
points.short@data[a,]

points.short@data$One_m_Class_1_1st_choice <- points.short@data$One_m_Class_1_1st_choice %>% 
  gsub(pattern = "non-photosynthetic_veg", replacement = "Vegetation")

levels(points.short@data$Five_m_Class_2_1st_choice)
points.short@data$Five_m_Class_2_1st_choice[a] <- "Non-photosynthetic"
points.short@data$Five_m_Class_2_1st_choice[a]
points.short@data[a,]

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>% 
  gsub(pattern = "non-photosynthetic_veg", replacement = "Vegetation")

points.short@data[a,]

rm(a)

unique(points.short@data$One_m_Class_1_1st_choice)

points.short@data$One_m_Class_1_1st_choice <- as.factor(points.short@data$One_m_Class_1_1st_choice)

levels(points.short@data$One_m_Class_1_1st_choice)

##### One_m_Class_2_1st_choice ####

levels(points.short@data$One_m_Class_2_1st_choice)
unique(points.short@data$One_m_Class_2_1st_choice)

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "barren", replacement = "Barren")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "BArren", replacement = "Barren")

#check to make sure "Bare" classification isn't a hierarchy screw up
b <- points.short
b <- which(b@data$One_m_Class_2_1st_choice == "Bare")
points.short@data[b,]
rm(b)
#looks like it can be changed to Barren

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Bare", replacement = "Barren")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Building", replacement = "Buildings")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "BUilding", replacement = "Buildings")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Buildingss", replacement = "Buildings")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "building", replacement = "Buildings")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "^Grass$", replacement = "Grass-herb")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Grass-Hern", replacement = "Grass-herb")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Grass-Herb", replacement = "Grass-herb")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Grass_Herb", replacement = "Grass-herb")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "grass-herb", replacement = "Grass-herb")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Non-photosynthic", replacement = "Non-photosynthetic")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Non-photostnthetic", replacement = "Non-photosynthetic")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Non_photosynthetic", replacement = "Non-photosynthetic")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Non-photsynthetic", replacement = "Non-photosynthetic")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "paved", replacement = "Paved")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Paved_", replacement = "Paved")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "PAved", replacement = "Paved")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "SHrub", replacement = "Shrub")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "shrub", replacement = "Shrub")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Shrubs", replacement = "Shrub")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "shurb", replacement = "Shrub")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "SOil", replacement = "Soil")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "soil", replacement = "Soil")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Tree_Canopy", replacement = "Tree_canopy")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "tree_canopy", replacement = "Tree_canopy")

points.short@data$One_m_Class_2_1st_choice <- points.short@data$One_m_Class_2_1st_choice %>% 
  gsub(pattern = "Tree_canopy", replacement = "Trees")

points.short@data$One_m_Class_2_1st_choice <- as.factor(points.short@data$One_m_Class_2_1st_choice)
levels(points.short@data$One_m_Class_2_1st_choice)

##### One_m_Class_3_1st_choice ####
levels(points.short@data$One_m_Class_3_1st_choice)
unique(points.short@data$One_m_Class_3_1st_choice)

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Natual_Barren", replacement = "Natural_barren")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Natural_Barre", replacement = "Natural_barren")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Natural_barrenn", replacement = "Natural_barren")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modifeid_G-H", replacement = "Modified_G-H")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "modified_G-H", replacement = "Modified_G-H")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_G_H", replacement = "Modified_G-H")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "modified_grass-herb", replacement = "Modified_G-H")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Mofified_G-H", replacement = "Modified_G-H")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_G-H`", replacement = "Modified_G-H")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear_Paved", replacement = "Linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^linear_paved", replacement = "Linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear_Paved_", replacement = "Linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear_paved_", replacement = "Linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non_linear_Paved", replacement = "Non-linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non_Linear_paved", replacement = "Non-linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non_linear_paved", replacement = "Non-linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non-Linear_paved", replacement = "Non-linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "non-linear_Paved", replacement = "Non-linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "non-Linear_paved", replacement = "Non-linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non-linear_Paved", replacement = "Non-linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non-linear_paved", replacement = "Non-linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non-Linear_Paved", replacement = "Non-linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "non-linear_paved", replacement = "Non-linear_paved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modifiied_Barren", replacement = "Modified_barren")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_Barren", replacement = "Modified_barren")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_BArren", replacement = "Modified_barren")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modifeid_Soil", replacement = "Modified_soil")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_Soil", replacement = "Modified_soil")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "modified_soil", replacement = "Modified_soil")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_soil", replacement = "Modified_soil")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "modified_Soil", replacement = "Modified_soil")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Unpaved_Linear", replacement = "Linear_unpaved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "UnPaved_Linear", replacement = "Linear_unpaved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Unpaved_linear", replacement = "Linear_unpaved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "unpaved_linear", replacement = "Linear_unpaved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "linear_unpaved", replacement = "Linear_unpaved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "unpaved_Linear", replacement = "Linear_unpaved")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Linear_pavedelev", replacement = "Linear_paved_elevated")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Decidous", replacement = "Deciduous")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "deciduous", replacement = "Deciduous")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "Confierous", replacement = "Coniferous")

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Mixed$", replacement = "Mixed_tree")

#to check
"Non-linear"
a <- points.short
a <- which(a@data$One_m_Class_3_1st_choice == "Non-linear")
points.short@data[a,]

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Non-linear$", replacement = "Non-linear_paved")

"Non-Linear"
a <- points.short
a <- which(a@data$One_m_Class_3_1st_choice == "Non-Linear")
points.short@data[a,]

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Non-Linear$", replacement = "Non-linear_paved")

"Natural"
a <- points.short
a <- which(a@data$One_m_Class_3_1st_choice == "Natural")
points.short@data[a,]

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Natural$", replacement = "Natural_G-H")

"Canopy"
a <- points.short
a <- which(a@data$One_m_Class_3_1st_choice == "Canopy")
points.short@data[a,]
rm(a)

points.short@data$One_m_Class_3_1st_choice <- points.short@data$One_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Canopy$", replacement = "Coniferous")

unique(points.short@data$One_m_Class_3_1st_choice)
points.short@data$One_m_Class_3_1st_choice <- as.factor(points.short@data$One_m_Class_3_1st_choice)
levels(points.short@data$One_m_Class_3_1st_choice)

#### Five_m_Class1_1st_choice #####

unique(points.short@data$Five_m_Class1_1st_choice)

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>% 
  gsub(pattern = "bare", replacement = "Bare")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>% 
  gsub(pattern = "bare_$", replacement = "Bare")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>% 
  gsub(pattern = "Bare_", replacement = "Bare")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>% 
  gsub(pattern = "built-up", replacement = "Built-up")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>% 
  gsub(pattern = "Built_up", replacement = "Built-up")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>% 
  gsub(pattern = "Built-up_", replacement = "Built-up")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>% 
  gsub(pattern = "Built_-up", replacement = "Built-up")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>% 
  gsub(pattern = "Vegeation", replacement = "Vegetation")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>% 
  gsub(pattern = "vegetation", replacement = "Vegetation")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>% 
  gsub(pattern = "Vegetaion", replacement = "Vegetation")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>%
  gsub(pattern = "^Veg$", replacement = "Vegetation")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>%
  gsub(pattern = "^Vef$", replacement = "Vegetation")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>%
  gsub(pattern = "^veg$", replacement = "Vegetation")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>%
  gsub(pattern = "shadow", replacement = "Shadow")

points.short@data$Five_m_Class1_1st_choice <- points.short@data$Five_m_Class1_1st_choice %>%
  gsub(pattern = "^Ice$", replacement = "Clouds/Ice")

unique(points.short@data$Five_m_Class1_1st_choice)

points.short@data$Five_m_Class1_1st_choice <- as.factor(points.short@data$Five_m_Class1_1st_choice)

levels(points.short@data$Five_m_Class1_1st_choice)

#### Five_m_Class_2_1st_choice ####
unique(points.short@data$Five_m_Class_2_1st_choice)
levels(points.short@data$Five_m_Class_2_1st_choice)

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "barren", replacement = "Barren")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "BArren", replacement = "Barren")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Bareen", replacement = "Barren")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Barren_", replacement = "Barren")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Buid", replacement = "Buildings")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Building", replacement = "Buildings")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "building", replacement = "Buildings")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Buildings_", replacement = "Buildings")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Buildingss", replacement = "Buildings")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "grass-herb", replacement = "Grass-herb")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Grass-Herb", replacement = "Grass-herb")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Grass_Herb", replacement = "Grass-herb")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Non-_Photosynthetic", replacement = "Non-photosynthetic")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Non-Photosynthetic", replacement = "Non-photosynthetic")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Non-photsynthetic", replacement = "Non-photosynthetic")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Non_photostnthetic", replacement = "Non-photosynthetic")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Non_photosynthetic", replacement = "Non-photosynthetic")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "other_built", replacement = "Other_built")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Other_Built", replacement = "Other_built")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "paved", replacement = "Paved")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Paved_", replacement = "Paved")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Paved_", replacement = "Paved")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Paved__", replacement = "Paved")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "PAved", replacement = "Paved")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "shrub", replacement = "Shrub")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Shrubs", replacement = "Shrub")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "soil", replacement = "Soil")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "tree_Canopy", replacement = "Trees")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Tree_Canopy", replacement = "Trees")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Tree_Canopy_", replacement = "Trees")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Tree_canopy_", replacement = "Trees")

points.short@data$Five_m_Class_2_1st_choice <- points.short@data$Five_m_Class_2_1st_choice %>% 
  gsub(pattern = "Tree_canopy", replacement = "Trees")

points.short@data$Five_m_Class_2_1st_choice <- as.factor(points.short@data$Five_m_Class_2_1st_choice)
levels(points.short@data$Five_m_Class_2_1st_choice)

#### For all 1st choice shrub entries, assign shrub to Class 3 and turn Class 2 shrubs into "Trees" ####

a <- points.short
a <- which(a@data$One_m_Class_2_1st_choice == "Shrub")
points.short@data[a,]
levels(points.short@data$One_m_Class_3_1st_choice)
points.short@data$One_m_Class_3_1st_choice[a] <- "Shrub"
points.short@data$One_m_Class_3_1st_choice[a]
head(points.short@data[a,])
points.short@data$One_m_Class_2_1st_choice[a] <- "Trees"
head(points.short@data[a,])

a <- points.short
a <- which(a@data$Five_m_Class_2_1st_choice == "Shrub")
points.short@data[a,]
levels(points.short@data$Five_m_Class_3_1st_choice)
points.short@data$Five_m_Class_3_1st_choice[a] <- "Shrub"
points.short@data$Five_m_Class_3_1st_choice[a]
head(points.short@data[a,])
points.short@data$Five_m_Class_2_1st_choice[a] <- "Trees"
head(points.short@data[a,])

a <- points.short
a <- which(a@data$One_m_Class_2_2nd_choice == "Shrub")
points.short@data[a,]
levels(points.short@data$One_m_Class_3_2nd_choice)
points.short@data$One_m_Class_3_2nd_choice[a] <- "Shrub"
points.short@data$One_m_Class_3_2nd_choice[a]
head(points.short@data[a,])
points.short@data$One_m_Class_2_2nd_choice[a] <- "Trees"
head(points.short@data[a,])

a <- points.short
a <- which(a@data$Five_m_Class_2_2nd_choice == "Shrub")
points.short@data[a,]
levels(points.short@data$Five_m_Class_3_2nd_choice)
points.short@data$Five_m_Class_3_2nd_choice[a] <- "Shrub"
points.short@data$Five_m_Class_3_2nd_choice[a]
head(points.short@data[a,])
points.short@data$Five_m_Class_2_2nd_choice[a] <- "Trees"
head(points.short@data[a,])

#### Five_m_Class_3_1st_choice ####

levels(points.short@data$Five_m_Class_3_1st_choice)

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Confierous", replacement = "Coniferous")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Coniferious", replacement = "Coniferous")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Decidious", replacement = "Deciduous")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Decidous", replacement = "Deciduous")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Deciduoud", replacement = "Deciduous")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "deciduous", replacement = "Deciduous")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^linear_paved", replacement = "Linear_paved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear_Paved", replacement = "Linear_paved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear_Paved_", replacement = "Linear_paved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear_paved_", replacement = "Linear_paved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "linear_unpaved", replacement = "Linear_unpaved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "unpaved_linear", replacement = "Linear_unpaved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Unpaved_linear", replacement = "Linear_unpaved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Unpaved_Linear", replacement = "Linear_unpaved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "UnPaved_Linear", replacement = "Linear_unpaved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Mixed$", replacement = "Mixed_tree")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modfied_Barren", replacement = "Modified_barren")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_Barren", replacement = "Modified_barren")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Barren_Modified", replacement = "Modified_barren")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modifeid_G-H", replacement = "Modified_G-H")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modifid_G-H", replacement = "Modified_G-H")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_G-h", replacement = "Modified_G-H")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "modified_grass-herb", replacement = "Modified_G-H")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_grass-herb", replacement = "Modified_G-H")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_Grass_Herb", replacement = "Modified_G-H")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modifies_G-H", replacement = "Modified_G-H")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "modified_soil", replacement = "Modified_soil")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_Soil", replacement = "Modified_soil")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_SOil", replacement = "Modified_soil")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Modified_soil", replacement = "Modified_soil")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "modified_Soil", replacement = "Modified_soil")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Natural_Bare", replacement = "Natural_barren")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Natural_Baren", replacement = "Natural_barren")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Natural_Barre", replacement = "Natural_barren")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Natural_Barren", replacement = "Natural_barren")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Natural_BArren", replacement = "Natural_barren")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Natural_Barrren", replacement = "Natural_barren")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Naural_Barren", replacement = "Natural_barren")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Natural_barrenn", replacement = "Natural_barren")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non-linear_Paved", replacement = "Non-linear_paved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non-Linear_paved", replacement = "Non-linear_paved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non-Linear_Paved", replacement = "Non-linear_paved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non-linear_paveed", replacement = "Non-linear_paved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non_linear_Paved", replacement = "Non-linear_paved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Non_Linear_paved", replacement = "Non-linear_paved")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Linear_paved_elev", replacement = "Linear_paved_elevated")

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "Linear_pavedelev", replacement = "Linear_paved_elevated")

#to check
# "Linear"
# "Non-linear"
#"Water"

#"Non-linear"
a <- points.short
a <- which(a@data$Five_m_Class_3_1st_choice == "Non-linear")
points.short@data[a,]

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Non-linear$", replacement = "Non-linear_paved")

#"Linear"
a <- points.short
a <- which(a@data$Five_m_Class_3_1st_choice == "Linear")
points.short@data[a,]

points.short@data$Five_m_Class_3_1st_choice <- points.short@data$Five_m_Class_3_1st_choice %>% 
  gsub(pattern = "^Linear$", replacement = "Linear_paved")

#"Water"
a <- points.short
a <- which(a@data$Five_m_Class_3_1st_choice == "Water")
points.short@data[a,]

points.short@data$Five_m_Class1_2nd_choice[a] <- "Water"
points.short@data$Five_m_Class_2_1st_choice[a] <- NA
points.short@data$Five_m_Class_2_2nd_choice[a] <- "Water"
points.short@data$Five_m_Class_3_1st_choice[a] <- NA


unique(points.short@data$Five_m_Class_3_1st_choice)
points.short@data$Five_m_Class_3_1st_choice <- as.factor(points.short@data$Five_m_Class_3_1st_choice)
levels(points.short@data$Five_m_Class_3_1st_choice)

#### 2nd choice variables #### 

#### One_m_Class_1_2nd_choice ####

unique(points.short@data$One_m_Class_1_2nd_choice)



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

points.short@data[,5] <- gsub(
  x = points.short@data[,5], 
  pattern = "SHrub", 
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

points.short@data[,11] <- gsub(
  x = points.short@data[,5], 
  pattern = "SHrub", 
  replacement = "Trees")

unique(points.short@data[,11])

### checking to make sure points numbers are respected when filtering
# a <- points.short@data %>% 
#   select(Point_Number,Shape_Area,Onem_Class_2_1st_choice)
# a
# 
# b <- points.short@data %>% 
#   select(Point_Number,Shape_Area,Onem_Class_2_1st_choice) %>% 
#   filter(!(Onem_Class_2_1st_choice == "Trees" | Onem_Class_2_1st_choice == "Building"))
# b  
# 
# length(a[,2])
# length(b[,2])
# 
# a[,1] %in% b[,1]
# a[,2] %in% b[,2]
# 
# head(a,15)
# head(b)
# tail(a,15)
# tail(b)
# 
# filter(points.short@data, !(Onem_Class_2_1st_choice == "Trees" | Onem_Class_2_1st_choice == "Building"))

#remove building or tree points
# points.short@data <- filter(points.short@data, !(Onem_Class_2_1st_choice == "Trees" | Onem_Class_2_1st_choice == "Building"))
unique(points.short@data[,5])
unique(points.short@data[,11])

points.short@data %>% 
  select(Point_Number,Onem_Class_2_1st_choice)

#write out cleaned up points
writeOGR(points.short, points.path, sprintf("%s_%s", points.filename, tile.name), driver="ESRI Shapefile", overwrite_layer=TRUE)  