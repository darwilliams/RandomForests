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


#fix any mispelled class names for the ground truth points 

#the fact that the attribute table data are imported as factors is raelly helpful
#I can see all the different permutations of the same entries in the dataset
summary(points.short)
levels(points.short@data$One_m_Class_1_1st_choice)
levels(points.short@data$One_m_Class_2_1st_choice)
levels(points.short@data$One_m_Class_3_1st_choice)

levels(points.short@data$One_m_Class_1_2nd_choice)
levels(points.short@data$One_m_Class_2_2nd_choice)
levels(points.short@data$One_m_Class_3_2nd_choice)


# figure out which unique values you have
unique(points.short@data$One_m_Class_1_1st_choice)

points.short@data$One_m_Class_1_1st_choice %>% 
  gsub(pattern = "bare", replacement = "Bare")



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