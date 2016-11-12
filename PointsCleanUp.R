# Read in raw points and clean up column names, values, and export again
#note that you'll need to fix truncated column names on reimport

points.raw <- readOGR(dsn=points.path, layer=points.filename) 


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