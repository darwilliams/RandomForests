#Intersect points buffers with image objects from LiDAR coverage areas

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
                      "forcats"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]   ## named vector members whose name is "Package"
if(length(new.packages)) install.packages(new.packages)   ## install only unavailable packages
for (pack in list.of.packages){
  library(pack, character.only=TRUE)  ## call all the packages in list.of.packages
}

#### Read in Data ##### 

# Read data tables
table.path <- "C:\\Users\\darwil.stu\\Documents\\Accuracy Assessment\\"
x <- 1:15
filenames<- paste0("Bin",x)

for (n in 1:length(filenames)){
    x <- read_csv(paste0(table.path,filenames[n],".csv"))
    assign(filenames[n],x)
    rm(x)
}

#Read shp files of points
shp.path <- "E:\\MetroVancouverData\\Training_Validation_Points"
x <- 1:15
shpnames <- paste0("Bin",x,"shp")

for (n in 1:length(filenames)){
  x <- readOGR(dsn=shp.path, layer = paste0(filenames[n]))
  assign(shpnames[n],x)
  rm(x)
}

#Read in buffer shp files
x <- 1:15
bufnames <- paste0("Bin",x,"_buf")
bufshpnames <- paste0("Bin",x,"_buf","shp")
for (n in 1:length(bufnames)){
  x <- readOGR(dsn=shp.path, layer = paste0(bufnames[n]))
  assign(bufshpnames[n],x)
  rm(x)
}

#join the csvs to the points
x <- 1:15
data_join_names <- paste0("Bin",x,"shp_csvJoin")
# shpnames
# filenames

for(n in 1:length(filenames)){
  y <- get(shpnames[n])@data %>% 
    inner_join(get(filenames[n]), by = c("PointID" = "Point_Number"))
  z <- get(shpnames[n])
  z@data <- y
  assign(data_join_names[n],z)
  rm(y)
  rm(z)
}

#create PointID variable for buffer polygons from ORIG_FID variable
x <- 1:15
buf_join_names <- paste0("Bin",x,"_buf","shp_join") 
  
for(n in 1:length(bufshpnames)){
  y <- get(bufshpnames[n])@data %>% 
    mutate(PointID = ORIG_FID + 1)
  z <- get(bufshpnames[n])
  z@data <- y
  assign(buf_join_names[n],z)
  rm(y)
  rm(z)
}

#join the expanded points data to the points buffers
x <- 1:15
shp_join_names <- paste0("Bin",x,"point_buf_join")
# buf_join_names
# data_join_names

for(n in 1:length(buf_join_names)){
  y <- get(buf_join_names[n])@data %>% 
    inner_join(get(data_join_names[n]), by = "PointID", copy = TRUE)
  z <- get(buf_join_names[n])
  z@data <- y
  assign(shp_join_names[n],z)
  rm(y)
  rm(z)
}

# #do spatial join for points and polygons
# x <- 1:15
# sjnames <- paste0("Bin",x,"_SJ")
# n <- 1
# for (n in 1: length(data_join_names)){
#   y <- over(get(bufshpnames[n]),get(data_join_names[n]), returnList = TRUE)
#   assign(sjnames[n],y)
#   rm(y)
# }

#write bin#point_buf_join polygons out
#field names exported for use in RF_Van script
for (n in 1:length(shp_join_names)){
  writeOGR(get(shp_join_names[n]),shp.path,shp_join_names[n],driver = "ESRI Shapefile")
  fieldnames <- as.data.frame(names(get(shp_join_names[n])))
  write_csv(fieldnames,paste0(shp.path,"\\",shp_join_names[n],"fieldnames.csv"),na = "NA",col_names = FALSE)
}


#repeat for extra points




