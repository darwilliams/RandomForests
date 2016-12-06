# shplist function

#creates a list of files in chosen folder
#appends the directory to file names, 
#then writes new file list to txt file

# directory should be full folder path with double backslahses throughout and on end, 
# e.g. "D:\\LiDAR_processing\\Brunette\\ndsm\\tiles\\"

shplist <- function(directory){
  # put the parameters into a list
  shplist <- data.frame(list.files(directory, pattern = ".shp$", full.names = TRUE))
  colnames(shplist) <- NULL
  write.csv(shplist, file = paste0(directory,"/shplist.txt"), quote = FALSE, row.names = FALSE)
  shplist <<- read.csv(file = paste0(directory,"/shplist.txt"), header = FALSE,sep = ",")
  shplist_direc <<- paste0(directory,"/shplist.txt")
  # return shplist_direc
}
