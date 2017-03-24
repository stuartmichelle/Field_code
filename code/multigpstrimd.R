#For years with multiple GPS units
multigpstrimd <- function(){
  # get all lat long data from a list of files
  source("../readGPXGarmin.R")
  files <- list.files(path = "../../gpx_trimmed", pattern = "*Track.*gpx")
  len <- length(files)
  data <- vector("list", len) # make an empty list the length of the number of files in dir
  for(i in 1:len){
    infile <- readGPXGarmin(paste("../../gpx_trimmed/", files[i], sep = ''))
    data[i] <- list(infile$data)
  }

 latlong <- plyr::ldply(data, data.frame)
  
  # Remove duplicates (duplicated returns TRUE if the value has been found previously)
 latlong <- dplyr::distinct(latlong)
  
  # Sort the records by time
  permut <- order(latlong$year, latlong$month, latlong$day, latlong$hour, latlong$min, latlong$sec)
  latlong <<- latlong[permut,]
  
  return(latlong)
}
