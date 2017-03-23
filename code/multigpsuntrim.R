#For years with multiple GPS units
multigpsuntrimd <- function(){
  files <- list.files(path = '../../gpx', pattern = "*Track.*gpx")
  len <- length(files)
  
  data <- vector("list", len)
  for(i in 1:len){
    infile <- FieldworkCode::readGPXGarmin(paste('../../gpx/', files[i], sep = ''))
    data[i] <- list(infile$data)
  }
  
  latlong <- data.frame(data[[1]])
  for(i in 2:len){
    new <- data.frame(data[[i]])
    latlong <- rbind(latlong, new)
  }
  dim(latlong)
  
  # Remove duplicates
  i <- duplicated(latlong)
  sum(i)
  latlong <- latlong[!i,]
  dim(latlong)
  
  # Sort the records by time
  permut <- order(latlong$year, latlong$month, latlong$day, latlong$hour, latlong$min, latlong$sec)
  latlong <- latlong[permut,]
  return(latlong)
}
