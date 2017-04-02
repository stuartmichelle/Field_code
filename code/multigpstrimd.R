#For years with multiple GPS units
multigpstrimd <- function(){
  # get all lat long data from a list of files
  source("code/readGPXGarmin.R")
  library(dplyr)
  library(lubridate)
  # get a list of the trimmed gpx files ####
  files <- list.files(path = "data/gpx_trimmed", pattern = "*Track.*gpx")
  
  # set up the dataframe with the first set of data ####
  infile <- readGPXGarmin(str_c("data/gpx_trimmed/", files[1], sep = ""))
  data <- infile$data
  
  # add to the dataframe all of the data from the other files ####
  for(i in 2:length(files)){
    infile <- readGPXGarmin(str_c("data/gpx_trimmed/", files[i], sep = ""))
    header <- infile$header
    data <- bind_rows(data, infile$data)
  }
  
  ### WAIT ####
  # get the warnings In rbind_all(x, .id) : Unequal factor levels: coercing to character # that's ok
  
  # Remove duplicates (duplicated returns TRUE if the value has been found previously) ####
# find unique values 
  data <- data %>% 
    distinct() %>%  # find unique values, went from 24704 to 24704 rows
    arrange(ymd_hms(time))
  
  return(data)
}
