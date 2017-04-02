source("code/multigpstrimd.R")
source("code/multigpsuntrimd.R")
source("code/writeleyte.R")
source("code/readGPXGarmin.R")
library(readr)


# concatenate all trimmed gpx files into one data frame
trimmed <- multigpstrimd()

### WAIT ###

write_csv(trimmed, path = "data/trimmed_tracks_concat_2017.csv")

# Repeat for untrimmed tracks ---------------------------------------------

untrimmed <- multigpsuntrimd()

write_csv(untrimmed, path = "data/untrimmed_tracks_concat_2017.csv")
rm(untrimmed)

# send data to the local db ------------------------------------------------
library(tidyverse)
library(RSQLite)

# open connection to db
local <- dbConnect(SQLite(), "data/local_leyte.sqlite3")

# pull GPX table from db
exist <- dbReadTable(local, "GPX")
# first time: exist <- data.frame()

# join the 2 tables and eliminate duplicate rows
trimmed <- bind_rows(trimmed, exist)
trimmed <- distinct(trimmed) # went from 24704 to 24704

# Add new pit scans to db *** note if you want to overwrite or append *****
dbWriteTable(local, "GPX", trimmed, overwrite = T, append = F)

dbDisconnect(local)
rm(local, trimmed, exist)


# # Send data to amphiprion database ### ONLY DO THIS ONCE !!! ###
# leyte <- writeleyte()
# 
# dbWriteTable(leyte,"GPX",data.frame(latlong), row.names = FALSE, append = TRUE)
# 
# dbDisconnect(leyte)
# rm(leyte)
#   
# finish <- paste("~/Documents/Philippines/Surveys_", year, "/code/QGIS_Phils/", sep = "")
# setwd(finish)
