source("../../code/multigpstrim.R")
source("../../code/multigpsuntrim.R")
source("../../code/writeleyte.R")

trimmed <- multigpstrimd()

write.csv(trimmed, file = "../../output/trimmed_tracks_concat_2017.csv")

# Repeat for untrimmed tracks ---------------------------------------------

untrimmed <- multigpsuntrimd()

# send data to the local db
suppressMessages(library(dplyr))
library(RSQLite)

# open connection to db
local <- dbConnect(SQLite(), "../../local_leyte.sqlite3")

# pull GPX table from db
exist <- dbReadTable(local, "GPX")

# join the 2 tables and eliminate duplicate rows
trimmed <- rbind(trimmed, exist)
trimmed <- distinct(trimmed)

# Add new pit scans to db *** note if you want to overwrite or append *****
dbWriteTable(local, "GPX", trimmed, overwrite = T, append = F)


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
