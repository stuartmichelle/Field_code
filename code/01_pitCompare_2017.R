# a script to compare new pit scans with newly entered pit data

# set up workspace ####
library(tidyverse)
library(RSQLite) # still need RSQLite to write
# setOldClass(c("tbl_df", "data.frame")) 
# open connection to db
local <- src_sqlite(path = "data/local_leyte.sqlite3", create = F)
write_local <- dbConnect(SQLite(), "data/local_leyte.sqlite3") # still need RSQLite to write

# PART 1 - add new PIT data to database # still need RSQLite to write  ####
  infile <- "data/PIT.txt"  # during field season
  # infile <- paste(year,"PIT.txt",sep='') # after field season
  
### have to remove any blank lines at the end of the text file before doing this###
pit <- read_csv(infile, 
    col_names = c("city", "tagid", "date", "time"), 
    col_types = cols(
      city = col_character(),
      tagid = col_character(),
      date = col_character(), # have to specify as string or it will write to db as integer, can't use col_date(%y-%m-%d)
      time = col_character() # col_time("%H:%M:%S") not working as of 3/22/2017 MRS on Lightning
    )
  )

# pull pitscan table from db
exist <- local %>% 
  tbl("pitscan") 

# what is in the new pit that is not already in the database
new <- anti_join(exist, pit, copy = T)
  
# Add new pit scans to db *** note if you want to overwrite or append *****
dbWriteTable(write_local, "pitscan", as.data.frame(new), append = T) # still need RSQLite to write
# to  create the first table 
# dbWriteTable(write_local, "pitscan", as.data.frame(pit))

### PART 2 - FORMAT THE DATA FOR USE LATER ####
  
  
# Separate out the 6 digit tag numbers from the data ------------------------
pit$scan <- paste(pit$city, pit$tagid, sep = "")

  # Import excel survey data --------------------------------------------
  # This would be a good place to pull data from a database instead of loading the entire excel sheet (it pulls in blank lines 1,048,575 obs)
  library(readxl)
  excelfile <- "../../GPSSurveys2017.xlsx"
  excel <- read_excel(excelfile, sheet = "Clownfish", col_names=TRUE)
  # fileList <- sort(list.files(path='output', pattern = "GPSSurvey.*"), decreasing=TRUE) # lists all extract anem output
  # excelfile <- fileList[1] # pulls out the most recent extractanem output
  # excel <- read.csv(paste("output/",excelfile, sep='')) # using this instead of read_excel because we need the date

  # Find the PIT tags in the excel data -----------------------------------
  # xcel <- excel[,c("TagID1","TagID2","TagID3", "TagID4", "Date", "ObsTime")]
  xcel <- excel[,c("TagID1","TagID2","TagID3", "TagID4", "TagID5", "TagID6", "TagID7", "DiveNum")] # including divenum so we have one row that will always have a value

  # Remove NA from the excel data -----------------------------------------
  xcel <- xcel[!is.na(xcel$DiveNum), ]

# make a list of existing tagids for all of the columns in the datasheet
  xcl <- c(xcel$TagID1[!is.na(xcel$TagID1)], xcel$TagID2[!is.na(xcel$TagID2)], xcel$TagID3[!is.na(xcel$TagID3)], xcel$TagID4[!is.na(xcel$TagID4)], xcel$TagID5[!is.na(xcel$TagID5)], xcel$TagID6[!is.na(xcel$TagID6)], xcel$TagID7[!is.na(xcel$TagID7)])
 
# List any scanned tags that are not in the excel data ------------------
  print(setdiff(as.character(pit$scan), as.character(xcl))) # searches for values in scan that are not in xcl
  # should return numeric(0)
  # scans <- setdiff(as.numeric(pit$scan), as.numeric(xcl))
  
  # List any excel tags that are not in the scanned data ------------------
  print(setdiff(xcl, pit$scan)) #searches for values in xcl that are not in scan
  # should return list()
# mistype <- setdiff(xcl, pit$scan)
  
  
  
