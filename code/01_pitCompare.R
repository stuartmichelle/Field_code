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

# anti_join, what is in y that is not already in x
# what is in the new pit that is not already in the database
new <- anti_join(exist, pit, copy = T)
  
# Add new pit scans to db *** note if you want to overwrite or append *****
dbWriteTable(write_local, "pitscan", as.data.frame(new), append = T) # still need RSQLite to write
# to  create the first table 
# dbWriteTable(write_local, "pitscan", as.data.frame(pit))

### PART 2 - FORMAT THE DATA FOR USE LATER ####
  
  
# combine the fields into the full string of numbers used to identify a fish ------------------------
# create a list of scans that are already in the database
dbscan <- as_tibble(exist) %>% 
  unite(scan, city, tagid, sep = "")


# Import excel survey data --------------------------------------------
  # This would be a good place to pull data from a database instead of loading the entire excel sheet (it pulls in blank lines 1,048,575 obs)
excelfile <- "data/GPSSurveys2017.xlsx"
cols <- c("text", "numeric", "date", "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text","text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric","numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "text", "numeric", "numeric", "numeric", "numeric","text")
  
excel <- readxl::read_excel(excelfile, sheet = "clownfish", col_names=TRUE, col_types = cols)
  

# Find the PIT tags in the excel data -----------------------------------

# select all columns which are named with the word tag
xltags <- excel %>% select(contains("tag"), divenum)

# remove NAs and combine into one column of values
xltags <- xltags %>% 
  gather(tag, number, contains("tag"), na.rm = T) %>% # name the columns tag and number, put all the column names that contain tag into the tag column and their values into the number column
  select(-divenum) # remove column

# List any scanned tags that are not in the excel data ------------------

anti_join(xltags, dbscan, by = c("number" = "scan"))  # what is in y that is not in x; what is in the database scans that is not in excel

anti_join(dbscan, xltags, by = c("scan" = "number"))  # what is in y that is not in x; what is in excel that is not in the database
