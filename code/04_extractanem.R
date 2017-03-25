# For 2017 surveys. Reads in from Excel file directly. 
library(tidyverse)
library(lubridate)
library(stringr)
library(RSQLite)
######################################################
## Surveys and Collections: Match times to locations
######################################################

####### Add lat/long to survey data

divecols <- c("numeric", "numeric", "date", "text", "text", "numeric", "text", "date", "date", "date", "numeric", "date", "date", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "text", "text")

fishcols <- c("text", "numeric", "date", "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text","text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric","numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "text", "numeric", "numeric", "numeric", "numeric","text")

excel_file <- ("data/GPSSurveys2017.xlsx")

surv <- readxl::read_excel(excel_file, sheet = "diveinfo", col_names=TRUE)
names(surv) <- str_to_lower(names(surv))

dat <- readxl::read_excel(excel_file, sheet = "clownfish", col_names = TRUE, na = "", col_types = fishcols)   
names(dat) <- str_to_lower(names(dat))

latlong <-  read_csv("data/untrimmed_tracks_concat_2017.csv") # use untrimmed tracks - trimmed are missing some points at the ends of the dives

rm(divecols, fishcols)

surv1 <- surv %>% 
  select(divenum, date, site, municipality, cover)

# join the surv1 columns to data
dat <- dat %>% 
  left_join(surv1, by = "divenum") 
rm(surv1)
  
# find samples that are lacking an anemone
(lack <- dat %>% 
  filter(is.na(anemspp) & !is.na(spp)))
# if this is zero, 
rm(lack) # if it is not zero, look into what is going on on the data sheet

# remove lines that are not anemones and remove weird dates that excel attaches
dat <- dat %>% 
  filter(!is.na(anemspp)) %>% 
  separate(obstime, into = c("baddate", "obstime2"), sep = " ") %>%
  select(-contains("bad")) 

dat <- rename(dat, obstime = obstime2)

# get the date and time info for each anemone
dat$obstime <- str_c(dat$date, dat$obstime, sep = " ")
dat$obstime <- ymd_hms(dat$obstime)
dat$obstime <- force_tz(dat$obstime, tzone = "Asia/Manila")

# convert to UTC
dat$obstime <- with_tz(dat$obstime, tzone = "UTC")

# split out time components to compare to latlong
dat <- dat %>% 
  mutate(month = month(obstime)) %>% 
  mutate(day = day(obstime)) %>% 
  mutate(hour = hour(obstime)) %>% 
  mutate(min = minute(obstime)) %>% 
  mutate(sec = second(obstime)) %>% 
  mutate(year = year(obstime))

latlong <- latlong %>% 
  mutate(month = month(time)) %>% 
  mutate(day = day(time)) %>% 
  mutate(hour = hour(time)) %>% 
  mutate(min = minute(time)) %>% 
  mutate(sec = second(time))
  
# find matches for times to assign lat long - there are more than one set of seconds (sec.y) that match
dat <- left_join(dat, latlong, by = c("month", "day", "hour", "min"))

# because all of the lat longs for the 4 observations are basically the same, remove sec.y and find distinct values
dat <- dat %>% 
  select(-sec.y) %>%  # remove sec.y column
  distinct(id, .keep_all = T) %>% # keep all unique observance events (need that id col in the excel sheet) %>% 
  rename(sec = sec.x)


# Add the total number of fish (only for dominant spp) & # Add the size of fish (only for dominant spp)
dat$numfish <- NA
dat$sizes <- NA
for (i in 1:nrow(dat)){
  dat$numfish[i] <- sum(c(!is.na(dat$size1[i]), !is.na(dat$size2[i]), !is.na(dat$size3[i]), !is.na(dat$size4[i]), !is.na(dat$size5[i]), !is.na(dat$size6[i]), !is.na(dat$size7[i]), !is.na(dat$size8[i]), !is.na(dat$size9[i]), !is.na(dat$size10[i]), !is.na(dat$size11[i])))
  dat$sizes[i] <- paste(dat$size1[i], dat$size2[i], dat$size3[i], dat$size4[i], dat$size5[i], dat$size6[i], dat$size7[i], dat$size8[i],dat$size9[i],dat$size10[i],dat$size11[i], collapse = ",")
}
# remove NA's from dat$sizes
dat$sizes <- str_replace_all(dat$sizes, "NA", "")

# Sort the data

dat <- dat %>%
  arrange(divenum, obstime)

# Examine the data
dat %>% 
  select(divenum, obstime, anemspp, spp, numfish, sizes, lat, lon)

# Write out anemone data
write_csv(dat, str_c("data/GPSSurvey_anemlatlon", dat$year[1], Sys.Date(), ".csv", sep = ""))

# send data to the local db
# open connection to db
local <- dbConnect(SQLite(), "data/local_leyte.sqlite3")

# pull dive table from db
exist <- dbReadTable(local, "diveinfo")
# first time: exist <- data.frame()
# have to do the math to convert dates to dates, right now in seconds I think

# join the 2 tables and eliminate duplicate rows
surv <- bind_rows(surv, exist)
surv <- distinct(surv) # went from 58 to 58

# Add new dives to db *** note if you want to overwrite or append *****
dbWriteTable(local, "diveinfo", surv, overwrite = F, append = T)

# prep the anem data
anem <- dat %>% 
  select(id, divenum, obstime, collector, gps, depth_m, depth_ft, anemspp, anemdia, anemid, oldanemid, anemsampleid, spp, numfish, notes)
  
  
anem$anem_table_id <- 1:nrow(anem)



dbDisconnect(local)
rm(local, surv, exist)
	

#############################################################################






	
	#prep anem data 
	anem <- data[ , c("id", "DiveNum", "ObsTime", "Collector", "GPS", "Depth_m", "Depth_ft", "AnemSpp", "AnemDia", "AnemID", "oldAnemID", "AnemSampleID", "Spp", "NumFish", "Notes")]
	anem$anem_table_id <- 1:nrow(anem)
	
	# pull anem tbl from db
	exist <- dbReadTable(local, "anemones")
	
	# join the 2 tables and eliminate duplicate rows
	anem <- rbind(anem, exist)
	anem <- distinct(anem)
	
	# Add new info to db *** note if you want to overwrite or append *****
	dbWriteTable(local, "anemones", anem, overwrite = T, append = F)
	
	# Write out for QGIS (has column headers)
	data$notes <- ""
	for(i in 1:nrow(data)){
		if(!is.na(data$Spp[i]) & data$Spp[i] != "") data$notes[i] <- paste(data$AnemSpp[i], " ",data$AnemID[i]," w/", data$NumFish[i], " ", data$Spp[i])
		else data$notes[i] <- as.character(paste(data$AnemSpp[i], " ", data$AnemID[i]))
	}
	out <- data[,c("lat", "lon", "notes", "Date", "Name", "Municipality")]

	write.table(out, file="../../output/GPSSurvey_anemlatlong_2017_forQGIS.csv", col.names=TRUE, sep=",", row.names=FALSE, quote=TRUE)

	
