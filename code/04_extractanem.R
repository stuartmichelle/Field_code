# For 2017 surveys. Reads in from Excel file directly. 
# library(anytime)
library(tidyverse)
library(lubridate)
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

latlong <-  read_csv("data/trimmed_tracks_concat_2017.csv")

surv1 <- surv %>% 
  select(divenum, date, site, municipality, cover)

# join the surv1 columns to data
dat <- dat %>% 
  left_join(surv1, by = "divenum") 
  
# find samples that are lacking an anemone
lack <- dat %>% 
  filter(is.na(anemspp) & !is.na(spp)) 
# if this is zero, 
rm(lack) # if it is not zero, look into what is going on on the data sheet

# remove lines that are not anemones
dat <- dat %>% 
  filter(!is.na(anemspp))

# remove the weird dates that excel attaches to times
dat <- dat %>% 
  separate(obstime, into = c("baddate", "obstime"), sep = " ") %>%
  select(-contains("bad")) 

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
  mutate(sec = second(obstime))

latlong <- latlong %>% 
  mutate(month = month(time)) %>% 
  mutate(day = day(time)) %>% 
  mutate(hour = hour(time)) %>% 
  mutate(min = minute(time)) %>% 
  mutate(sec = second(time))
  
# find matches for times to assign lat long
lats <- semi_join(dat, latlong, by = c("month", "day", "hour", "min"))

# what didn't match # if this isn't zero, look into why
miss <- anti_join(dat, latlong, by = c("month", "day", "hour", "min"))

# currently 19, make sure this is real data and not a code error






names <- names(data)
data$NumFish <- NA # total number of fish (dominant species)
data$Sizes <- NA # concatenated sizes of fish (dominant species)
data$lat <- NA
data$lon <- NA


	

		
		# Find the location records that match the date/time stamp (to nearest second)
		latlongindex<-which(latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
		i2<-which.min(abs(latlong$sec[latlongindex] - sec))
		
		# Calculate the lat/long for this time
		if(length(i2)>0){
			data$lat[i]<-latlong$lat[latlongindex][i2]
			data$lon[i] <- latlong$long[latlongindex][i2]
		}
		
		
		# Add the total number of fish (only for dominant spp)
    data$NumFish[i] <- sum(c(!is.na(data$Size1[i]), !is.na(data$Size2[i]), !is.na(data$Size3[i]), !is.na(data$Size4[i]), !is.na(data$Size5[i]), !is.na(data$Size6[i]), !is.na(data$Size7[i]), !is.na(data$Size8[i]), !is.na(data$Size9[i]), !is.na(data$Size10[i]), !is.na(data$Size11[i])))
    
		# Add the size of fish (only for dominant spp)
		temp <- c(data$Size1[i], data$Size2[i], data$Size3[i], data$Size4[i], data$Size5[i], data$Size6[i], data$Size7[i], data$Size8[i],data$Size9[i],data$Size10[i],data$Size11[i])
		temp <- temp[!is.na(temp)]
		temp <- paste(temp, collapse=",")
		data$Sizes[i] <- temp
	}
	
	
	# Sort the data
	permut <- order(data$DiveNum, data$ObsTime)
	data <- data[permut,]
	row.names(data) <- 1:nrow(data)
	
	# Examine the head and tail of the data
	head(data[,c("DiveNum", "ObsTime", "AnemSpp", "Spp", "NumFish", "Sizes", "lat", "lon")])
	tail(data[,c("DiveNum", "ObsTime", "AnemSpp", "Spp", "NumFish", "Sizes", "lat", "lon")])
	
	
	# Write out anemone data

	write.csv(data, file= paste("../../output/GPSSurvey_anemlatlong_2017.csv", sep = ""))
	
# send data to the local db
	suppressMessages(library(dplyr))
	library(RSQLite)
	
	# open connection to db
	local <- dbConnect(SQLite(), "../../local_leyte.sqlite3")
	
	# pull dive table from db
	exist <- dbReadTable(local, "diveinfo")
	exist$Date <- anydate(exist$Date)
	
	# join the 2 tables and eliminate duplicate rows
	dive <- as.data.frame(surv)
	# add a dive_table_id column
	dive$dive_table_id <- 1:nrow(dive)
	dive <- rbind(dive, exist)
	dive <- distinct(dive)

	# Add new info to db *** note if you want to overwrite or append *****
	dbWriteTable(local, "diveinfo", dive, overwrite = T, append = F)
	
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

	
