# For 2017 surveys. Reads in from Excel file directly. 
library(anytime)
######################################################
## Surveys and Collections: Match times to locations
######################################################

####### Add lat/long to survey data

diveinfocoltypes <- c("numeric", "numeric", "date", "text", "text", "numeric", "text", "date", "date", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "text", "text")

# clownfishcoltypes <- c("numeric", "date", "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text")

excel_file <- ("../../GPSSurveys2017.xlsx")

surv <- readxl::read_excel(excel_file, sheet = "DiveInfo", col_names=TRUE, col_types = diveinfocoltypes)

data <- readxl::read_excel(excel_file, sheet = "Clownfish", col_names = TRUE, na = "")   

latlong <-  read.csv("../../output/trimmed_tracks_concat_2017.csv", row.names=1)

names <- names(data)
data$NumFish <- NA # total number of fish (dominant species)
data$Sizes <- NA # concatenated sizes of fish (dominant species)
data$lat <- NA
data$lon <- NA

# add survey name
data <- merge(data, surv[,c("DiveNum", "Date", "Name", "Municipality", "Cover")])

# remove lines that weren't samples
i <- is.na(data$AnemSpp) & !is.na(data$Spp)
if(sum(i)>0){
		stop("lacking anemone for a sample")
		data[i,]
	}
i <- is.na(data$AnemSpp)
sum(i)
data <- data[!i,]
	
# strip out Excel default date from ObsTime, Start and EndTime
	data$ObsTime <- gsub("1899-12-30 ", "", data$ObsTime)
	surv$StartTime <- gsub("1899-12-30 ", "", surv$StartTime)
	surv$EndTime <- gsub("1899-12-30 ", "", surv$EndTime)
	
	# process data for each anemone
	len <- nrow(data)
	options(warn=1) # print warnings as they occur
	for(i in 1:len){
	  #Get date and time information for the anemone
		survey<-data$DiveNum[i]
		survindex<-which(surv$DiveNum == survey)
		date<-as.character(surv$Date[survindex])
		datesplit<-strsplit(date,"-", fixed=T)[[1]]
		month<-as.numeric(datesplit[2])
		day<-as.numeric(datesplit[3])
		time<-as.character(data$ObsTime[i])
		timesplit<-strsplit(time, ":", fixed=T)[[1]]
		hour<-as.numeric(timesplit[1])
		min<-as.numeric(timesplit[2])
		sec<-as.numeric(timesplit[3])
		
		# Convert time to GMT
		hour<-hour - 8
		if(hour <0){
			day<-day-1
			hour<-hour + 24
		}
		
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

	
