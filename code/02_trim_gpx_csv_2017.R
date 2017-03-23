################################################################
## Trim GPX files only to survey times
## (for plotting in Google Earth)
################################################################
library(tidyverse)
library(lubridate)
source("code/readGPXGarmin.R")
source("code/writeGPXGarmin.R")

excelfile <- "data/GPSSurveys2017.xlsx"
surv <- readxl::read_excel(excelfile, "diveinfo", col_names = T, col_types = NULL, na = "")

# strip out extra rows that were added by excel
surv <- surv %>% 
  filter(!is.na(DiveNum)) %>% 
  select(DiveNum, Date, StartTime, EndTime, PauseStart, PauseEnd)


# eliminate weird dates added by excel and split year, month, day into columns, create endday for the purpose of converting time zone later
surv <- surv %>% 
  separate(Date, into = c("year", "startmonth", "startday"))  %>% 
  separate(StartTime, into = c("baddate", "starttimep"), sep = " ") %>% 
  separate(EndTime, into = c("baddate2", "endtimep"), sep = " ") %>%
  separate(PauseStart, into = c("baddate3", "paustime"), sep = " ") %>% 
  separate(PauseEnd, into = c("baddate4", "pausentime"), sep = " ") %>% 
  select(-contains("bad")) %>% 
  mutate(endday = as.numeric(startday)) %>% 
  mutate(endmonth = as.numeric(startmonth))

# split out times into hours, mins, secs for the purpose of changing the timezone
surv <- surv %>%
  separate(starttimep, into = c("starthour", "startmin", "startsec")) %>% 
  separate(endtimep, into = c("endhour", "endmin", "endsec")) %>% 
  separate(paustime, into = c("pausthour", "paustmin", "paustsec")) %>% 
  separate(pausentime, into = c("pausenhour", "pausenmin", "pausensec")) 

# convert to GMT and adjust dates
surv <- surv %>% 
  # convert from phil to gmt
  mutate(starthour = as.numeric(starthour) - 8) %>% 
  mutate(endhour = as.numeric(endhour) - 8) %>%
  mutate(pausthour = as.numeric(pausthour) - 8) %>% 
  mutate(pausenhour = as.numeric(pausenhour) - 8) %>% 
  # if the time crossed midnight, change the day
  mutate(startday = ifelse(starthour < 0, as.numeric(startday) - 1, startday)) %>%
  mutate(endday = ifelse(endhour < 0, as.numeric(endday) - 1, endday)) %>% 
  # if the time crossed midnight change the hour
  mutate(starthour = ifelse(starthour < 0, starthour + 24, starthour)) %>% 
  mutate(endhour = ifelse(endhour < 0, endhour + 24, endhour)) %>% 
  mutate(pausthour = ifelse(pausthour < 0, pausthour + 24, pausthour)) %>% 
  mutate(pausenhour = ifelse(pausenhour < 0, pausenhour + 24, pausenhour)) 
  
# TEST
# count(surv, as.numeric(startday) < 1) # make sure no days moved to the previous 
# count(surv, as.numeric(endday) < 1) # make sure no days moved to the previous month
# should return all false - FALSE    58

# this is the old code until I learn more about the tidyverse
starttimePX = strptime(paste(surv$startmonth, surv$startday, surv$year, surv$starthour, surv$startmin), tz='GMT', format='%m %d %Y %H %M') # start times in POSIXlt format for each survey

endtimePX = strptime(paste(surv$endmonth, surv$endday, surv$year, surv$endhour, surv$endmin), tz='GMT', format='%m %d %Y %H %M') # end times in POSIXlt format for each survey
pausestarttimePX = strptime(paste(surv$startmonth, surv$startday, surv$year, surv$pausthour, surv$paustmin), tz='GMT', format='%m %d %Y %H %M') # start times in POSIXlt format for each survey
pauseendtimePX = strptime(paste(surv$endmonth, surv$endday, surv$year, surv$pausenhour, surv$pausenmin), tz='GMT', format='%m %d %Y %H %M') # end times in POSIXlt format for each survey


# Read in each GPX file
files  <-  list.files(path = "data/gpx/", pattern="*Track.*gpx")
len = length(files)

for(i in 1:len){ # for each file
	infile <- readGPXGarmin(paste("data/gpx/", files[i], sep=""))
	intimes <- strptime(as.character(infile$data$time), tz = 'GMT', format = '%Y-%m-%dT%H:%M:%SZ') # start time of the GPS track in POSIXlt format
	instarttime = intimes[1] # start time for this GPX track
	inendtime = intimes[length(intimes)] # end time for this GPX track
	inds = which(starttimePX >= instarttime & endtimePX <= inendtime) # find which survey fits within this GPX track's date & time
	if(length(inds) == 0){
		print(paste('File', files[i], 'does not cover a complete survey'))
		inds = which((endtimePX <= inendtime & endtimePX > instarttime) | (starttimePX < inendtime & starttimePX >= instarttime)) # find a survey that at least starts or ends within this GPX track
		if(length(inds) == 0){
			print(paste('EVEN WORSE:', files[i], 'does not cover even PART of a survey'))
		}
	}
	if(length(inds) > 0){
		for(j in inds){ # step through each survey that fits within this track (one or more)
			# output all: not just if this was a dive for collecting APCL
			if(is.na(pausestarttimePX[j])){ # if no pause
				k = which(intimes >= starttimePX[j] & intimes <= endtimePX[j]) # find the GPX points that fit within the survey
				outfile = list(header = infile$header, data = infile$data[k,])
				outfile$data$elev = 0 # set elevation to 0
				writeGPX(filename = paste('data/gpx_trimmed/', surv$DiveNum[j], '_', files[i], sep=''), outfile = outfile)
			}
			if(!is.na(pausestarttimePX[j])){ # account for a pause if need be
				k1 = which(intimes >= starttimePX[j] & intimes <= pausestarttimePX[j]) # find the GPX points during survey and before pause
				k2 = which(intimes >= pauseendtimePX[j] & intimes <= endtimePX[j]) # find the GPX points after pause and during survey
				outfile1 = list(header = infile$header, data = infile$data[k1,])
				outfile2 = list(header = infile$header, data = infile$data[k2,])
				outfile1$data$elev = 0 # set elevation to 0
				outfile2$data$elev = 0 # set elevation to 0

				nm = unlist(strsplit(files[i], '.gpx'))
				writeGPX(filename = paste('data/gpx_trimmed/', surv$DiveNum[j], '_', nm, '_1.gpx', sep=''), outfile = outfile1) # write as two tracks
				writeGPX(filename = paste('data/gpx_trimmed/', surv$DiveNum[j], '_', nm, '_2.gpx', sep=''), outfile = outfile2)
			}
		}
	}
}
