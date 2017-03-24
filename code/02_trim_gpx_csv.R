################################################################
## Trim GPX files only to survey times
## (for plotting in Google Earth)
################################################################
library(tidyverse) # for dplyr
library(lubridate) # for changing time zones
library(stringr) # for combining dates and times into dttm
source("code/readGPXGarmin.R")
source("code/writeGPXGarmin.R")

excelfile <- "data/GPSSurveys2017.xlsx"
surv <- readxl::read_excel(excelfile, "diveinfo", col_names = T, col_types = NULL, na = "")

# strip out extra rows that were added by excel and reduce columns####
surv <- surv %>% 
  filter(!is.na(DiveNum)) %>% 
  select(DiveNum, Date, StartTime, EndTime, PauseStart, PauseEnd)


# eliminate weird dates added by excel ####
surv <- surv %>% 
  separate(StartTime, into = c("baddate", "start"), sep = " ") %>% 
  separate(EndTime, into = c("baddate2", "end"), sep = " ") %>%
  separate(PauseStart, into = c("baddate3", "paust"), sep = " ") %>% 
  separate(PauseEnd, into = c("baddate4", "pausend"), sep = " ") %>% 
  select(-contains("bad")) 

# combine date and time to form a dttm column ####
  # set time zone to PHT, Asia/Manila
surv$start <- str_c(surv$Date, surv$start, sep = " ")  
surv$start <- ymd_hms(surv$start)
surv$start <- force_tz(surv$start, tzone = "Asia/Manila")
surv$end <- str_c(surv$Date, surv$end, sep = " ")  
surv$end <- ymd_hms(surv$end)
surv$end <- force_tz(surv$end, tzone = "Asia/Manila")
surv$paust <- str_c(surv$Date, surv$paust, sep = " ")  
surv$paust <- ymd_hms(surv$paust)
surv$paust <- force_tz(surv$paust, tzone = "Asia/Manila")
surv$pausend <- str_c(surv$Date, surv$pausend, sep = " ")  
surv$pausend <- ymd_hms(surv$pausend)
surv$pausend <- force_tz(surv$pausend, tzone = "Asia/Manila")

# change time zone to UTC ####
surv$start <- with_tz(surv$start, tzone = "UTC")
surv$end <- with_tz(surv$end, tzone = "UTC")
surv$paust <- with_tz(surv$paust, tzone = "UTC")
surv$pausend <- with_tz(surv$pausend, tzone = "UTC")

# Read in each GPX file ####
files  <-  list.files(path = "data/gpx/", pattern="*Track.*gpx")
len = length(files)

for(i in 1:len){ # for each file
	infile <- readGPXGarmin(paste("data/gpx/", files[i], sep="")) 
	header <- infile$header
	data <- infile$data
	data$time <- ymd_hms(data$time)
	data <- arrange(data, time)
	# start time for this GPX track
	instarttime <-  data$time[1]
	# end time for this GPX track
	inendtime = data$time[nrow(data)]
	# change elevation to zero
	data$elev <- 0
	
	# which survey started after the gpx and ended before the gpx ####
	inds <- surv %>% 
	  filter(start >= instarttime & end <= inendtime)
	
	# if none of the surveys fit ####
	if(nrow(inds) == 0){
		print(str_c("File", files[i], "does not cover a complete survey"))
	  
	  # find a survey that at least starts or ends within this GPX track
	  # which survey ended before the gpx and ended after the gpx started or started before the gpx ended and started after the gpx started
		inds <- surv %>% 
		  filter((end <= inendtime & end >= instarttime)| (start <= inendtime & start >= instarttime))
		if(length(inds) == 0){
			print(str_c("EVEN WORSE:", files[i], "does not cover even PART of a survey"))
		}
	}
	if(nrow(inds) > 0){
		for(j in inds){ # step through each survey that fits within this track (one or more)
			# output all: not just if this was a dive for collecting APCL
		  # if no pause
		  if(is.na(surv$paust[j])){ 
		    # find the GPX points that fit within the survey
				k <- data %>% 
				  filter(time >= surv$start[j] & time <= surv$end[j]) 
				outfile <- list(header, k)
				
				writeGPX(filename = paste("data/gpx_trimmed/", surv$DiveNum[j], "_", files[i], sep=""), outfile = outfile)
			}
			if(!is.na(surv$paust[j])){ # account for a pause if need be
				k1 = which(intimes >= surv$start[j] & intimes <= surv$paust[j]) # find the GPX points during survey and before pause
				k2 = which(intimes >= surv$pausend[j] & intimes <= [j]) # find the GPX points after pause and during survey
				outfile1 = list(header = infile$header, data = infile$data[k1,])
				outfile2 = list(header = infile$header, data = infile$data[k2,])

				nm = unlist(strsplit(files[i], '.gpx'))
				writeGPX(filename = paste('data/gpx_trimmed/', surv$DiveNum[j], '_', nm, '_1.gpx', sep=''), outfile = outfile1) # write as two tracks
				writeGPX(filename = paste('data/gpx_trimmed/', surv$DiveNum[j], '_', nm, '_2.gpx', sep=''), outfile = outfile2)
			}
		}
	}
}
