################################################################
## Trim GPX files only to survey times
## (for plotting in Google Earth)
################################################################

excelfile <- "~/Downloads/GPSSurveys2017.xlsx"
excelfile <- "../../GPSSurveys2017.xlsx"
surv <- readxl::read_excel(excelfile, "DiveInfo", col_names = T, col_types = NULL, na = "")

# strip out extra rows that were added by excel
surv <- surv[!is.na(surv$DiveNum), ]

# read in survey times (start, end, pause start and pause end)

dates <- surv %>% select(Date) %>% separate(Date, into = c("year", "startmonth", "startday"))
startmonth <- dates$startmonth
endmonth <- startmonth
startday <- dates$startday
endday <- startday
year <- dates$year

# strip out Excel default date from StartTime & EndTime
surv$StartTime <- gsub('1899-12-30 ', '', surv$StartTime)
surv$EndTime <- gsub('1899-12-30 ', '', surv$EndTime)
surv$PauseStart <- gsub('1899-12-30 ', '', surv$PauseStart)
surv$PauseEnd <- gsub('1899-12-30 ', '', surv$PauseEnd)


times <- surv %>% select(StartTime, EndTime) %>% separate(StartTime, into = c("starthour", "startmin"), sep = ":") %>% separate(EndTime, into = c("endhour", "endmin"), sep = ":")
starthour <- times$starthour 
startmin <- times$startmin
endhour = times$endhour
endmin = times$endmin

# make all of the NA pause fields 00:00:00 so that they split into 3 values
surv$PauseStart[is.na(surv$PauseStart)] <- "00:00:00"
surv$PauseEnd[is.na(surv$PauseEnd)] <- "00:00:00"

pausestarttime = as.character(surv$PauseStart)
	pausestarttime[pausestarttime == ''] = '-99:-99' # to signify missing data
pausestarttimesplit = unlist(strsplit(pausestarttime, ":", fixed=T))
pausestarthour = as.numeric(pausestarttimesplit[seq(1,length(pausestarttimesplit), by=3)])
	pausestarthour[pausestarthour == -99] = NA
pausestartmin = as.numeric(pausestarttimesplit[seq(2,length(pausestarttimesplit), by=3)])
	pausestartmin[pausestartmin == -99] = NA
pauseendtime = as.character(surv$PauseEnd)
	pauseendtime[pauseendtime == ''] = '-99:-99' # to signify missing data
pauseendtimesplit = unlist(strsplit(pauseendtime, ":", fixed=T))
pauseendhour = as.numeric(pauseendtimesplit[seq(1,length(pauseendtimesplit), by=3)])
	pauseendhour[pauseendhour == -99] = NA
pauseendmin = as.numeric(pauseendtimesplit[seq(2,length(pauseendtimesplit), by=3)])
	pauseendmin[pauseendmin == -99] = NA


	
# Convert survey time to GMT
starthour = starthour - 8
i = starthour < 0; sum(i) # update if crossed midnight
startday[i] = startday[i] - 1
starthour[i] = starthour[i] + 24
i = startday < 1; sum(i) # make sure no days moved to previous month

endhour = endhour - 8
i = endhour < 0; sum(i)
endday[i] = endday[i] - 1
endhour[i] = endhour[i] + 24
i = endday < 1; sum(i) # make sure no days moved to previous month

pausestarthour[which(pausestarthour == 0)] <- NA
pausestarthour <- pausestarthour - 8
i = pausestarthour < 0 & !is.na(pausestarthour); sum(i) # update if crossed midnight
pausestarthour[i] = pausestarthour[i] + 24

pauseendhour[which(pauseendhour == 0)] <- NA
pauseendhour = pauseendhour - 8
i = pauseendhour < 0 & !is.na(pauseendhour); sum(i)
pauseendhour[i] = pauseendhour[i] + 24

# can do this better with tidyverse, see r4ds
starttimePX = strptime(paste(startmonth, startday, year, starthour, startmin), tz='GMT', format='%m %d %Y %H %M') # start times in POSIXlt format for each survey
endtimePX = strptime(paste(endmonth, endday, year, endhour, endmin), tz='GMT', format='%m %d %Y %H %M') # end times in POSIXlt format for each survey
pausestarttimePX = strptime(paste(startmonth, startday, year, pausestarthour, pausestartmin), tz='GMT', format='%m %d %Y %H %M') # start times in POSIXlt format for each survey
pauseendtimePX = strptime(paste(endmonth, endday, year, pauseendhour, pauseendmin), tz='GMT', format='%m %d %Y %H %M') # end times in POSIXlt format for each survey

# Read in each GPX file
files = list.files(path = "../../gpx/", pattern="*Track.*gpx")
len = length(files)

for(i in 1:len){
	infile <- FieldworkCode::readGPXGarmin(paste("../../gpx/", files[i], sep=''))
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
				FieldworkCode::writeGPX(filename = paste('../../gpx_trimmed/', surv$DiveNum[j], '_', files[i], sep=''), outfile = outfile)
			}
			if(!is.na(pausestarttimePX[j])){ # account for a pause if need be
				k1 = which(intimes >= starttimePX[j] & intimes <= pausestarttimePX[j]) # find the GPX points during survey and before pause
				k2 = which(intimes >= pauseendtimePX[j] & intimes <= endtimePX[j]) # find the GPX points after pause and during survey
				outfile1 = list(header = infile$header, data = infile$data[k1,])
				outfile2 = list(header = infile$header, data = infile$data[k2,])
				outfile1$data$elev = 0 # set elevation to 0
				outfile2$data$elev = 0 # set elevation to 0

				nm = unlist(strsplit(files[i], '.gpx'))
				FieldworkCode::writeGPX(filename = paste('../../gpx_trimmed/', surv$DiveNum[j], '_', nm, '_1.gpx', sep=''), outfile = outfile1) # write as two tracks
				FieldworkCode::writeGPX(filename = paste('../../gpx_trimmed/', surv$DiveNum[j], '_', nm, '_2.gpx', sep=''), outfile = outfile2)
			}
		}
	}
}
