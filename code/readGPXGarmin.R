#' Read GPX Garmin
#'
#' This function allows you to read gpx files generated on the Garmin GPS unit.
#' @param filename What gpx file would you like to read?
#' @keywords gpx
#' @export
#' @examples
#' readGPXGarmin(filename)

# Read a .gpx file and return a dataframe of time, lat, long, and elevation, as well as the header data
# Treats the file as only one trkseg (and may not work on files with multiple trksegs)

readGPXGarmin <- function(filename){
    infile <- readLines(filename, warn=FALSE)
    lines <- strsplit(infile, split="<trkpt", fixed=TRUE)[[1]] # split into each point
    header <- lines[1] # the file header, through <trkseg>

    i <- grep('lat', lines)
    lines <- lines[i] # trim only to lines that have latitude (drops the header)
    len <- length(lines)

    # from example under grep for named capture
    parse.one <- function(res, result) {
      m <- do.call(rbind, lapply(seq_along(res), function(i){
        if(result[i] == -1) return("")
        st <- attr(result, "capture.start")[i, ]
        substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)}))
      colnames(m) <- attr(result, "capture.names")
      m
    }
    pat <- 'lat=\"(?<lat>[[:digit:]]+[[:punct:]][[:digit:]]+)\".*lon=\"(?<lon>[[:digit:]]+[[:punct:]][[:digit:]]+)\".*<time>(?<year>[[:digit:]]{4})-(?<month>[[:digit:]]{2})-(?<day>[[:digit:]]{2})T(?<hour>[[:digit:]]{2}):(?<min>[[:digit:]]{2}):(?<sec>[[:digit:]]{2})Z</time>' # regexp expression to extract information, including hour/min/sec separately
    pattime <- '<time>(?<time>.*)</time>' # to extract the time, as originally formated
    patelev <- '<ele>(?<ele>.*)</ele>' # to extract the elevation

    time <- character(len)
    year <- numeric(len)
    month <- numeric(len)
    day <- numeric(len)
    hour <- numeric(len)
    min <- numeric(len)
    sec <- numeric(len)
    lat <- numeric(len)
    long <- numeric(len)
    elev <- numeric(len)
    for(i in 1:len){
      thisline <- lines[i]
      parsed <- regexpr(pat, thisline, perl = TRUE)
      temp <- parse.one(thisline, parsed)
      lat[i] <- as.numeric(temp[,1])
      long[i] <- as.numeric(temp[,2])
      year[i] <- as.numeric(temp[,3])
      month[i] <- as.numeric(temp[,4])
      day[i] <- as.numeric(temp[,5])
      hour[i] <- as.numeric(temp[,6])
      min[i] <- as.numeric(temp[,7])
      sec[i] <- as.numeric(temp[,8])

      parsed <- regexpr(pattime, thisline, perl = TRUE)
      time[i] <- parse.one(thisline, parsed)

      parsed <- regexpr(patelev, thisline, perl = TRUE)
      elev[i] <- as.numeric(parse.one(thisline, parsed))
    }

    data <- data.frame(time=time, year=year, month=month, day=day, hour=hour, min=min, sec=sec, lat=lat, long=long, elev = elev)
    out <- list(header=header, data=data)

    return(out)
  }
