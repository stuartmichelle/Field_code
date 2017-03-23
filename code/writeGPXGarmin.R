#' Write GPX
#'
#' This function allows you to write GPX format given the header and the data from readGPXGarmin_2013_06_01.R
#' @param outfile The file name of the output
#' @param filename The name of the input
#' @keywords gpx
#' @export
#' @examples
#' writeGPX(outfile, filename)


writeGPX = function(outfile, filename){
	con = file(filename, open = 'wt')
	cat(outfile$header, file=con) # write the same header

	# write each trkpt
	for(i in 1:nrow(outfile$data)){
		cat('<trkpt lat="', file=con)
		cat(outfile$data$lat[i], file=con)
		cat('" lon="', file=con)
		cat(outfile$data$lon[i], file=con)
		cat('"><ele>', file=con)
		cat(outfile$data$elev[i], file=con)
		cat('</ele><time>', file=con)
		cat(as.character(outfile$data$time[i]), file=con)
		cat('</time></trkpt>', file=con)
	}

	# finish up the file
	cat('</trkseg></trk></gpx>', file=con)

	# close up
	close(con)
}
