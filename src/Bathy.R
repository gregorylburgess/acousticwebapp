# Loads and constructs a Bathymetry Grid for the area of interest.  
# Returns a Grid of depths for the area of interest.
# Dependency: Have the ncdf module installed.

library(ncdf)


	bathy <- function(inputFile, startX=0, startY=0, XDist, YDist, seriesName, debug=FALSE) {
		
		# open the netCDF file
		ncdfObj = open.ncdf(inputFile)

		# grab a slice (in grid form)
		BGrid = get.var.ncdf(ncdfObj, 'z', start=c(startX, startY), 
				count=c( XDist, YDist))
		
		# debug print
		if (isTRUE(debug)) {
			print(ncdfObj)
			print(BGrid)
		}
		
		# return grid
		return(BGrid)
	}

	
	