# Loads and constructs a Bathymetry Grid for the area of interest.  
# Returns a Grid of depths for the area of interest.
# Dependency: Have the ncdf module installed.

library(ncdf)


	bathy <- function(inputFile, startX=0, startY=0, XDist, YDist, seriesName, debug=FALSE) {

		if(file.exists(inputFile)){
			## open the netCDF file
			ncdfObj = open.ncdf(inputFile)

			## grab a slice (in grid form)
			BGrid = get.var.ncdf(ncdfObj, 'z', start=c(startX, startY), 
			count=c( XDist, YDist))
		
          	## debug print
          	if (isTRUE(debug)) {
				print(ncdfObj)
				print(BGrid)
		  	}
        }
		
		else {
			## Create test BGrid to use if real data unavailable
			nx <- 100
			ny <- 100
			x <- seq(-2*pi,2*pi,length=nx)
			X <- matrix(rep(x,ny),ny,nx,byrow=TRUE)
			y <- seq(-2*pi,2*pi,length=ny)
			Y <- matrix(rep(y,nx),ny,nx,byrow=FALSE)
			BGrid <- sin(X)*sin(Y)*abs(X)*abs(Y)-pi
			BGrid[BGrid>0] <- 0
			## Plot BGrid
			##image(x,y,BGrid)
			##contour(x,y,BGrid,xlab='x',ylab='y',add=TRUE,nlevels=5)
		}
		
		## return grid
		return(BGrid)
	}

	
	
