# Provides utility functions.

source("shapefunctions.R")

	# Finds a "good" set of sensor placements for a given setup [BGrid, FGrid, params].
	# Returns a list of locations as grid coordinates.
	sensors <- function(numSensors, BGrid, FGrid, range, bias, debug=FALSE) {
		#TODO create a function to mesh the BGrid and FGrid
		Grid = BGrid
		#TODO create a function to mesh the BGrid and FGrid
		sensorList = {}
		rows = dim(BGrid)[1]
		cols = dim(BGrid)[2]
		
		mergeGrid(BGrid, FGrid, bias)
		
		# for each sensor, find a good placement
		for (i in 1:numSensors) {
			# calculate the sumGrid
			sumGrid = sumGrid(Grid, range, debug)
			
			# find the max location 
			maxLoc = which.min(sumGrid)
			maxLoc = c(r = row(sumGrid)[maxLoc], c = col(sumGrid)[maxLoc]) 
			
			# append maxLoc to the sensor list.
			sensorList = c(sensorList, list(maxLoc))
			
			#zero out all the values within range of the target cell.
			Grid= zeroOut(Grid, maxLoc, range, 0)
		}
		return(sensorList)
	}
	
	
	# For each cell in a given grid, the function sums the values of neighboring
	# cells within a given range.
	sumGrid<- function (Grid, range, debug=FALSE) {
		tempGrid = Grid
		rows = dim(Grid)[1]
		cols = dim(Grid)[2]
		for (i in 1:rows) {
			for(j in 1:cols) {
				vals = getArea(c(r=i,c=j), dim(Grid), range)
				tempGrid[i,j] = sum(Grid[vals["rs"]:vals["re"], vals["cs"]:vals["ce"]])
			}
		}
		if(debug){
			write("TempGrid", stderr())
			print(tempGrid)
			write("Grid",stderr())
			print(Grid)
		}
		return(tempGrid)
	}
	
	
	# Ingests a Grid, and sets the cells between RStart and REnd, 
	# and CStart and CEnd to the provided value.
	zeroOut<-function(Grid, loc, range, value) {
		vals = getArea(loc, dim=dim(Grid), range)
		mini = vals["rs"]
		maxi = vals["re"]
		minj = vals["cs"]
		maxj = vals["ce"]
		for (i in mini:maxi) {
			for (j in minj:maxj) {
				Grid[i,j] = value
			}
		}
		return(Grid)
	}
	
	# Defines the "shape" of a sensor's range, returns an set of start/end indexes
	# for rows and columns respectively named : {rs,re,cs,ce}.
	getArea<-function(loc, dim, range) {
		r = loc["r"] # the row index for our central point
		c = loc["c"] # the col index for our central point
		rows = dim[1] # the max number of rows in the grid
		cols = dim[2] # the max number of cols in the grid
		
		# defines a square
		rs0 = max(1, r - range) 
		re0 = min(rows, r + range)
		cs0 = max(1 ,c - range)
		ce0 = min(cols, c + range)
		toRet = c(rs=rs0, re=re0, cs=cs0, ce=ce0)
		return(toRet)
	}
	
	
	# Determines the likelihood of a tag at a given position is detectable by a sensor at a 
	# given position, using a specific shape fcn.  This function considers Bathymetry and
	# sensor range.
	# Returns the percent chance of detection as a double between 0 [no chance of detection] 
	# and 1 [guaranteed detection].
	detect <- function(BGrid, sensorPos, tagPos, fcn, params) {
		# Check for proper parameter lengths
		if (fcn == "shape.sigmoidal") {
			if (! length(params) == 3) {
				write("Insufficient Parameters", stderr())
			}
		}
		else {
			if (! length(params) == 2) {
				write("Insufficient Parameters", stderr())
			}
		}
		
		dist = sqrt((sensorPos["c"] - tagPos["c"])^2 + (sensorPos["r"] - tagPos["r"])^2)
		return(do.call(fcn, list(dist, params)))
	}
	
	
	# Merges the BGrid and FGrid into a single Grid measuring the "goodness" of a location 
	# in terms of sensor placement.
	mergeGrid<- function(BGrid, FGrid, bias) {
		BGridBias = bias
		FGridBias = 1-bias
		Grid = {}
		return (Grid)
	}
	
	# Provides Statistical data on detection, given a particular BGrid, FGrid, and sensor 
	# arrangement.
	# Returns a dictionary of staistical values.
	stats <- function(params, BGrid, FGrid, sensors) {
		statDict = {}
		return(statDict)
	}


detect({}, c(c=1,r=3), c(c=2,r=5), "shape.t", c(1,.75))