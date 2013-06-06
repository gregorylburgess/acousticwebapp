# Provides utility functions for the program.

	# Finds a "good" set of sensor placements for a given setup [BGrid, FGrid, params].
	# Returns a list of locations as grid coordinates.
	sensors <- function(params, BGrid, FGrid) {
		sensorList = {}
		return(sensorList)
	}
	
	
	# Determines the likelihood of a tag at a given position is detectable by a sensor at a 
	# given position, using a specific shape fcn.
	# Returns the percent chance of detection as a double between 0 [no chance of detection] 
	# and 1 [guaranteed detection].
	detect <- function(sensorPos, tagPos, fcn) {
		probDetected = 0
		return(probDetected)
	}
	
	
	# Provides Statistical data on detection, given a particular BGrid, FGrid, and sensor 
	# arrangement.
	# Returns a dictionary of staistical values.
	stats <- function(params, BGrid, FGrid, sensors) {
		statDict = {}
		return(statDict)
	}