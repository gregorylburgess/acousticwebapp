# Main Method
# Supported operations:
#		1. User provides area of interest and sensor locations, asks for stats.
#		2. User provides area of interest and number of sensors, asks for optimal 
#				placement and stats.
#		3. User provides area of interest, fish behaviors, and number of sensors, 
#				asks for optimal placement and stats.
source('Bathy.R')
source('FishModel.R')
source('Utility.R')

	run <- function(params){
		## Create/Load the Bathy Grid for the area of interest
		BGrid = bathy(params)
		
		## Create Fish Grid
		FGrid = fish(params)
		
		## Find good sensor placements
		sensors = sensors(params, BGrid, FGrid)
		
		## Stat analysis of proposed setup.
		statDict = stats(params, BGrid, FGrid, sensors)
		
		## Return Fish Grid, Bathy Grid, and Sensor Placements as a Dictionary.
		results = list("BGrid" = BGrid, "FGrid" = FGrid, "Sensors" = sensors, 
				"Stats" = statDict)

		return(results)
	}

# Test execution.
params = {}
result = run(params)
print(result)
