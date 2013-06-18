# Main Method
# Supported operations:
#		1. User provides area of interest and sensor locations, asks for stats.
#		2. User provides area of interest and number of sensors, asks for optimal 
#				placement and stats.
#		3. User provides area of interest, fish behaviors, and number of sensors, 
#				asks for optimal placement and stats.
rm(list=ls()) ## Clear all variables
source('Bathy.R')
source('FishModel.R')
source('Utility.R')

	run <- function(params){
		numSensors = 2
		range = 1
		bias = .5
		## Create/Load the Bathy Grid for the area of interest
		BGrid <- bathy(inputFile = "himbsyn.bathytopo.v19.grd\\bathy.grd",
				startX = 9000, startY = 8000, 
				XDist = 5, YDist = 5,
				seriesName = 'z',
				debug = TRUE)
		## Create Fish Grid
		FGrid = fish(params, BGrid)
		
		## Find good sensor placements
		sensors = sensors(numSensors, BGrid, FGrid, range, bias)
		

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
##print(result)

## Plotting
graphics.off()
image(result$BGrid,main='BGrig')
contour(result$BGrid,xlab='x',ylab='y',add=TRUE,nlevels=5)
dev.new()
image(result$FGrid,main='FGrig')
numSensors <- length(result$Sensors)
nx <- dim(result$FGrid)[2]
ny <- dim(result$FGrid)[1]
for(i in 1:numSensors) points(result$Sensors[[i]][2]/nx,result$Sensors[[i]][1]/ny)
