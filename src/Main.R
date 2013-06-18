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
    # holds the number of new cells that a single bathymetric cell should
    # be split into.  Setting cellRatio to 10 signifies that one bathymetric cell
    # will be split into a ten by ten grid.
    cellRatio = 1
    bias = .5
    ## Create/Load the Bathy grid for the area of interest
    bGrid <- bathy(inputFile = "himbsyn.bathytopo.v19.grd\\bathy.grd",
            startX = 9000, startY = 8000, 
            XDist = 5, YDist = 5,
            seriesName = 'z',
            debug = TRUE)
    bGrid = list("bGrid"=bGrid, "cellRatio"=cellRatio)
    
    ## Create Fish grid
    fGrid = fish(params, bGrid)
    
    ## Find good sensor placements
    sensors = sensors(numSensors, bGrid, fGrid, range, bias)
    
    
    ## Stat analysis of proposed setup.
    statDict = stats(params, bGrid, fGrid, sensors)
    
    ## Return Fish grid, Bathy grid, and Sensor Placements as a Dictionary.
    results = list("bGrid" = bGrid, "fGrid" = fGrid, "sensors" = sensors, 
            "stats" = statDict)
    
    return(results)
}

# Test execution.
params = {}
result = run(params)
#print(result)

## Plotting
graphics.off()
image(result$bGrid,main='bGrig')
contour(result$bGrid,xlab='x',ylab='y',add=TRUE,nlevels=5)
dev.new()
image(result$fGrid,main='fGrig')
numSensors <- length(result$Sensors)
nx <- dim(result$fGrid)[2]
ny <- dim(result$fGrid)[1]
for(i in 1:numSensors) points(result$Sensors[[i]][2]/nx,result$Sensors[[i]][1]/ny)
