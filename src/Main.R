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

run <- function(params, debug=FALSE){
    if(debug) {
        cat("\n[run]\n")
    }
    numSensors = 2
    range = 2
    parameters = list(sd=1, peak=.75, fcn= "shape.t")
    # holds the number of new cells that a single bathymetric cell should
    # be split into.  Setting cellRatio to 10 signifies that one bathymetric cell
    # will be split into a ten by ten grid.
    cellRatio = 1
    bias = 3
    ## Create/Load the Bathy grid for the area of interest
    bGrid <- bathy(inputFile = "himbsyn.bathytopo.v19.grd\\bathy.grd",
            startX = 9000, startY = 8000, 
            XDist = 10, YDist = 10,
            seriesName = 'z',
            debug)
    
    bGrid = list("bGrid"=bGrid, "cellRatio"=cellRatio)
    
    ## Create Fish grid
    fGrid = fish(params, bGrid)
    
    ##Test
    rows = dim(fGrid)[1]
    cols = dim(fGrid)[2]
    for (i in 1:rows) {
        for (j in 1:cols) {
            fGrid [i,j]= (i-1)*rows + j
        }
    }
    ## Find good sensor placements
    sensors = sensors(numSensors, bGrid, fGrid, range, bias, parameters, debug)
    
    ## Stat analysis of proposed setup.
    statDict = stats(params, bGrid, fGrid, sensors)
    
    ## Return Fish grid, Bathy grid, and Sensor Placements as a Dictionary.
    results = list("bGrid" = bGrid, "fGrid" = fGrid, "sensors" = sensors, 
            "stats" = statDict)
    return(results)
}

# Test execution.
params = {}
result = run(params,FALSE)
print(result)

## Plotting
graphics.off()
image(result$bGrid$bGrid,main='bGrig')
contour(result$bGrid$bGrid,xlab='x',ylab='y',add=TRUE,nlevels=5)
dev.new()
image(result$fGrid,main='fGrig')
numSensors <- length(result$sensors)
nx <- dim(result$fGrid)[2]
ny <- dim(result$fGrid)[1]
for(i in 1:numSensors) points(result$sensors[[i]]$c/nx,result$sensors[[i]]$r/ny)
