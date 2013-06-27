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
    ## Specify a standard scale of x and y axes if previously undefined
    if(!('x' %in% names(bGrid))) bGrid$x <- seq(0,1,length=dim(bGrid$bGrid)[2])
    if(!('y' %in% names(bGrid))) bGrid$y <- seq(0,1,length=dim(bGrid$bGrid)[1])
    
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

## Test execution.
params = list()
## Mean squared displacement of fish (a proxy for movement capacity)
params$msd <- 0.1
## Sampling time step
params$dt <- 1
## Choose random walk type movement model
params$fishmodel <- 'rw'
## Set to TRUE if depth preference should be applied
if(FALSE){
    ## Depth preference of fish
    params$dp <- -5    
    ## Strength of depth preference as a standard deviation, 95% of the time is spent within plus minus two dpsd
    params$dpsd <- 2
}
## Set to TRUE of Ornstein-Uhlenbeck (OU) movement should be applied
if(FALSE){
    ## Choose Ornstein-Uhlenbeck type movement model
    params$fishmodel <- 'ou'
    ## OU parameter: center of home range
    params$mu <- c(0.4,0.2)
    ## OU: Attraction parameter, determines strength of attraction toward home range center
    params$B <- 0.1*diag(2)
}

result = run(params,FALSE)
print(result)

## Plotting
graphics.off()
image(bGrid$x,bGrid$y,t(result$bGrid$bGrid),main='bGrid')
contour(bGrid$x,bGrid$y,t(result$bGrid$bGrid),xlab='x',ylab='y',add=TRUE,nlevels=5)
dev.new()
image(bGrid$x,bGrid$y,t(result$fGrid),main='fGrid')
numSensors <- length(result$sensors)
nx <- dim(result$fGrid)[2]
ny <- dim(result$fGrid)[1]
for(i in 1:numSensors) points(result$sensors[[i]]$c/nx,result$sensors[[i]]$r/ny)
