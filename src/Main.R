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
    params = checkParams(params)
    ## Create/Load the Bathy grid for the area of interest
    bGrid = bathy(params$inputFile,
            params$inputFileType,
            params$startX, params$startY, 
            params$XDist, params$YDist,
            params$seriesName,
            debug)
    bGrid = list("bGrid"=bGrid, "cellRatio"=params$cellRatio)
    ## Specify a standard scale of x and y axes if previously undefined
    dims = dim(bGrid$bGrid)
    print(dims)
    if(!('x' %in% names(bGrid))) bGrid$x <- seq(0,1,length=dims[2])
    if(!('y' %in% names(bGrid))) bGrid$y <- seq(0,1,length=dims[1])
    
    fGrid = fish(params, bGrid)
    ## Find good sensor placements
    sensors = sensors(params$numSensors, bGrid, fGrid, params$range, params$bias, params, debug)
    
    ## Stat analysis of proposed setup.
    statDict = stats(params, bGrid, fGrid, sensors$sensorList)
    
    ## Return Fish grid, Bathy grid, and Sensor Placements as a Dictionary.
    results = list("bGrid" = bGrid, "fGrid" = fGrid, "sumGrid"=sensors$sumGrid, "sensors" = sensors$sensorList, 
            "stats" = statDict)
    return(results)
}


#### TEST RUN
params = list()
## Array variables
params$numSensors = 10 
params$range = 4 
params$cellRatio = 1
params$bias = 3

# BGrid Variables
params$inputFile = "himbsyn.bathytopo.v19.grd\\bathy.grd"
params$inputFileType = "netcdf"
params$startX = 9000
params$startY = 8000 
params$XDist = 50
params$YDist = 50
params$seriesName = 'z'

## Receiver variables
params$sd=1
params$peak=.75 
params$shapeFcn= "shape.t"

## Supression variables
params$supressionFcn = "supression.scale"
params$supressionRange = 4
params$maxSupressionValue = 1
params$minSupressionValue = .5
## Mean squared displacement of fish (a proxy for movement capacity)
params$msd <- 0.1
## Sampling time step
params$dt <- 1
## Choose random walk type movement model
params$fishmodel <- 'rw'
## Set to TRUE if vertical habitat range is applied
if(TRUE){
    ## Minimum depth
    params$mindepth <- -58
    ## Maximum depth
    params$maxdepth <- -60
}
## Set to TRUE if depth preference should be applied
if(TRUE){
    ## Depth preference of fish relative to bottom (in meters off the bottom)
    params$dp <- 10
    ## Strength of depth preference as a standard deviation, 95% of the time is spent within plus minus two dpsd
    params$dpsd <- 2
}
## Set to TRUE of Ornstein-Uhlenbeck (OU) movement should be applied
if(TRUE){
    ## Choose Ornstein-Uhlenbeck type movement model
    params$fishmodel <- 'ou'
    ## OU parameter: center of home range
    params$mu <- c(0.4,0.2)
    ## OU: Attraction parameter, determines strength of attraction toward home range center
    params$B <- 0.1*diag(2)
}

## Print time stamp (to be able to check run time)
startTime = Sys.time()
result = run(params,FALSE)
print(result)
## Print time stamp (to be able to check run time)
endTime = Sys.time()
paste('Starting:', startTime)
paste('Finished:', endTime)

## Plotting
graphics.off()
image(result$bGrid$x,result$bGrid$y,result$bGrid$bGrid,main='bGrid')
contour(result$bGrid$x,result$bGrid$y,result$bGrid$bGrid,xlab='x',ylab='y',add=TRUE,nlevels=5)
dev.new()
image(result$bGrid$x,result$bGrid$y,result$fGrid,main='fGrid')
numSensors <- length(result$sensors)
for(i in 1:numSensors) points(result$bGrid$x[result$sensors[[i]]$r],result$bGrid$y[result$sensors[[i]]$c])
dev.new()
image(1:25, 1:25 ,result$sumGrid,main='sumGrid')
