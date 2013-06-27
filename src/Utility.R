# Provides utility functions.

source("ShapeFunctions.R")

# Finds a "good" set of sensor placements for a given setup [bGrid, fGrid, params].
# Returns a list of locations as grid coordinates.
# Bias cases:
# 1. fish only
# 2. bathy only
# 3. detectable fish due to bathy
sensors <- function(numSensors, bGrid, fGrid, range, bias, params, debug=FALSE) {
    if (debug) {
        cat("\n[sensors]\n")
        print("bGrid")
        print(bGrid)
        print("fGrid")
        print(fGrid)
        print(sprintf("bias=%g",bias))
        print("params")
        print(params)
    }
    
    sensorList = {}
    rows = dim(fGrid)[1]
    cols = dim(fGrid)[2]
    grids = list("bGrid" = bGrid, "fGrid"=fGrid)
    
    # for each sensor, find a good placement
    for (i in 1:numSensors) {
        # calculate the sumGrid
        grids = sumGrid(grids, range, bias, params, debug)
        #cat("\n\n[[INITIAL GRIDS]]\n\n")
        print(grids)
        # find the max location 
        sumGrid = grids$sumGrid
        maxLoc = which.max(sumGrid)
        maxLoc = list(r=ceiling(maxLoc/rows), c=maxLoc %% cols)
        print(maxLoc)
        # append maxLoc to the sensor list.
        sensorList = c(sensorList, list(maxLoc))
        #zero out all the values within range of the target cell.
        #print(sprintf("zeroing out: (%g,%g)", maxLoc$c, maxLoc$r))
        grids= zeroOut(grids, maxLoc, range, 0, debug)
        #cat("\n\n[[ZEROED OUT GRIDS]]\n\n")
        #print(grids)
    }
    return(sensorList)
}


# Calculates the composite "goodness" grid for a particular bias.
sumGrid<- function (grid, range, bias, params, debug=FALSE) {
    if (debug) {
        cat("\n[sumGrid]\n")
        print("bGrid")
        print(grid$bGrid)
        print("fGrid")
        print(grid$fGrid)
        print(sprintf("bias=%g", bias))
        print("params")
        print(params)
    }
    #Fish
    if (bias == 1) {
        return(sumGrid.sumSimple(grid, "fGrid", range, debug))
    }
    #Bathy
    else if (bias == 2) {
        return(sumGrid.sumBathy(grid, range, params$fcn, params, debug))
    }
    #Combo
    else if (bias ==3) {
        return(sumGrid.sumProduct(grid, range, params$fcn, params, debug))
    }
    else {
        write("ERROR: Invalid Bias", stderr())
    }   
}

# Simply sums the values within range of a cell for each cell in the given grid.
sumGrid.sumSimple <- function (grid, key, range, debug=FALSE) {
    tempGrid = get(key, grid)
    tempCopy = tempGrid
    rows = dim(tempGrid)[1]
    cols = dim(tempGrid)[2]
    
    
    for (i in 1:rows) {
        for(j in 1:cols) {
            vals = getArea(list(r=i,c=j), dim(tempGrid), range)
            tempGrid[i,j] = sum(tempCopy[vals$rs:vals$re, 
                                         vals$cs:vals$ce])
        }
    }
    
    grid$sumGrid = tempGrid
    if(debug){
        cat("\n[sumGrid.sumSimple]\n")
        print("grid")
        print(grid)
    }
    return(grid)
}

# Sums the result of calling the detect() function on each cell within range of 
# a target cell for each cell in the given grid.
sumGrid.sumBathy <- function (grid, range, fcn="shape.t", 
        params, debug=FALSE) {
    
    sumGrid0 = grid$bGrid$bGrid
    tempCpy = grid$bGrid$bGrid
    rows = dim(sumGrid0)[1]
    cols = dim(sumGrid0)[2]
    
    for (i in 1:rows) {
        for(j in 1:cols) {
            vals = getArea(list(r=i,c=j), dim(sumGrid0), range)
            rs = vals$rs
            re = vals$re
            cs = vals$cs
            ce = vals$ce
            visibilities = {}
            for (r in rs:re) {
                for (c in cs:ce) {
                    visibilities = c(
                            visibilities,
                            detect(tempCpy, sensorPos=list(r=i,c=j), tagPos=list(r=r,c=c), fcn=fcn,
                            params, debug))
                }
            }
            sumGrid0[i,j] = sum(visibilities)
        }
    }
    
    grid$sumGrid = sumGrid0
    if(debug){
        cat("\n[sumGrid.sumBathy]\n")
        print("visibilities")
        print(visibilities)
        print("grid")
        print(grid)
    }
    return(grid)
}

# For each cell in a given grid, the function sums (the number of fish
# times the probability of detection) for all cells within range
sumGrid.sumProduct <- function (grids, range, fcn="shape.t", 
        params, debug=FALSE) {
    
    sumGrid0 = grids$bGrid$bGrid
    fGrid = grids$fGrid
    tempCpy = grids$bGrid$bGrid
    rows = dim(sumGrid0)[1]
    cols = dim(sumGrid0)[2]
    
    for (i in 1:rows) {
        for(j in 1:cols) {
            vals = getArea(list(r=i,c=j), dim(sumGrid0), range)
            rs = vals$rs
            re = vals$re
            cs = vals$cs
            ce = vals$ce
            visibilities = {}
            for (r in rs:re) {
                for (c in cs:ce) {
                    visibilities = c(
                            visibilities,
                            detect(tempCpy, sensorPos=list(r=i,c=j), tagPos=list(r=r,c=c), fcn=fcn,
                                    params, debug) * fGrid[r,c])
                }
            }
            sumGrid0[i,j] = sum(visibilities)
        }
    }
    
    grids$sumGrid = sumGrid0
    if(debug){
        cat("\n[sumGrid.sumProduct]\n")
        print("visibilities")
        print(visibilities)
        print("grids")
        print(grids)
    }
    return(grids)
}

# Ingests a grid, and sets the cells between RStart and REnd, 
# and CStart and CEnd to the provided value.
zeroOut<-function(grids, loc, range, value, debug=FALSE) {
    if(debug) {
        cat("\n[zeroOut]\n")
        print(sprintf("loc: (%g,%g)",loc$c,loc$r))
        print("grid")
        print(grids)
    }
    
    bGrid = grids$bGrid$bGrid
    fGrid = grids$fGrid
    vals = getArea(loc, dim=dim(fGrid), range)
    mini = vals$rs
    maxi = vals$re
    minj = vals$cs
    maxj = vals$ce
    for (i in mini:maxi) {
        for (j in minj:maxj) {
            bGrid[i, j] = NA
            fGrid[i, j] = value
        }
    }
    grids$bGrid$bGrid = bGrid
    grids$fGrid = fGrid
    return(grids)
}

# Defines the "shape" of a sensor's range, returns an set of start/end indexes
# for rows and columns respectively named : {rs,re,cs,ce}.
getArea<-function(loc, dim, range) {
    r = loc$r # the row index for our central point
    c = loc$c # the col index for our central point
    rows = dim[1] # the max number of rows in the grid
    cols = dim[2] # the max number of cols in the grid
    
    # defines a square
    rs0 = max(1, r - range) 
    re0 = min(rows, r + range)
    cs0 = max(1 ,c - range)
    ce0 = min(cols, c + range)
    toRet = list(rs=rs0, re=re0, cs=cs0, ce=ce0)
    return(toRet)
}


# Determines the likelihood of a tag at a given position is detectable by a sensor at a 
# given position, using a specific shape fcn.  This function considers Bathymetry and
# sensor range.
# Returns the percent chance of detection as a double between 0 [no chance of detection] 
# and 1 [guaranteed detection].
detect <- function(bGrid, sensorPos, tagPos, fcn, params, debug=FALSE) {
    # Check for proper parameter lengths
    if (fcn == "shape.sigmoidal") {
        if (length(params) < 3) {
            write("Insufficient Parameters", stderr())
        }
    }
    else {
        if (length(params)< 2) {
            write("Insufficient Parameters", stderr())
        }
    }
    dist = sqrt((sensorPos$c - tagPos$c)^2 + (sensorPos$r - tagPos$r)^2)
    probOfRangeDetection = do.call(fcn, list(dist, params))
    
    probOfLOSDetection = checkLOS(bGrid, sensorPos, tagPos, params, debug)
    probOfDetection = probOfRangeDetection * probOfLOSDetection
    if(debug) {
        cat("\n[detect]\n")
        print(sprintf("probOfLOSDetection=%g",probOfLOSDetection))
        print(sprintf("probOfRangeDetection=%g",probOfRangeDetection))
        print(sprintf("TotalProbOfDetection=%g",probOfDetection))
    }
    return(probOfRangeDetection * probOfLOSDetection)
}


# Returns the percent of the water column visible at a target cell from a
# starting cell.
checkLOS<- function(bGrid, startingCell, targetCell, params, debug=FALSE) {
    sensorElevation = params["sensorElevation"]
    sensorElevation = 1
    dist = sqrt((startingCell$c - targetCell$c)^2 + (startingCell$r - targetCell$r)^2)
    if (dist ==0) {
        return(1)
    }
    # our sensor's z value
    sensorDepth = bGrid[startingCell$r, startingCell$c] + sensorElevation
    # retrieve list of intervening cells
    table = getCells(startingCell, targetCell, debug) ######getCells returns nothing because the cells are adjacent...
    # annotate each cell's z value from the bGrid
    table$z <-apply(table, 1, function(rows){ table$z = bGrid[rows[2],rows[1]]})
    # annotate each cell's percieved slope form our sensor to the cell
    table$m <-apply(table, 1, function(row) { 
                table$m = (row[3] - sensorDepth) / sqrt((row[1] - startingCell$c)^2 + (row[2] - startingCell$r)^2)
            })
    # take the max of all slopes as the limit on our LoS
    m = max(table$m)
    b = sensorDepth
    # y = mx + b
    targetCellsVisibleDepth = m*dist + b
    # compute % visibility (of water column height) from sensor to target cell
    percentVisibility = targetCellsVisibleDepth / bGrid[targetCell$r,targetCell$c]
    percentVisibility = min(1, percentVisibility)
    percentVisibility = max(0, percentVisibility)
    if (debug) {
        cat("\n[checkLOS]\n")
        print(sprintf("sensorDepth=%g",sensorDepth))
        print(sprintf("dist/z: y=%gx+%g", m,b))
        print("Table:")
        print(table)
        print(sprintf("targetCellsVisibleDepth=%g", targetCellsVisibleDepth))
        print(sprintf("percentVisibility=%g",percentVisibility))
    }
    return(percentVisibility)
}

# Returns the cells crossed by a beam from the starting cell to
# the target cell.
getCells<-function(startingCell, targetCell, debug=FALSE) {
    sC=offset(startingCell)
    tC=offset(targetCell)
    m = (sC$r-tC$r)/(sC$c-tC$c)
    if(abs(m)== Inf) {
        if(m>0) {
            m = 999999
        }
        else {
            m = -999999
        }
    }
    # assume the sensor is in the middle of the cell
    b = sC$r-m*sC$c
    lowerX = min(startingCell$c, targetCell$c)
    upperX = max(startingCell$c, targetCell$c)
    lowerY = min(startingCell$r, targetCell$r)
    upperY = max(startingCell$r, targetCell$r)
    tx = {}
    ty= {}

    #STEEP SLOPES
    if(abs(m)>1) {
        startY = lowerY
        endY = upperY
        if(m<0){
            temp = startY
            startY = endY
            endY = temp
        }
        for( y in startY:endY) {
            x = (y-b)/m
            x1= ceiling(x)
            tx=c(tx,x1)
            ty =c(ty,y)
            if(y+1<=upperY) {
                tx=c(tx,x1)
                ty=c(ty,y+1)
            }
        }
    } else {
        #SLOW SLOPES
        startX = lowerX
        endX = upperX
        if(m<0){
            temp = lowerX
            startX = upperX
            endX = temp
        }
        for( x in startX:endX) {
            y = m * x + b
            y1 = ceiling(y)
            if(y == y1) {
                tx = c(tx,x)
                if (m<0) { 
                    ty= c(ty,y1+1)
                }
                else {
                    ty= c(ty,y1)
                }
            } else {
                tx=c(tx,x)
                ty =c(ty,y1)
                if(x+1<=upperX) {
                    tx=c(tx,x+1)
                    ty=c(ty,y1)
                }
            }
        }
    }
    start = list('x'=startingCell$c,'y'=startingCell$r)
    end = list('x'=targetCell$c,'y'=targetCell$r)
    grid = data.frame("x"=tx,"y"=ty)
    # uniques
    grid = unique(grid)
    # remove start and end cells
    grid = grid[!(grid$x == startingCell$c & grid$y == startingCell$r),]
    #grid = grid[!(grid$x == targetCell$c & grid$y == targetCell$r),]
    if(debug) {
        cat("\n[getCells]\n")
        print(sprintf("x/y: y = %gx + %g",m,b))
        print(sprintf("Starting Cell:(%g,%g)",startingCell$c,startingCell$r))
        print(sprintf("Target Cell: (%g,%g)",targetCell$c,targetCell$r))
        print("Table:")
        print(grid)
    }
    return(grid)
}

# Offsets a cartesian point towards the center of the gridcell it represents.
# ex: the cartesian point (3,2) would be converted to (2.5, 1.5), which puts it in the
# cell located at the third column, second row (aka the cell at (3,2) on a 1-based grid
# system).
offset<- function(point){
    r= point$r
    c=point$c
    if(r>0) {
        r=r-.5
    } else {
        r=r+.5
    }
    if(c>0){
        c=c-.5
    } else {
        c=c+.5
    }
    return(list("r"=r,"c"=c))
}

# Provides Statistical data on detection, given a particular bGrid, fGrid, and sensor 
# arrangement.
# Returns a dictionary of staistical values.
stats <- function(params, bGrid, fGrid, sensors) {
    statDict <- list()
    numSensors <- length(sensors)
    xSens <- rep(0,numSensors)
    ySens <- rep(0,numSensors)
    for(i in 1:numSensors){
        xSens[i] <- bGrid$x[sensors[[i]]$c]
        ySens[i] <- bGrid$y[sensors[[i]]$r]
    }
    distMat <- matrix(0,numSensors,numSensors)
    for(i in 1:numSensors){
        distMat[i,] <- sqrt((xSens[i]-xSens)^2 + (ySens[i]-ySens)^2)
    }
    ## a is the median of the distances between the receivers
    a <- median(distMat[upper.tri(distMat)])
    ## delta is a sparsity measure (see Pedersen & Weng 2013)
    statDict$delta <- a/(2*params$range) 
    ## phi is a dimensionless indicator of movement capacity relative to detection range, it can also be viewed as a signal to noise ratio
    statDict$phi <- params$msd/params$range

    return(statDict)
}

#Test
#source('Bathy.R')
#bGrid <- bathy(inputFile = "himbsyn.bathytopo.v19.grd\\bathy.grd",
#    startX = 8700, startY = 8000, 
#    XDist = 5, YDist = 5,
#    seriesName = 'z',
#    debug = TRUE)
"
for( i in 1:5){
    for (j in 1:5) {
        bGrid[i,j] = (i-5)*rows +5
    }
}
temp= bGrid

fGrid = temp
bGrid = list(bGrid=bGrid)
grids = list(fGrid=fGrid, bGrid=bGrid)
print(zeroOut(grids, list(r=1,c=1), 1, 0, TRUE))"
#detect(bGrid, list(c=5,r=5), list(c=5,r=4), "shape.t", list(sd=1, peak=.75, fcn= "shape.t"), debug=TRUE)
