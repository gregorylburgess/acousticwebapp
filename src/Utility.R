# Provides utility functions.

source("ShapeFunctions.R")

# Finds a "good" set of sensor placements for a given setup [bGrid, fGrid, params].
# Returns a list of locations as grid coordinates.
sensors <- function(numSensors, bGrid, fGrid, range, bias, debug=FALSE) {
    #TODO create a function to mesh the bGrid and fGrid
    grid = bGrid$bGrid
    
    sensorList = {}
    rows = dim(bGrid)[1]
    cols = dim(bGrid)[2]
    
    mergeGrid(bGrid, fGrid, bias)
    
    # for each sensor, find a good placement
    for (i in 1:numSensors) {
        # calculate the sumGrid
        sumGrid = sumGrid(grid, range, debug)
        
        # find the max location 
        maxLoc = which.min(sumGrid)
        #TODO actually convert to rows and cols, 
        #not just search for the value of the cell
        maxLoc = c(r = row(sumGrid)[maxLoc], c = col(sumGrid)[maxLoc]) 
        
        # append maxLoc to the sensor list.
        sensorList = c(sensorList, list(maxLoc))
        
        #zero out all the values within range of the target cell.
        grid= zeroOut(grid, maxLoc, range, 0)
    }
    return(sensorList)
}


# For each cell in a given grid, the function sums the values of neighboring
# cells within a given range.
sumGrid<- function (grid, range, debug=FALSE) {
    tempGrid = grid
    rows = dim(grid)[1]
    cols = dim(grid)[2]
    for (i in 1:rows) {
        for(j in 1:cols) {
            vals = getArea(c(r=i,c=j), dim(grid), range)
            tempGrid[i,j] = sum(grid[vals["rs"]:vals["re"], vals["cs"]:vals["ce"]])
        }
    }
    if(debug){
        write("tempGrid", stderr())
        print(tempGrid)
        write("grid",stderr())
        print(grid)
    }
    return(tempGrid)
}


# Ingests a grid, and sets the cells between RStart and REnd, 
# and CStart and CEnd to the provided value.
zeroOut<-function(grid, loc, range, value) {
    vals = getArea(loc, dim=dim(grid), range)
    mini = vals["rs"]
    maxi = vals["re"]
    minj = vals["cs"]
    maxj = vals["ce"]
    for (i in mini:maxi) {
        for (j in minj:maxj) {
            grid[i,j] = value
        }
    }
    return(grid)
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
detect <- function(bGrid, sensorPos, tagPos, fcn, params) {
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
    
    dist = sqrt((sensorPos$c - tagPos$c)^2 + (sensorPos$r - tagPos$r)^2)
    probOfRangeDetection = do.call(fcn, list(dist, params))
    probOfLOSDetection = checkLOS(bGrid, sensorPos, tagPos, params)
    return(probOfRangeDetection * probOfLOSDetection)
}

# Returns the percent of the water column visible at a target cell from a
# starting cell.
checkLOS<- function(bGrid, startingCell, targetCell, params, debug=FALSE) {
    sensorHeight = params["sensorHeight"]
    initialHeight = bGrid[sC$c, sC$r]
    cells = getCels(startingCell, targetCell)
    
}

# Returns the cells crossed by a beam from the starting cell to
# the target cell.
getCells<-function(startingCell,targetCell) {
    sC=offset(startingCell)
    tC=offset(targetCell)
    m = (sC$r-tC$r)/(sC$c-tC$c)
    # assume the sensor is in the middle of the cell
    b = sC$r-m*sC$c
    lowerX = min(startingCell$c, targetCell$c)
    upperX = max(startingCell$c, targetCell$c)
    lowerY = min(startingCell$r, targetCell$r)
    upperY = max(startingCell$r, targetCell$r)
    print(c("y=",m,"x+",b))
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
            #print(c("x=",x))
            #print(c("y=",y))

            #print(c(x1,y))
            #print(c((x1),y+1))
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
            lowerX = upperX
            upperX = temp
        }
        for( x in startX:endX) {
            y= m * (x) + b
            y1= ceiling(y)
            #print(c("x=",x))
            #print(c("y=",y))
            
            if(y == y1) {
                print(c(x,y))
                tx = c(tx,x)
                ty= c(ty,y1)
            } else {
                #print(c(x,y1))
                #print(c((x+1),y1))
                tx=c(tx,x)
                ty =c(ty,y1)
                if(x+1<=upperX) {
                    tx=c(tx,x+1)
                    ty=c(ty,y1)
                }
            }
        }
    }
    grid = data.frame("x"=tx,"y"=ty)
    grid = unique(grid)
    return(grid)
}

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
# Merges the bGrid and fGrid into a single grid measuring the "goodness" of a location 
# in terms of sensor placement.
mergeGrid<- function(bGrid, fGrid, bias) {
    bGridBias = bias
    fGridBias = 1-bias
    grid = {}
    return (grid)
}

# Provides Statistical data on detection, given a particular bGrid, fGrid, and sensor 
# arrangement.
# Returns a dictionary of staistical values.
stats <- function(params, bGrid, fGrid, sensors) {
    statDict = {}
    return(statDict)
}


detect({}, list(c=1,r=5), list(c=3,r=1), "shape.t", c(1,.75))