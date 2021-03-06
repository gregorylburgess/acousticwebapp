source("Utility.R")
library("data.table")
# Use a non-square grid to ensure that columns and rows
# are being correctly referenced
r=5
c=2
bGrid = matrix(c(1,2,3,4,5,6,7,8,9,10), 
        nrow=r, 
        ncol=c) 
cellRatio = 1
fGrid = bGrid
bGrid = list(bGrid=bGrid, cellRatio=cellRatio)
grids = list(bGrid=bGrid, fGrid=fGrid)
range = 1
params = checkParams(list(numSensors=0))

# Resets the test values
reset <- function () {
    bGrid = matrix(c(1,2,3,4,5,6,7,8,9,10), 
            nrow=r, 
            ncol=c) 
    cellRatio = 1
    fGrid = bGrid
    bGrid = list(bGrid=bGrid, cellRatio=cellRatio)
    grids = list(bGrid=bGrid, fGrid=fGrid)
    range = 1
    params = checkParams(list(numSensors=0))
}

#Tests the sumGrid function and all of its bias options
TestUtility.sumGrid <- function () {
    reset()
    s1 = matrix(c(16,27,33,39,28,16,27,33,39,28),
            nrow=r, 
            ncol=c)
    s2 = matrix(c(1.750, 2.375, 2.375, 2.375, 1.750, 1.750, 2.375, 2.375, 2.375, 1.750),
            nrow=r, 
            ncol=c)
    s3 = matrix(c(5.500, 9.125, 11.500, 13.875, 11.250, 8.000, 12.250, 14.625, 17.0, 13.75),
            nrow=r, 
            ncol=c)
    solGrid = list(s1=s1, s2=s2, s3=s3)
    sumGrid = {}
    
    for( i in 1:length(solGrid)) {
        sumGrid[[i]] = sumGrid(grids, range, i, params)$sumGrid
        
        if (isTRUE(all.equal(solGrid[[i]], sumGrid[[i]]))) {
            print(sprintf("[SumGrid:bias %g]: Pass",i))
        }
        else {
            print(sprintf("[SumGrid%g]: FAIL",i))
            print("solGrid:")
            print(solGrid[[i]])
            print("result:")
            print(sumGrid[[i]])
        }
    }

}

# Tests the zeroOut function and the getArea function
TestUtility.zeroOut <- function () {
    reset()
    dims = dim(fGrid)
    loc = list(r=dims[1], c=dims[2])
    value = 0
    # zeroing out the grid around the bottom right corner
    # should result in the bottom two cells being '0'
    solGrid = matrix(c(1,2,3,0,0,6,7,8,0,0),
            nrow=r, 
            ncol=c)
    
    fGrid = zeroOut(fGrid, dims, loc, range, value)
    if (isTRUE(all.equal(fGrid, solGrid))) {
        print("[zeroOut: %s]: Pass")
    }
    else {
        print(sprintf("[zeroOut: %s]: FAIL",i))
        print("solGrid:")
        print(solGrid)
        print("result:")
        print(fGrid)
    }
}

TestUtility.supress.scale <- function() {
    tests = list(
        list(d=0, min=.25, max=.75, ans=.25),
        list(d=100, min=.25, max=.75, ans=.75),
        list(d=25, min=.25, max=.75, ans=.375),
        list(d=0, min=0, max=1, ans=0),
        list(d=100, min=0, max=1, ans=1),
        list(d=25, min=0, max=1, ans=.25)
    )
    supressionRange = 100
    params = {}
    
    for( test in tests) {
        val = supression.scale(test$d, supressionRange, test$min, 
                        test$max, params)
        if (val != test$ans) {
            print(sprintf("Error: [supress.scale] incorect value.  Expected %g, recieved %g",test$ans, val ))
        }
    }
}

TestUtility.getCells <- function() {
    reset()
    startCell = list(r=1,c=1)
    endCell = list(r=5,c=2)
    cells = (getCells(startCell, endCell))
    points = list(  list(x=1,y=2),
                    list(x=1,y=3),
                    list(x=2,y=3),
                    list(x=2,y=4),
                    list(x=2,y=5))
    if (dim(cells)[1] > 5) {
        print("[getCells]: FAIL: too many results, duplicates exist!")
    }
    for (point in points) {
        if(dim(subset(cells, x==point$x & y==point$y))[1] != 1) {
            print(sprintf("[getCells]: FAIL: (%g,%g) missing or duplicate",point$x,point$y))
        }
    }
    print("[getCells]: Pass")
}


TestUtility.detect <- function () {
    reset()
    detect(bGrid, sensorPos, tagPos, fcn, params, debug=FALSE)
}
TestUtility.sumGrid()
#TestUtility.zeroOut()
TestUtility.getCells()
TestUtility.supress.scale()
print("Success! All tests passed!")