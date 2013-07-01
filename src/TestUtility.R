source("Utility.R")

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

# Tests the zeroOut function
TestUtility.zeroOut <- function () {
    reset()
    dims = dim(fGrid)
    loc = list(r=dims[1], c=dims[2])
    value = 0
    solGrid = matrix(c(1,2,3,0,0,6,7,8,0,0),
            nrow=r, 
            ncol=c)
    
    print("OK")
    fGrid = zeroOut(fGrid, dims, loc, range, value)
    print("OK")
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

TestUtility.sumGrid()
TestUtility.zeroOut()
print("Success!")