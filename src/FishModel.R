# Models Fish Behavior.  Outputs a Fish grid with the same dimensions as the Bathymetry grid,
# containing the percentage of transmissions sent per cell.

fish <- function(params, bGrid) {
    if(!('fishmodel' %in% names(params))) params$fishmodel <- 'rw' ## Use random walk if not specified
    rows <- dim(bGrid$bGrid)[1]
    cols <- dim(bGrid$bGrid)[2]
    land <- bGrid$bGrid>=0
    switch(params$fishmodel,
            rw={ ## Random walk case
                print('rw')
                if('dp' %in% names(params)){
                    print('RW: Using depth preference to calculate fGrid')
                    fGrid <- dnorm(bGrid$bGrid,params$dp,params$dpsd)
                }else{
                    fGrid <- matrix(1,rows,cols)
                }
            },
            ou={ ## Ornstein-Uhlenbeck case
                print('ou')
                library(mvtnorm)
                sigma <- params$msd/sqrt(params$dt)
                hrCov <- 0.5*sigma^2*solve(params$B)
                X <- matrix(rep(bGrid$x,rows),rows,cols,byrow=TRUE)
                Y <- matrix(rep(bGrid$y,cols),rows,cols,byrow=FALSE)
                XY <- cbind(as.vector(X),as.vector(Y))
                hrVals <- dmvnorm(XY,params$mu,hrCov)
                fGrid <- matrix(hrVals,rows,cols,byrow=FALSE)
                if('dp' %in% names(params)){
                    print('OU: Using depth preference to calculate fGrid')
                    dpGrid <- dnorm(bGrid$bGrid,params$dp,params$dpsd)
                    fGrid <- fGrid * dpGrid
                }
            }
    )
    fGrid[land] <- 0 ## Set land areas to zero
    fGrid <- fGrid/sum(fGrid) ## Make sure fGrid sums to one
    
    return (fGrid)
}
