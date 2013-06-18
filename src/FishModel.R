# Models Fish Behavior.  Outputs a Fish grid with the same dimensions as the Bathymetry grid,
# containing the percentage of transmissions sent per cell.

fish <- function(params, bGrid) {
    if(!any(names(params)=='fishmodel')) params$fishmodel <- 'rw' ## Use random walk if not specified
    if(params$fishmodel == 'ou'){
        params$fishmodel <- 'rw'
        print('OU model not implemented, using RW instead.')
    }
    rows <- dim(bGrid)[1]
    cols <- dim(bGrid)[2]
    land <- bGrid>=0
    water <- bGrid<0
    nowater <- sum(water)
    fGrid <- matrix(0,rows,cols)
    switch(params$fishmodel,
            rw={ ## Random walk case
                print('rw')
                fGrid <- water/nowater
            },
            ou={ ## Ornstein-Uhlenbeck case
                print('ou')
                fGrid = {}
            }
    )
    
    return (fGrid)
}