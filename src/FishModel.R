# Models Fish Behavior.  Outputs a Fish Grid with the same dimensions as the Bathymetry Grid,
# containing the number of transmissions sent per cell.

	fish <- function(params, BGrid) {
          if(!any(names(params)=='fishmodel')) params$fishmodel <- 'rw' ## Use random walk if not specified

          rows <- dim(BGrid)[1]
          cols <- dim(BGrid)[2]
          land <- BGrid=>0
          FGrid <- matrix(0,rows,cols)
          switch(params$fishmodel,
                 rw={ ## Random walk case
                   print('rw')
                 },
                 ou={ ## Ornstein-Uhlenbeck case
                   print('ou')
                 }
                 )
          
          FGrid = {}
          return (FGrid)
	}
