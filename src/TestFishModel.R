rm(list=ls())
source("FishModel.R")
source("Utility.R")
## Use a non-square grid to ensure that columns and rows
## are being correctly referenced
r=4
c=5
bGrid = -matrix(c(1,1,0,1,2,3,3,3,4,5,5,4,3,2,0,0,2,2,1,1), 
        nrow=r, 
        ncol=c) 
cellRatio = 1
bGrid = list(bGrid=bGrid, cellRatio=cellRatio)
params = checkParams(list(numSensors=0))
dims = dim(bGrid$bGrid)
if(!('x' %in% names(bGrid))) bGrid$x <- seq(0,1,length=dims[2])
if(!('y' %in% names(bGrid))) bGrid$y <- seq(0,1,length=dims[1])

check <- function(typ,fGrid,sol){
    if(isTRUE(all.equal(fGrid,sol,tolerance=2e-7))){
        print(paste("[fish:",typ,"]: Pass"))
        return(TRUE)
    }else{
        print(paste("[fish:",typ,"]: FAIL"))
        print("solGrid:")
        print(sol)
        print("result:")
        print(fGrid)
        return(FALSE)
    }
}

TestFishModel.fish <- function(){
    ch <- rep(FALSE,3)
    ## RW
    typ <- "RW"
    fGrid <- fish(params, bGrid)
    sol <- matrix(c(0.05882353, 0.05882353, 0.05882353, 0.05882353, 0.05882353,0.05882353, 0.05882353, 0.05882353, 0.05882353, 0.05882353,0.00000000, 0.05882353, 0.05882353, 0.00000000, 0.05882353,0.05882353, 0.05882353, 0.05882353, 0.00000000, 0.05882353),nrow=4,ncol=5,byrow=TRUE)
    ch[1] <- check(typ,fGrid,sol)

    ## OU
    typ <- "OU"
    params$fishmodel <- "ou"
    params$mu <- c(0.5,0.5)
    params$msd <- 0.7
    params$dt <- 1
    params$B <- matrix(c(1,0,0,1),2,2)
    fGrid <- fish(params, bGrid)
    sol <- matrix(c(0.03494469, 0.05123484, 0.05820497, 0.05123484, 0.03494469, 0.05499715, 0.08063513, 0.09160496, 0.08063513, 0.05499715, 0.00000000, 0.08063513, 0.09160496, 0.00000000, 0.05499715, 0.03494469, 0.05123484, 0.05820497, 0.00000000, 0.03494469),nrow=4,ncol=5,byrow=TRUE)
    ch[2] <- check(typ,fGrid,sol)    

    ## Vertical habitat
    typ <- "Vertical habitat"
    params$mindepth <- -0.5
    params$maxdepth <- -3.5
    fGrid <- fish(params, bGrid)
    sol <- matrix(c(0.04989390, 0.0731529,    0, 0.0731529, 0.04989390, 0.07852472, 0.1151305,    0, 0.1151305, 0.07852472, 0.00000000, 0.1151305,    0, 0.0000000, 0.07852472, 0.04989390, 0.0731529,    0, 0.0000000, 0.04989390),nrow=4,ncol=5,byrow=TRUE)
    ch[3] <- check(typ,fGrid,sol)

    return(ch)
}

ch.fish <- TestFishModel.fish()
if(all(ch.fish)) print("Success! All tests passed!")
