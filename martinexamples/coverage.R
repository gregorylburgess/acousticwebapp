## Script for plotting detection coverage
## 17.05.2013
rm(list=ls())
source('simfuns.R')
source('basicfuns.R')

## --- Receivers ---
r <- list()
r$cutoffrange <- 0.4
r$SD <- 0.1
r$dfun <- 't' # Detection function
## Locations
r$x <- c(0.25,0.25,0.75,0.75,0.5)
r$y <- c(0.25,0.75,0.25,0.75,0.5)
nr <- length(r$x)

## --- Grid ---
rx <- c(0,1) # range x
ry <- c(0,1) # range y
ngx <- 51
ngy <- 53
gx <- seq(rx[1],rx[2],length=ngx)
dx <- gx[2]-gx[1]
gy <- seq(ry[1],ry[2],length=ngy)
dy <- gy[2]-gy[1]
G <- meshgrid(gx,gy)

m <- maps(ngx,ngy,nr,r,G) ## Calculate distance maps, detections maps, coverage map


filled.contour(gy,gx,m$cover,color.palette=rainbow,main=c('Coverage map showing probability of detection'),xlab='x',ylab='y',plot.axes = {axis(1); axis(2); points(r$x,r$y)})
