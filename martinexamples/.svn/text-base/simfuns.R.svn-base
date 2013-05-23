
pw2pn <- function(parvec){
  sd <- exp(parvec)
  sd
}

pn2pw <- function(sd){
  parvec <- log(sd)
  parvec
}

detect.fun <- function(x,r=0.25,sd=0.05,fun='t'){
  ## Calculate detection probability
  if(fun=='t') ret <- ifelse(x<r,dt(x/sd,df=1)/dt(0,df=1),0)
  if(fun=='gauss') ret <- ifelse(x<r,dnorm(x/sd,0,1)/dnorm(0,0,1),0)
  ret
}

dist.fun <- function(x1,y1,x2,y2) sqrt((x1-x2)^2 + (y1-y2)^2)

write.site <- function(sitefile,r){
  cat('# Receiver setup for site 1\n',file=sitefile)
  cat('# cutoffrange\n',file=sitefile,append=TRUE)
  cat(r$cutoffrange,'\n',file=sitefile,append=TRUE)
  cat('# SD\n',file=sitefile,append=TRUE)
  cat(r$SD,'\n',file=sitefile,append=TRUE)
  cat('# dfun\n',file=sitefile,append=TRUE)
  cat(r$dfun,'\n',file=sitefile,append=TRUE)
  cat('# x\n',file=sitefile,append=TRUE)
  cat(r$x,'\n',file=sitefile,append=TRUE)
  cat('# y\n',file=sitefile,append=TRUE)
  cat(r$y,'\n',file=sitefile,append=TRUE)
}

read.site <- function(sitefile){
  ## Read site file
  r <- list()
  r$cutoffrange <- as.numeric(read.table(sitefile,skip=2,nrows=1))
  r$SD <- as.numeric(read.table(sitefile,skip=4,nrows=1))
  r$dfun <- as.character(read.table(sitefile,skip=6,nrows=1)$V1)
  r$x <- as.numeric(read.table(sitefile,skip=8,nrows=1))
  r$y <- as.numeric(read.table(sitefile,skip=10,nrows=1))
  r
}

write.datafile <- function(data,filename){
  ## Write a data file
  movefile <- paste(substring(filename,1,nchar(filename)-4),'Move.csv',sep='')
  write.table(data$mov,file=movefile,row.names=FALSE)
  cat('# Transmitter data\n',file=filename)
  cat('# Site file\n',file=filename,append=TRUE)
  cat(data$sitefile,'\n',file=filename,append=TRUE)
  cat('# dt\n',file=filename,append=TRUE)
  cat(data$dt,'\n',file=filename,append=TRUE)
  cat('# T (total time at liberty)\n',file=filename,append=TRUE)
  cat(data$T,'\n',file=filename,append=TRUE)
  cat('# Release x\n',file=filename,append=TRUE)
  cat(data$relx,'\n',file=filename,append=TRUE)
  cat('# Release y\n',file=filename,append=TRUE)
  cat(data$rely,'\n',file=filename,append=TRUE)
  cat('# Move model y\n',file=filename,append=TRUE)
  cat(data$move.model,'\n',file=filename,append=TRUE)
  cat('# True S\n',file=filename,append=TRUE)
  cat(data$S,'\n',file=filename,append=TRUE)
  cat('# Receiver data\n',file=filename,append=TRUE)
  write.table(data$data,file=filename,sep=',',row.names=FALSE,append=TRUE)
}

write.admbfile <- function(r,data,admbfile){
  ## --- Write a ADMB data file ---
  ## Write site data
  cat('# Receiver setup for ',r$sitename,'\n',file=admbfile)
  cat('# cutoffrange\n',file=admbfile,append=TRUE)
  cat(r$cutoffrange,'\n',file=admbfile,append=TRUE)
  cat('# SD\n',file=admbfile,append=TRUE)
  cat(r$SD,'\n',file=admbfile,append=TRUE)
  cat('# dfun\n',file=admbfile,append=TRUE)
  cat('# ',r$dfun,'\n',file=admbfile,append=TRUE)
  cat('# number of receivers\n',file=admbfile,append=TRUE)
  cat(length(r$x),'\n',file=admbfile,append=TRUE)
  cat('# x\n',file=admbfile,append=TRUE)
  cat(r$x,'\n',file=admbfile,append=TRUE)
  cat('# y\n',file=admbfile,append=TRUE)
  cat(r$y,'\n',file=admbfile,append=TRUE)
  ## Write receiver data
  cat('# Transmitter data\n',file=admbfile,append=TRUE)
  cat('# Site file\n',file=admbfile,append=TRUE)
  cat('# ',data$sitefile,'\n',file=admbfile,append=TRUE)
  cat('# dt\n',file=admbfile,append=TRUE)
  cat(data$dt,'\n',file=admbfile,append=TRUE)
  cat('# T (total time at liberty)\n',file=admbfile,append=TRUE)
  cat(data$T,'\n',file=admbfile,append=TRUE)
  cat('# Release x\n',file=admbfile,append=TRUE)
  cat(data$relx,'\n',file=admbfile,append=TRUE)
  cat('# Release y\n',file=admbfile,append=TRUE)
  cat(data$rely,'\n',file=admbfile,append=TRUE)
  cat('# Move model y\n',file=admbfile,append=TRUE)
  cat('# ',data$move.model,'\n',file=admbfile,append=TRUE)
  cat('# True S\n',file=admbfile,append=TRUE)
  cat(data$S,'\n',file=admbfile,append=TRUE)
  cat('# Number of observations\n',file=admbfile,append=TRUE)
  cat(length(data$data$rec),'\n',file=admbfile,append=TRUE)
  cat('# Reception receiver number\n',file=admbfile,append=TRUE)
  cat(data$data$rec,'\n',file=admbfile,append=TRUE)
  cat('# Reception times\n',file=admbfile,append=TRUE)
  cat(data$data$time,'\n',file=admbfile,append=TRUE)
}


read.datafile <- function(filename){
  ## Read a data file
  dat <- list()
  dat$sitefile <- as.character(read.table(filename,skip=2,nrows=1)$V1)
  dat$dt <- as.numeric(read.table(filename,skip=4,nrows=1)$V1)
  dat$T <- as.numeric(read.table(filename,skip=6,nrows=1)$V1)
  dat$relx <- as.numeric(read.table(filename,skip=8,nrows=1)$V1)
  dat$rely <- as.numeric(read.table(filename,skip=10,nrows=1)$V1)
  dat$move.model <- as.character(read.table(filename,skip=12,nrows=1)$V1)
  dat$S <- as.numeric(read.table(filename,skip=14,nrows=1)$V1)
  dat$data <- read.table(filename,skip=16,header=TRUE,sep=',')
  dat
}

maps <- function(ngx,ngy,nr,r,G){
  ## Distance maps
  dimap <- array(0,dim=c(ngy,ngx,nr))
  for(k in 1:nr) dimap[,,k] <- sqrt((G$X-r$x[k])^2+(G$Y-r$y[k])^2) ## Distance to receiver
  ##filled.contour(gy,gx,dimap[,,2],color.palette=rainbow,main=c('Distance map'),xlab='x',ylab='y',plot.axes = {axis(1); axis(2); points(r$x,r$y)})

  ## Detection maps
  demap <- array(0,dim=c(ngy,ngx,nr))
  for(k in 1:nr) demap[,,k] <- detect.fun(dimap[,,k],r=r$cutoffrange,sd=r$SD,fun=r$dfun)  ## Distance to receiver
  ##filled.contour(gy,gx,demap[,,2],color.palette=rainbow,main=c('Detection map'),xlab='x',ylab='y',plot.axes = {axis(1); axis(2); points(r$x,r$y)})

  ## Coverage map
  cover <- matrix(1,ngy,ngx)
  p <- rep(0,nr)
  for(k in 1:nr){
    cover <- cover * (1 - demap[,,k]) ## Probability of no detection
  }
  cover <- 1 - cover ## Detection probability at location
  ##filled.contour(gy,gx,cover,color.palette=rainbow,main=c('Coverage map showing probability of detection'),xlab='x',ylab='y',plot.axes = {axis(1); axis(2); points(r$x,r$y)})
  out <- list()
  out$dimap <- dimap
  out$demap <- demap
  out$cover <- cover
  out
}

data.lik <- function(gx,gy,dat,demap,nr){
  ## Calculate data likelihood
  ngx <- length(gx)
  ngy <- length(gy)
  posx <- which.min(abs(gx-dat$relx))
  posy <- which.min(abs(gy-dat$rely))
  rellik <- matrix(0,ngy,ngx)
  rellik[posy,posx] <- 1
  uniqtime <- unique(dat$data$time)
  pingrate <- 1/dat$dt # pings per second
  Np <- dat$T/dat$dt+1 # Number of pings (assuming a ping exactly when the fish is released)
  time <- seq(0,dat$T,length=Np) # Time vector in seconds

  nt <- length(time)
  nut <- length(uniqtime)
  datmat <- matrix(0,nut,nr)
  noping <- matrix(1,ngy,ngx)
  for(k in 1:nr) noping <- noping*(1-demap[,,k])

  ##data <- c(0,0,1,0,1) # Data sample, 1 = detected, 0 = not detected
  lik <- array(1,dim=c(nt,ngy,ngx))
  iu <- 0
  for(i in 1:nt){
    if(any(time[i]==uniqtime)){
      iu <- iu+1
      ind <- dat$data$rec[dat$data$time==uniqtime[iu]]
      datmat[iu,ind] <- 1
      for(k in 1:nr){
        if(datmat[iu,k]==1){
          lik[i,,] <- lik[i,,] * demap[,,k]
        }else{
          lik[i,,] <- lik[i,,] * (1-demap[,,k])
        }
      }
    }else{
      lik[i,,] <- noping
    }
  }
  lik[1,,] <- lik[1,,]*rellik ## Include information from release location
  lik[1,,] <- lik[1,,]/sum(lik[1,,]) ## Include information from release location
  lik
}

make.kern.1D <- function(sd,dx,limval=3e-5,maxsize=101,printmeansd=FALSE){
  ## Construct a convolution kernel
  ## Default maxsize corresponds to a range of [-4*sd,4*sd]. pnorm(-4,0,1)
  require(Matrix)
  if((maxsize %% 2) == 0) maxsize <- maxsize+1
  size <- maxsize
  sx <- 0.5*(size-1)
  hx <- seq(0,sx*dx,by=dx)
  x <- c(-hx[(sx+1):2],hx)

  D <- 0.5*(sd/dx)^2
  ## Generator, apologies for bad code
  Q <- matrix(0,maxsize,maxsize)
  Q[-1,-maxsize] <- Q[-1,-maxsize] + diag(rep(D,maxsize-1))
  Q <- Q+t(Q)
  Q <- Q - diag(apply(Q,1,sum))
  P <- expm(Q)
  k <- P[0.5*(maxsize+1),]
  
  ##tk <- pnorm(c(hx[1]-0.5*dx,hx+0.5*dx),0,sd=sd)
  ##k <- diff(tk)
  ##k <- c(k[(sx+1):2],k)
  mean <- sum(x*k)
  ##if(abs(mean)>1e-5){
  ##  warmsg <- paste('Mean value of kernel is significantly different from zero! mean:',mean)
  ##  warning(warmsg)
  ##}
  sdemp <- sqrt(sum((x-mean)^2*k))
  ##if(abs(sdemp-sd)/sd > 0.01){
  ##  warmsg <- paste('sd value of kernel is significantly different from the input value! sd input:',sd,'sd empirical:',sdemp)
  ##  warning(warmsg)
  ##}
  if(printmeansd){
    print(mean)
    print(sdemp)
  }
  ##print(cumsum(k))
  indlow <- which(cumsum(k)>limval)
  indhig <- which((1-cumsum(k)) > limval)
  indhig <- c(indhig,indhig[length(indhig)]+1)
  kout <- k[intersect(indlow,indhig)] # Remove elements below limval
  kout <- kout/sum(kout)
  ##print(length(k))
  if(length(kout)==maxsize){
    warmsg <- paste('Maxsize of convolution kernel is reached! Either increase maxsize or decrease upper limit for sd. Parameters: sd:',sd,'dx:',dx,'limval:',limval,'maxsize:',maxsize)
    warning(warmsg)
  }
  if(length(kout)==1){
    warmsg <- paste('Convolution kernel is of size 1. Artificially adding diffusion to reduce chance of error. You should possibly refine grid or increase lower limit for sd. Parameters: sd:',sd,'dx:',dx,'limval:',limval,'maxsize:',maxsize)
    warning(warmsg)
    kout <- c(limval,1-2*limval,limval)
  }
  kout
}

conv.1D <- function(fun,kern){
  ## Convolution in 1D
  lk <- length(kern)
  kern <- kern[lk:1]
  lk2 <- 0.5*(lk-1)
  lf <- length(fun)
  test <- convolve(fun,kern,type='o')
  test[(lk2+1):(lf+lk-lk2-1)]
}

conv.2D <- function(mat,kx,ky){
  ## Convolution in 2D
  dimmat <- dim(mat)
  matout <- matrix(0,dimmat[1],dimmat[2])
  for(i in 1:dimmat[1]) matout[i,] <- conv.1D(mat[i,],kx)
  for(i in 1:dimmat[2]) matout[,i] <- conv.1D(matout[,i],ky)
  matout
}


hmm.filter <- function(lik,sd,dx,dy,maxsize){
  ## Filter data to estimate locations and behaviour
  T <- dim(lik)[1]
  row <- dim(lik)[2]
  col <- dim(lik)[3]
  kx <- make.kern.1D(sd,dx,maxsize=maxsize,limval=1e-12)
  ky <- make.kern.1D(sd,dy,maxsize=maxsize,limval=1e-12)

  pred <- array(0,dim=c(T,row,col))
  phi  <- array(0,dim=c(T,row,col))
  ## Start in resident state at the known initial location
  phi[1,,]  <- lik[1,,]
  pred[1,,] <- lik[1,,]
  psi <- rep(0,T-1)
  ## Start filter interations
  for(t in 2:T){
    p1 <- phi[t-1,,]
    pred[t,,] <- conv.2D(p1,kx,ky)
    post <- pred[t,,]*lik[t,,]
    ##post <- post + 1e-12 ## Add a tiny number so program continues even in the case all probability mass dissappears (e.g. when optimizer picks a very small diffusion)
    psi[t-1] <- sum(as.vector(post))
    phi[t,,] <- post/(psi[t-1]+1e-15)
  }

  list(phi=phi,pred=pred,psi=psi)
}

hmm.smoother <- function(f,sd,dx,dy,maxsize){
  ## Smoothing the filtered estimates
  T <- dim(f$phi)[1]
  row <- dim(f$phi)[2]
  col <- dim(f$phi)[3]
  kx <- make.kern.1D(sd,dx,maxsize=maxsize)
  ky <- make.kern.1D(sd,dy,maxsize=maxsize)
  
  smooth <- array(0,dim=dim(f$phi))
  smooth[T,,] <- f$phi[T,,]
  for(t in T:2){
    RAT <- smooth[t,,]/(f$pred[t,,]+1e-15)
    post <- conv.2D(RAT,kx,ky)
    post <- post * f$phi[t-1,,]
    fac <- sum(as.vector(post))
    smooth[t-1,,] <- post/fac
  }
  smooth
}

neg.log.lik.fun <- function(logS,lik,dt,dx,dy,maxsize){
  ## Calculate the likelihood of the parameters
  ##S <- exp(logS)
  print(logS)
  S <- pw2pn(logS) ##S*sqrt(dt)
  sd <- S*sqrt(dt)
  ##print(sd)
  f <- hmm.filter(lik,sd,dx,dy,maxsize)
  nll <- -sum(log(f$psi))
  nll
}
