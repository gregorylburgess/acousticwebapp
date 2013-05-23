source('~/work/R/Filled.legend.R')
source('~/work/R/Filled.contour3.R')

sub2ind <- function(sub,dim){
  if(length(dim)==3) ind <- (sub[3]-1)*dim[1]*dim[2] + (sub[2]-1)*(dim[1]) + sub[1]
  if(length(dim)==2) ind <- (sub[2]-1)*(dim[1]) + sub[1]
  ind
}

ind2sub <- function(ind,dim){
  ndim <- length(dim)
  N <- length(ind)
  sub <- array(0,c(N,ndim))
  if(ndim == 3){
    sub[,3] <- ceiling(ind/(dim[1]*dim[2]))
    ind <- ind - (sub[,3]-1)*dim[1]*dim[2]
  }
  sub[,2] <- ceiling(ind/dim[1])
  sub[,1] <- ind - dim[1]*(sub[,2]-1)
  sub
}
repmat <- function(X,m,n){
mx = dim(X)[1]
nx = dim(X)[2]
matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}

meshgrid <- function(x,y){
Y <- repmat(as.matrix(y),1,length(x))
X <- repmat(t(as.matrix(x)),length(y),1)
list(X=X,Y=Y)
}

date2time <- function(date){
  days  <- as.numeric(strftime(date,format='%j'))
  hours <- as.numeric(strftime(date,format='%H'))
  mins  <- as.numeric(strftime(date,format='%M'))
  time <- days + hours/24 + mins/(24*60)
  time
}

logit <- function(p) log(p/(1-p))
invlogit <- function(a) 1/(1+exp(-a))

glogit <- function(p,xl=0,xu=1) log((p-xl)/(xu-p))
invglogit <- function(a,xl=0,xu=1) (xu*exp(a)+xl)/(1+exp(a))

normalize <- function(distr){
  size <- dim(distr)
  rd <- as.vector(distr)
  rd <- rd/sum(rd)
  rds <- sort(rd,index.return=TRUE)
  rdsc <- cumsum(rds$x)
  rd[rds$ix] <- rdsc
  ##rdm <- matrix(rd,size[1],size[2])
  ##rdm[which.max(rdm)] <- 1
  ##rdm[which.min(rdm)] <- 0
  rd
}
