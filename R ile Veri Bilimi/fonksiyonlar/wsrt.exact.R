wsrt.exact <- function(x, y=NULL, mu =0,alternative="two.sided",conf.level=0.95){
  if (!is.null(y)) {
    d <- x-y
    ad <- abs(d-mu)
  }else{
    d <- x-mu
    ad <- abs(d)
  }
  
  n <- length(d)
  rnk <- ad
  zidx <- which(ad!=0)
  rx <- rank(ad[zidx])
  rnk[zidx] <- rx
  rnk[-zidx] <- NA
  if (!is.null(y)) {
    atable <- cbind(x,y,d,sgn=sign(d),rnk)
  }else{
    atable <- cbind(x,d,sgn=sign(d),rnk)
  }
  wneg <- sum(rnk[d<0])
  wpos <- sum(rnk[d>0])
  n0 <- length(rnk[d==0])
  n <- length(rnk[d!=0])
  if (alternative=="less") {
    w <- wpos
    p.value <- psignrank(w,n)
  }else if (alternative=="greater") {
    w <- wneg
    p.value <- 1-psignrank(w,n,lower.tail = F)
  }else if (alternative=="two.sided") {
    w <- min(wneg,wpos)
    p.value <- 2*min(psignrank(w,n),1- psignrank(w,n,lower.tail = F))
  }
  walmat <- outer(d,d,"+")/2
  walvec <- walmat[lower.tri(walmat, diag = T)]
  n.wal <- length(walvec)
  est.med <- median(walvec)
  sig.level <- 1 - conf.level
  k <- qsignrank(sig.level/2,n)
  if (k==0) {
    k <- k+1
  }
  achieved.CI <- 1 - 2 * psignrank(k-1,n)
  pop.med.CI <- c(walvec[k],walvec[n.wal+1-k])
  sonuc <- list(atable=atable, wneg=wneg,wpos=wpos,n=n,n0=n0,w=w,
                est.med=est.med,achieved.CI=achieved.CI, pop.med.CI=pop.med.CI,
                p.value=p.value)
  return(sonuc)
  
}











