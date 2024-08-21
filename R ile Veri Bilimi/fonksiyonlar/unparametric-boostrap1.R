
# zeynel cebeciden alintidir

calc.mean <- function(x, indices){
  if(is.vector(x)) x <- as.data.frame(x)
  if(missing(indices)) indices <- 1:nrow(x)
  xmean <- mean(x[indices,1])
  return(xmean)
}


# Parametrik olmayan bootstrap fonksiyonu 
bs.nonpar1 <- function(dset, statistic, R=2000){
  if(!is.data.frame(dset) | !is.matrix(dset)) 
    dset <- as.data.frame(dset)
  n <- nrow(dset)  # orneklem Buyuklugu
  # Orijinal orneklem kestirimleri
  tetahat <- statistic(dset, indices=1:n) 
  nstats <- length(tetahat) # istatistik sayisi
  # Bootstrap kestirimleri matrisi
  tetastar <- matrix(NA, nrow=R, ncol=nstats)
  for (i in 1:R){
    indices <- sample(1:n, replace=TRUE)
    tetastar[i,] <- statistic(dset,indices=indices)
  }
  return(list(t0=tetahat, t=tetastar, data=dset,
              statistic=statistic, R=R, call=match.call()))
}
