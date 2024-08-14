zcetvel <- function(){
  options(digits = 4)
  z <- seq(0, 3.69, by = 0.01)
  prz <- pnorm(z)
  zv <- matrix(prz, ncol = 10, byrow = T)
  rownames(zv) = seq(0, 3.6, by = 0.1)
  colnames(zv) = seq(0,0.09, by = 0.01)
  return(zv)
}
