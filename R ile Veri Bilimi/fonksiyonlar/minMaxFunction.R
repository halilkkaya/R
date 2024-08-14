minMaxFun <- function(x,cr){
  if (cr==1) {
    mean(apply(x,1,min )/ apply(x,1,max)) 
  }else if (cr==2) {
    mean(apply(x,2,min )/ apply(x,2,max))
  }else {
    stop("cr can only take 1 or 2")
  }
}