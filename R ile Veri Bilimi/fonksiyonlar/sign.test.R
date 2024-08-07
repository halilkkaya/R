# isaret testi fonksiyonum

sign.test <- function(x, y=NULL,mu=0,alternative="two.sided",conf.level=0.95){
      if (is.null(y)) { y <- mu}
      n <- sum((y-x)!=0)
      sneg <- sum(x<y)
      spos <- sum(x>y)
      s <- spos
      if (alternative=="greater") {
        s <- sneg
      }
      if (alternative=="two.sided") {
        s <- min(sneg,spos)
      }
      sonuc <- signtest.prob(n=n,s=s,alternative = "two.sided")
      return(sonuc)
}


