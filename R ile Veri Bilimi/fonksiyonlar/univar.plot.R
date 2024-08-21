univar.plot <- function(x){
  par(mfrow=c(3,2))
  hist(x, col="gray90", prob=TRUE, ylab="Yogunluk",
       main="Histogram")
  lines(density(x), col=4)
  if(!require(vioplot, quietly=TRUE)){ 
    boxplot(x, col="gray90", bg=3, xlab="x",
            main="Kutu-biyik grafigi")
  }else{
    vioplot::vioplot(x, col="gray90", bg=3,
                     xlab="", main="Keman grafigi")
  }
  plot(x, col=1, bg="gray90", xlab="", main="Serpilme grafigi")
  abline(h=median(x), col=2)
  abline(h=mean(x), col=4)
  stripchart(x, col=1, bg="gray90", pch=21, cex=1, 
             xlab="x", main="serit grafik")
  plot.ecdf(x, col=1, bg="gray90", main="ECDF")
  curve(pnorm(x,mean(x), sd(x)), col=4, add=TRUE)
  qqnorm(x, datax=TRUE, pch=21, col=1, bg="gray90",
         main="Normal Q-Q grafigi")
  qqline(x, datax=TRUE, col=4,lwd=1) # Q-Q dogrusu
}
