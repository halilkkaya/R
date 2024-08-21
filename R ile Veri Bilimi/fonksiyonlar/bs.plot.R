bs.plot <- function(bootres, index=1, gayontem="yuzde",
                    conf.level=0.95){
  opar <- par(mfrow=c(3,2))
  if(!require(vioplot, quietly=TRUE)){
    install.packages("vioplot"); 
    require(vioplot, quietly=TRUE)}
  bsonuc <- bs.sonuc(bootres, index)
  # Grafik 1: Bootstrap kestirimleri (t*) kutu-biyik grafigi
  boxplot(bsonuc$sbst, col=3, bg=3, pch=21,
          ylab="t*", main="t* Kutu-biyik grafigi")
  # Grafik 2: Bootstrap kestirimleri (t*) keman grafigi
  vioplot::vioplot(bsonuc$sbst, col="gray90", 
                   rectCol=3, colMed=1, xaxt="n", ylab="t*",
                   main="t* Keman Grafigi", names=c(" "))
  # Grafik 3: Bootstrap kestirimleri (t*) QQ grafigi
  qqnorm(bsonuc$sbst, col=3, bg="gray90", pch=21, cex=1.1, 
         main="t* Norm QQ Grafigi", xlab="Teorik Kantiller",
         ylab="Orneklem Kantilleri")
  qqline(bsonuc$sbst, col=1, lwd=2) 
  # Grafik 4: Bootstrap kestirimleri (t*) ECDF grafigi
  plot(ecdf(bsonuc$sbst), lwd=1, xlab="t*", ylab="Fn(t*)", 
       main="t* Ampirik CDF")
  # Grafik 5: Bootstrap kestirimleri (t*) simetri grafigi
  plot(bsonuc$sbst, type="l", lwd=1, col=1, 
       xlab="Sira#", ylab="t*", 
       main="t* kestirimleri simetri grafigi")
  abline(h= bsonuc$est, col=2, lty=2)
  abline(h= bsonuc$bsest, col=4, lty=3)
  # Grafik 6: Bootstrap kestirimleri (t) R'ye g??re degisim grafigi
  plot(bootres$t[,index], type="l", lwd=1, col=3, 
       xlab="R", ylab="t*", main="Tekrarlara g??re t* kestirimleri")
  abline(h= bsonuc$est, col=2, lty=2)
  abline(h= bsonuc$bsest, col=4, lty=3)
  par(opar)
}

bs.sonuc <- function(bootres, index=1){
  est <- bootres$t0[index]  # ??statistik kestirimi
  sbst <- sort(bootres$t[,index]) #S??ral?? bootstrap kestirimleri 
  if(!is.null(bootres$tvals)) 
    tvals <- bootres$tvals[,index] else tvals <- NULL
  bsest <- mean(sbst) #Bootstrap kestirimleri ortalamas??
  se <- sd(sbst)  # Standart hata kestirimi
  bias <- bsest-est # Yanlilik kestirimi
  mse <- mean((sbst-est)^2)  # Hata kareler ortalamasi (MSE)
  R <- length(sbst)
  return(list(est=est, bsest=bsest, bias=bias, se=se, mse=mse,  
              sbst=sbst, tvals=tvals, R=R))
}
