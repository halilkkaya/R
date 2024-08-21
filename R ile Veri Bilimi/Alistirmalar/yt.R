x <- rnorm(50,mean = 12.5,sd = sqrt(1.5321))
x2 <- rweibull(50,shape = 11.5,scale = 12)

univar.plot(x = x)
univar.plot(x = x2)

shapiro.test(x)
shapiro.test(x2)

x <- as.data.frame(x)

a <- bs.nonpar1(dset = x2,statistic = calc.mean, R = 3000)
a
# burada normal dagilmayan bir veri setimiz icin boostrap yaptik

alfa <- 0.05
yuzdelik <- c(alfa/2,1-alfa/2)

ort <- mean(a$t)
ort
guvenara <- c(quantile(a$t[,1],probs = yuzdelik,names = F))
guvenara
# guven araliklarim budur
mean(x2)
# normal ortalamam da buydu
hist(x2)
# normal dagilmadigini da zaten biliyorduk bir daha baktik

wilcox.test(x2,mu = 10,conf.int = T)
# guven araligi 11.33-11.91
t.test(x2,mu = 10)
# guven araligi 11.21-11.87
guvenara
# guven araligi 11.19-11.85


# bu 3 teste gore guven araliklarim bunlardir
# boostrap bir orneklemeyi R kez yeniden ornekler ve ortalamanin guven araligini bulur


install.packages("GGally")
library(GGally)
bs.plot(a)

ggpairs(iris,columns = 1:4,aes(color = Species,alpha = 0.5),
          lower = list(continuous = "smooth"))

## cok iyi grafikkkkkkkk


install.packages("acepack")
library(acepack)

argmax <- ace(x,y)
acecor <- cor(argmax$tx,argmax$ty)[1]
acercor

# ister dogrusal olsun ister olmasin her bir veri setinde korelasyonu olcen cok iyi bir fonksiyon
# cok onerdi hoca kullllaannnnn


install.packages("earth")
library(earth)

