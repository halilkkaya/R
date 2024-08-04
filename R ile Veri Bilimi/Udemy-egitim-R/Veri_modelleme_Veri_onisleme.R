##### min max normalizasyon islemi ######

# verileri 0-1 arasina sikistiriyoruz

(x - min(x))/ (max(x)- min(x))
# min max normalizasyon formulum
# indisteki ilk sayiyla mininimum sayiyi cikariyor sonra onu maximum sayi ve minimum sayi
# farkina boluyor. formul budur.
# mesela c(2,3,1,5,6,7) x=2, min(1), max(7)

b <- c(2,3,1,5,6,7)
c <- numeric(length = 6)
for (i in 1:length(b)) {
  
  res <- (b[i] - min(b))/(max(b)-min(b))
  c[i] <- res
}
c
minMaxNorm(b)
# bi deneme yaptim ayni sonuclari aldim fonksiyonu asagida olusturmustuk

minMaxNorm <- function(x){
  a <- (x - min(x))/ (max(x)- min(x))
  return(a)
}
minMaxNorm(df$cdur)
# 0-1 arasina normalize islemi yaptik

min(minMaxNorm(df$cdur))
# 0
max(minMaxNorm(df$cdur))
# 1
# 0 ile1 arasina yerlestirdik yani

df$cdur <- minMaxNorm(df$cdur)
df$vdur <- minMaxNorm(df$vdur)
df$wordfreq<- minMaxNorm(df$wordfreq)
# hepsini 0-1 arasina yerlestirdik
# 100 ile carparsak 0-100 arasina koyariz
df

df$cdur <- minMaxNorm(df$cdur)*100
df$vdur <- minMaxNorm(df$vdur)*100
df$wordfreq<- minMaxNorm(df$wordfreq)*100
# 0-100 arasina aldik


##### scaling (olceklendirme- standartlastirma) islemi #####

?scale 
# x numeric matrix 
# center ortalamsi
# scale standart sapmasi ikisi de logical deger true dersek verdigimiz
# verilerdeki o verileri kullancak

scale(df$vdur,center = T,scale = T)
# standartlastirilmis verilerimiz
# 
mean(df$vdur)
sd(df$vdur)
# scale fonk icindeki verilerle ayniymis

# yani verimizin ortalama degerine ve standart sapma degerine gore 
# standartlastirma islemi yaptigini soyleyebiliriz

scale(df$vdur,center = 50,scale = 5)
# centeri 50 scalei 5 olan standartlastirilmis verileri olusturduk

# neden bu sekilde fonk verildi peki
# modelimizi yaptik diyelim modelimizdeki degiskenler
# bu sekilde scale yapilmis degiskenlerdir
# ve ardindan bi veri geldi ve veriden tahmin yapicaz
# veriyi once scaleden gecirmek gerekiyor. bu yuzden verinin
# kendi ortalama standart sapmasini kullanilmasi gerekiyor




par(mfrow=c(1,2))
hist(df$vdur)
hist(scale(df$vdur))

# ufak tefek degisiklikler var ama bu veriyi degistirmek degildir
# sadece standartlastirmadir. veriler degismez


###### aykiri deger tespit yontemleri ######

# boxplot yontemiyle

library(rstatix)


identify_outliers(as.data.frame(AirbnbNewyork$price))
## bayyaa cok veri verdi bazilari ekstrem hatta o da yaziyo

out <- identify_outliers(as.data.frame(AirbnbNewyork$price))

names(out)

min(out$`AirbnbNewyork$price`)
# min deger 335

max(out$`AirbnbNewyork$price`)
# 10000 max deger

min(AirbnbNewyork$price)
max(AirbnbNewyork$price)
# direkt tablodaki min ve max degerlerim

# min degerimiz normal tablomuzda 0 geld
# out verisinde de 335di yani aslinda 335 altindakiler normal veri 
# ustundekiler de aykiri deger mantigini kullaniyoruz

ids <- which(out$is.extreme== TRUE)
# ekstrem degerlerin indislerini aldik
ekstrem <- out[ids,"AirbnbNewyork$price"]
# ekstrem degerlerim

min(ekstrem)
# 495
max(ekstrem)
# 10000

# ekstrem deger araligimdir

library(tidyverse)

df_clean <- AirbnbNewyork %>% filter(price<335)
# aykiri degerleri attik

p <- numeric(length = 100)
for (i in 1:100) {
  deger <- sample(df_clean$price,size=100)
  test <- shapiro.test(deger)
  pvalue <- test$p.value
  p[i] <- pvalue
}
mean(p)
# normallik testi yaptik normal cikmadi
# p degerim 0a cok yakin

hist(df_clean$price)
# harbiden saga carpik yani veriler solda togunluklu bi tablomuz var
# neyse burasi oylesine deneme icin yaptim 

## z, t, chi square skorlarina gore aykiri deger kontrolu

install.packages("outliers")
library(outliers)


setwd("C:/Users/halil ibrahim kaya/Desktop")
load(file = ".Rdata")
# calisma dosyami masaustune kaydetmistim ordan devam icin loading islemi yaptim



?scores

View(airquality)
scores(airquality$Ozone, type = "z", prob = 0.95)
# NA sonuclari geldi. veri setinde cok NA deger vardi onlari aykiri verdi
# na.omit ile NA'lari atip ole bakalim

a <- scores(na.omit(airquality$Ozone), type = "z", prob = 0.95)
# TRUE FALSE seklinde hangi degerlerin aykiri oldugunu verdi

b <- which(a=="TRUE")
# TRUE yazanlari yani aykiri deger olanlarin indislerini ald

na.omit(airquality$Ozone)[b]
# a atamasini yaparken NA degerleri silip aykiri degerleri oyle bulduk
# o yuzden na.omit diyerek o indisleri aramamiz gerekki indis
# kaymasi olmasin
# verilere bakiyoruz ve 97den sonrasi aykiri deger gibi duruyor
# yine de bunu fonksiyonlarla yapalim
min(na.omit(airquality$Ozone)[b])
# 97 diyor

max(na.omit(airquality$Ozone)[b])
# 168 diyor

# yani 97den buyukleri atacagiz

par(mfrow=c(2,1))
hist(airquality$Ozone)
hist(na.omit(airquality$Ozone)[-b])
# grafik olarak da inceleyelim dedik
# grafikleri alt alta baktik 



library(tidyverse)

df_clean <- airquality %>% filter(Ozone<97) 

# temiz verileri aldik


# t ile yapma
View(airquality)
scores(airquality$Ozone, type = "t", prob = 0.95)
# tine na.omit islemi yapalim
a <- scores(na.omit(airquality$Ozone), type = "t", prob = 0.95)
b <- which(a=="TRUE")
na.omit(airquality$Ozone)[b]
# ayni sonuclari verdi yine



par(mfrow=c(2,1))
hist(airquality$Ozone)
hist(na.omit(airquality$Ozone)[-b])
# grafikleri de ayni


# ki square ile yapma
View(airquality)
scores(airquality$Ozone, type = "chisq", prob = 0.95)
# yine na.omit islemi yapalim
a <- scores(na.omit(airquality$Ozone), type = "chisq", prob = 0.95)
b <- which(a==TRUE)
na.omit(airquality$Ozone)[b]
# daha az sonuc en kucuk degeri 108 olarak aldi diger tarata 97ydi
# sebebi chi sq saga carpik veriler verdigi icin biraz daha
# verilerin soldakilerini aykiri saymadi


# iqr ile yapma
View(airquality)
scores(airquality$Ozone, type = "iqr", prob = 0.95)
# yine na.omit islemi yapalim
a <- scores(na.omit(airquality$Ozone), type = "iqr", prob = 0.95)
b <- which(a==TRUE)
na.omit(airquality$Ozone)[b]
# hicbi aykirid eger bulmadi bu


# mad gore yapma medyan yani
View(airquality)
scores(airquality$Ozone, type = "mad", prob = 0.95)
# yine na.omit islemi yapalim
a <- scores(na.omit(airquality$Ozone), type = "mad", prob = 0.95)
b <- which(a==TRUE)
na.omit(airquality$Ozone)[b]
# mad yani medyana gore en kucuk aykiri deger 77 oldu
# daha fazla deger verdi


par(mfrow=c(2,1))
hist(airquality$Ozone)
hist(na.omit(airquality$Ozone)[-b])
# aykiri degerleri cikarinca daha da normale dondu ama yine de saga carpiklik var





###### mhalanobis fistance ile cok degiskenli aykiri deger kontrolu#####

getwd()
setwd("C:/Users/halil ibrahim kaya/Desktop")

load(file = ".RData")
# yine kayit dosyami getirdim

library(ggplot2)
library(car)
# gerekli kutuphaneleri getirdim

View(airquality)

# ozone ve temp kullanip wind'i tahmin etmeye calisacagiz  

fig <- ggplot(airquality, aes(x=Ozone, y= Temp)) +
        geom_point(size=2)+
        xlab("ozone degerleri")+ ylab("temp degerleri")
fig
# na degerleri icin uyari veriyor ama grafik
# yine de verildi
# lineer iliski var gibi duruyor yani 
# biri artinca digeri de artiyo ama aykiri degerler bunu bzouyor gibi duruyo
# ortalamaya bi elips ciziyo ve aykiri degerleri buluyor
# cok degiskenli veriler icin boyle yapiliyor


X <- na.omit(airquality[c("Ozone","Temp")])
View(X)
# NAlari attik

air.center <- colMeans(X)
air.center
# ortalamalarini aldik ozone 42, temp 77

air.cov <- cov(X)
air.cov
# matris verdi temp-ozone kovaryansi 218 cikti
# yani birlikte olan degisim katsayilari

# alipsin yari capini hesaplamamiz lazim bunu da kikare dagilimindan bulacagiz

rad <- sqrt(qchisq(0.95, df = 2))
# df kac degisken varsa o kadar sayi gircez
# 0.95 de olasilik degerim
# ki kare testi oldugu icin karekok alinmis halini almamiz gerek
rad
# 2.44 verdi yani yaricapim 2.44





elipse <- ellipse(center = air.center,shape = air.cov, radius = rad,
        segments = 100, draw = FALSE)

elipse
# ellipse degerlerim var. x ve y koordinatlarimi verdi bunlari ggplot tablosuna 
# ekleyecegiz 

colnames(elipse) <- colnames(X)
# sutun adlarini degistirdik

fig <- fig + geom_polygon(data = elipse, color = "orange",
                   fill = "orange",
                   alpha = 0.3) +
          geom_point(aes(x= air.center[1],y= air.center[2]),
           size = 5, color = "blue")

fig
# elipsi cizdik simdi aciklayayim
#  fig ile halihazirda yukarida olusturdugumuz grafigi cagirdik
# + operatoru ile ustune geom_polygon fonksiyonu ile elips cizdirdik
# geom_polygon icersinde data elips degerlerimi yani kopordinatlarimi
# color rengimii fill icerisinin rengini yani dolguyu diyelim
# alpha da dolgunun seffafligini belirtiyor
# geom_point ile iki degiskenin ortalamalarinin kesitigi noktayi cizdik
# bu da bizim center yani merkez noktamiz oldu.
# aes icerisine x ve y degerlerine ortalamalari ataadik
# noktanin buyuklugunu de size ile belirledik
# color ile noktaya renk verdik
# ellipse icerisinde olanlar normal disinda olanlar ise aykiri degerler
# biz bunu ki kareye gore cizdik yani 0.95 ile yaptik kesinligi daha da arttirisak
# nawsil olur onu incelkeyelim

fig <- ggplot(airquality, aes(x=Ozone, y=Temp)) +
  geom_point(size=2)+
  xlab("ozone degerleri")+ ylab("temp degerleri")
fig
X <- na.omit(airquality[c("Ozone","Temp")])
View(X)


air.center <- colMeans(X)
air.center
# ortalamalarini aldik ozone 42, temp 77

air.cov <- cov(X)
air.cov
rad <- sqrt(qchisq(0.99, df = 2))

elipse <- ellipse(center = air.center,shape = air.cov, radius = rad,
                  segments = 100, draw = FALSE)
elipse
colnames(elipse) <- colnames(X)

fig <- fig + geom_polygon(data = elipse, color = "orange",
                          fill = "orange",
                          alpha = 0.3) +
              geom_point(aes(x= air.center[1],y= air.center[2]),
             size = 5, color = "blue")

fig
# 0.99 ile ikinc, olusturdugum daha da cok alan aldi
# bir de 0.80 yapalim
rad <- sqrt(qchisq(0.80, df = 2))

elipse <- ellipse(center = air.center,shape = air.cov, radius = rad,
                  segments = 100, draw = FALSE)
elipse
colnames(elipse) <- colnames(X)

fig <- fig + geom_polygon(data = elipse, color = "orange",
                          fill = "orange",
                          alpha = 0.3) +
  geom_point(aes(x= air.center[1],y= air.center[2]),
             size = 5, color = "blue")

fig
# daha kucuk bi alani aldi yani daha fazla aykiri deger verdi
# dig degerini hic degismedigimiz icin hep ic ice cizdi
# genel olarak 0.95 kullan ama
# aykiri degerleri bulalim



dist <- mahalanobis(x = X ,center = air.center , cov = air.cov)
dist
# tablo formatinda gozlem indislerine gore merkezden uzakliklarini verdi
# ki kare kullanip cutoff yani kesim noktasi dgeri bulalim


cutoff <- qchisq(p=0.95, df=2)
cutoff

ids <- which(dist>cutoff)
ids
# aykiri degerlerimin indekslerini noyle buldum

names(ids)
# indislerimi veriyor

X[ids,]
# aykiri degerlerimi buldum mahlanobise gore bulduk








































