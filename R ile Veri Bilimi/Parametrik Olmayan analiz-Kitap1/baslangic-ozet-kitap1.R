install.packages("KernSmooth")
install.packages("MASS")
install.packages("Matrix")
install.packages("boot")
install.packages("class")
install.packages("cluster")
install.packages("codetools")
install.packages("foreign")
install.packages("lattice")
install.packages("mgcv")
install.packages("nlme")
install.packages("nnet")
install.packages("rpart")
install.packages("spatial")
install.packages("survival")
install.packages("xlsx")


deneme <- function(x,y){
  if (missing(x)|| missing(y)) {
    stop("x veya y degeri giriniz")
  }
  
  a <- sum(x)+sum(y)
  return(a)
  
}

x <- c(1,2,3)
deneme(x)
# burada y degerim olmadigindan hata aliyoruz cunku missing ile kontrol ettirdik ve yoksa bu hata mesajini yaz dedik ve stopla durdukduk

y <- c(2,5,8)
deneme(x,y)
# vektorlerin indislerine gore toplayip verdi





getwd()
setwd("C:/Users/halil ibrahim kaya/Desktop")
source("funcdeneme.R")
# deneme seklinde olusturdugumuz fonksiyonu funcdeneme seklinde masaustune kaydetmistik
# bu sekilde de baska projeler icin cagirabiliyoruz. guzel bi kod unutma bunu

deneme(x,y)
# ayni sonucu aldik


log(sqrt(deneme(x,y)))
# teke seferlik nisimiz varsa atama yapmayip boyle yazariz ancak bu karisik bi yapi
# asagidaki paketi yukleyelim
# tidyverse paketine benziyor


install.packages("magrittr")
library(magrittr)

deneme(x,y) %>% sqrt %>% log
# bu paket ile cok islem gerektirip ic ice yazdigimiz kodlari tek satirda daha anlasilir yazmamizi sagliyor



ls() # degiskenleri goruntuler
rm() # icine yazilan degiskeni siler
rm(x) #xi sildik
ls() #x yok

# sayfa 32 alistirmalari 
install.packages("Matrix")
install.packages("ggplot2")

library(MASS)
detach(package:MASS, unload = T)


##### veri turleri kitaptan bilmediklerimiz#####

complex(real = 2, imaginary = 1)
# 2+1i

complex(real = 2, imaginary = 7,)
# 2+7i

complex(real = 5, imaginary = 7,)
# 5+7i 

# sayilarini veriyor. karmasik sayilar.




# charToRaw fonksiyonu icine yazdigimiz ifadenin onaltilik karsiliklarini verir

charToRaw("selam")
# 73 65 6c 61 6d

charToRaw('25')
# 32 35

charToRaw("a");charToRaw("A")
# a 61 A 41


y[0]
# 0. indis o vektorun turunu yazar

t(y)
# t fonksiyonu ile devrigini ald??k 
# matrisde bu devriklikn daha baska isliyorcu

a <- c(12,13,14)
b <- c(15,16,17)

t(data.frame(a,b))


TurAd <- c("Kara melike","Kucuk atesguzeli", "Guzelmavi")
TurAd
KntAck <- c(52.5,30.2,34.2)
KntAck
KntRenk <- c("siyah","ates","mavi")
KntRenk
Benek <- c(TRUE,T,F)
Benek
Habitat <- c("O","C","OC")
Habitat
BolDrc <- c(3L,5L,6L)
BolDrc
UreBitki <- c("Bogurtlen","Labada","Baklagil")
UreBitki
MaksYuk <- c(1600,1600,1750)
MaksYuk






kelebekGozlem <- data.frame(TurAd,KntAck,KntRenk,Benek,Habitat,BolDrc,UreBitki,MaksYuk)
kelebekGozlem


m <- matrix(c(1,2,3,4,5,6), nrow = 2,ncol = 3)
m
# 2*3 maris verdi

t(m)
# devrik halini verdi.




il <- 'Adana'
yillar <- c(2017,2018)
smuretim <- matrix(c(1.1,1.3,2.1,2.5), nrow = 2, ncol = 2, byrow = T)

adanatarim <- list(il,yillar,smuretim)

adanatarim

adanatarim[[1]]

adanatarim[[2]]

adanatarim[[2]][2]

adanatarim[[3]]

adanatarim[[3]][1,1]

adanatarim1 <- list(lokasyon=il, donem=yillar, uretim=smuretim)

adanatarim1

adanatarim1$lokasyon

adanatarim1$uretim

names(adanatarim1)


adanatarim1[[1]]
adanatarim1$lokasyon
adanatarim1['lokasyon']
# ??c?? de aayn?? i??levi gercekle??tiriyor

adanatarim1['uretim'][1,1] # hata verir
adanatarim1[['uretim']][1,1] # 1. satir 1. sutunu verir
# ikili kare parantezin onemi budur


matdizi <- array(dim = c(3,2,2))
matdizi

# her biri 3 sat??r 2 sutun olan 2 tane matris verdi

matdizi1 <- array(dim = c(5,2,3))
matdizi1

# her biri 5 satir 2 s??tun olan 3 matris verdi


x <- c(3,4,2,9,4,5,6,12,3,2,1,6)
dizi <- array(x,dim = c(3,2,2))

dizi[,,1]
# ilk matrisi verdi

dizi[1,1,1]
# 1. satir 1. sutun 1. matris


dizi[3,2,2]
# 3. satir 2. sutun 2. matris




amasya <- rep('Amasya',3)
nigde <- rep('Nigde',3)
demir <- rep('Demir',3)

elma <- data.frame(il=c(amasya,nigde,demir), sayi=c(90,120,110,150,93,97,45,77,63))

levels(elma$il)

elma$il <- as.factor(elma$il)

levels(elma$il)

summary(elma)

factor(elma$il,ordered = T)
# ordered fonksiyonu ayni zamanda s??ralad??


factor(elma$il,levels = rev(levels(elma$il)))
# rev fonksiyonu ile s??ralamay?? tersten ald??k


ordered(factor(elma$il,levels = rev(levels(elma$il))))
# burda da s??ralama i??areti koyduk, tersten s??ralaman??n i??areti

ordered(elma$il)
# normal s??ralamas??n?? burda verdi



attach(mtcars)
# mtcars uzerinde caliscaz bi daha yazmim diye attach ettim

silindir <- table(cyl)
# silindir sayilarini ve o silindir sayisina ait araba sayisini verdi

vites <- table(gear)
# vites sayilarini ve o viteslere ait araba sayilari


tablo3 <- table(cyl,gear)
tablo3
# ikili karsilastirmali bi tablo verdi

margin.table(tablo3,1)
# sat??rlar??n toplam??n?? verir

margin.table(tablo3,2)
# sutunlar??n toplam??n?? verir


margin.table(tablo3)
# genel toplam?? verir



mosaicplot(tablo3,
           main = 'Silindi-Vites Grafi??i',
           color = c('orange','green','purple'),
           cex = 1)

prop.table(tablo3)
# # hepsini oran olarak veriyor. toplam??n 1 eden oran olarak 
# yogunluklar?? bu??luyoruz

prop.table(tablo3,1)
# sat??r seviyesinde oranlar?? veriyor

prop.table(tablo3,2)
# sutun seviyesinde oranlar?? veriyo

# yani her sat??r/sutun oran?? 1e esit oluyor burda





table(cyl,gear,carb)
# bu dizi olarak veriyor. her bir carb degeri icin matris veriyor

ftable(cyl,gear,carb)
# bu da direkt tablo olarak veriyor.



xtabs(~cyl+gear,mtcars)

tablo4 <- xtabs(~cyl+gear,mtcars)

ftable(tablo4)
# xtabs ile capraz tablo olusturma da boylr yapiliyor. 3 degiskenli yapalim bir de

tablo5 <- xtabs(~cyl+gear+carb, mtcars)

ftable(tablo5)
# bu sekilde olusturduk

install.packages('gmodels')
library(gmodels)

CrossTable(cyl,gear,fisher = T,mcnemar = T)
# crosstabel cokkkkkk iyi bir fonksiyonmus  her seyi alabiliyoz resmen
# bunun argumanlar??na bakal??m biraz

??CrossTable

table(cyl,gear)

x <- c(x1=1,x2=2)
x
unname(x)
# isimleri kaldirir atama yaparak isimleri kaldirilmis halini alabilriz

dimnames(mtcars)
# hem sat??r hem sutun  isimlerini listeler seklinde verir


rm(x)

x <- data.frame(a=rnorm(15),b=rnorm(15),c=rnorm(15))
x

edit(x)
# x veri setini ek bi goruntude acar ve duzenlememizi saglar atama yapmazsak kaydetmez


fix(x)
# x veri setini edit kismi gibi acar ve duzenlememizi saglar. atama yapmadan kaydeder
x







y <- c(TRUE,FALSE)
y
typeof(y)
# logical
y[length(y)+1] <- 3 
y
# true 1 false 0 oldu
typeof(y)
# double

y[length(y)+1] <- 'halil' 
y
# hepsi string oldu
typeof(y)
# character



letters[1:4]
# ilk 4 harfi verir
letters
# alfabeyi verir her bi harfi tek tek verir


df <-  data.frame(f=letters[1:4], y=sample(1:100,4))
df



sub("am","man","selam")
# ilk k??s??m c??karacaklar??m??z ikinci k??s??m ekleyecekkerimiz son k??s??m neyden olacag??

sub('\\s','a','sensense')


v <- c(1,2,3,4,5,6,7,8)


a <- matrix(v,nrow = 4,ncol = 4)
a

crossprod(a)


diag(c(1,2,3,4,5,6,7,8),4,4)


m1 <- matrix(c(2,2,3,4,5,6),nrow = 2,ncol = 3,byrow = T)

m2 <- matrix(c(7,9,11,8,10,12),nrow = 3,ncol = 2)


m1%*%m2
# matrisler ic caprimidir.

m1%o%m2
# matrisler dis carpimidir



attach(mtcars)
detach(mtcars)
head(mtcars[order(mpg),])
# mpg'ye gore s??rala ver dedik


head(mtcars[order(mpg,decreasing = T),])
# buyukten kucukge yaptik burda da


head(mtcars[order(mpg,cyl,decreasing = T),])
# once mpg sonra cyl'ye gore siraliyor


head(mtcars[order(-mpg,cyl),])
# once mpgye gore buyukten kucuge sonra cylye gore kucukten buyuge



boy <- 3

class(boy)
typeof(boy)

seq(from=1,to = 10, by =2)



piller <- data.frame(marka =c('pil','marka',"aticam","20 tane","say","sen","duracell"),
                     tipi=rep('iyon',7),
                     dayanma =c(10,20,10,50,30,50,10))
piller


A <- matrix(c(4,2,1,6,3,2),nrow = 2,ncol = 3)

A[2,]
A[,1]
A[2,3]*A[1,3]


tarimisletme <- list(il=c('istanbul','adana'),
                     ilce=c('sultanbeyli','cukurova'),
                     kurulusyil=c(2001,1998),
                     urun=c('pirinc','pamuk'),
                     gelir=c(150000,450000))
tarimisletme

7
aromatikbitki <- c('kekik','anason','dereotu')

class(aromatikbitki)
mode(aromatikbitki)
typeof(aromatikbitki)

debi  <- c(44,NA,23,32,51)
anyNA(debi)

x <- c(12,4,8,7)
length(x)


x <- c('a','b','c','d')
y <- c(300,275,412,87)

df <- data.frame(x,y)

nrow(df)
ncol(df)
dim(df)


str(x)
head(x,2)
tail(x,2)

x <- c("1980","1985","1990")

x <- as.numeric(x)
class(x)
x <- as.factor(x)
class(x)




pillermatrix <- as.matrix.data.frame(piller)
pillermatrix




a <- 1.2 -> c 
a
c

14%/%3*2^2



x=10
y=25
z=5

y >= x*2 | sqrt(y) > x/z


'z' %in% 'zeytin'

x <- c(3,2,4,3,8,1,5,5,6,2)

order(x,decreasing = T)
sort(x,decreasing = T,method='auto')
order(x)
?order

df <- data.frame(kod=c('x11','a20','d23'),
                 perf=c(63,32,24))
df


df[order(df$perf),]
# performansa gore kucukten buyuge siralama




m1 <- matrix(rnorm(15,sd = 3,mean = 5),nrow = 3,ncol = 5)
m1

m2 <- matrix(rnorm(n = 15,sd = 7,mean = 10),nrow = 3,ncol = 5)
m2


m1*m2
m1+m2
m1-m2
m1/m2

m1%o%m2



x <- c(10,5,4,3,3,6)
mean(x)
median(x)
sd(x)

# sayfa 89-90 alistirmalari buraya kadardi



getwd()

data(iris)
iris

View(iris)

mean(iris$Sepal.Length)



gozlemler <- textConnection("
x1 x2
3  4
7  4
6  3    
12 5
10 6"
)

x <- read.table(gozlemler,header = T)
close(gozlemler)
gozlemler
x
# yukarida bi yerden direkt kopyalanmis bi tablo aldik ve bunu tablo sekline sokmak icin bu komutlar?? kulland??k

colnames(x) <- c('x1','x2')
rownames(x) <- letters[1:5]
x
# sat??r sutun isimlerini verdik


install.packages("read_xlsx")
library(readxl)



install.packages("RCurl")
library(RCurl)

depo <- "https://archive.ics.uci.edu/dataset/39/ecoli"

a <- read.table(textConnection(getURL(a)),sep = "\t",header = F)



a <- "https://doi.org/10.24432/C5MW4Z."


a <- read.table(file.choose(), sep = "\t",header = T)
b <- read.table(file.choose())

names(a) <- b





depo <- "https://archive.ics.uci.edu/ml/machine-learning-databases/ecoli/ecoli.data"


ecoli <- read.table(textConnection(getURL(depo)),header = F)

install.packages("readr") # CSV dosyalar?? i??in
install.packages("foreign") # ARFF dosyalar?? i??in
install.packages("RWeka") # ARFF dosyalar?? i??in alternatif
library(foreign)
?read.arff

read.arff(choose.files())





getwd()
setwd("C:/Users/halil ibrahim kaya/Desktop")
sink(file = 'analiz.out')
# konuma .out seklinde bir dosya olusturduk. girdigimiz verileri oraya yazacagiz
cat("DF veriseti\n")
# cat ile icindeki yaziyi yazdik oraya
print(df)
# df veri setini icerisine yazdik
cat("\n temel istatistikler\n")
# cat icerisindekini yazdirdik
tstat <- summary(df)
tstat
# temel istatistik degerlerini alip yazdik. calisan sonuclari direkt oraya yaziyor.
# kendi ekranimiz bos durumda burdaki sonuclarin nasil ciktigini da ekleyecegim analiz.out
# dosyasindan ciktilara bakabiliriz
sink()
# yazdirmayi bitirdik. parametresiz sink() yazarak



sink(file = "analiz.out",append = T)
cat("\n normallik test sonuclarindan p.value'lar?? \n")
a <- df %>% group_by(X) %>% summarise(shapiro.test(Alcohol)$p.value)
a
cat("\n ilk iki grubun normallik testleri \n")
sink()

# tekrar bir sey eklemek istedigimizde append =TRUE yazmamiz gerekiyor
# bu sink fonksiyonuyla matris ve veri cerceveleri icin yetersiz 
# onun yerine baska paketler avr



write.table(df,file = "df.txt",sep = "\t")
# df icerisindeki verileri tab bosluk ekleyerek yazdirdik.
# goruntusunu yine df.txt dosyasiyla birakacagim



tstat <- summary(df)
tstat

save(df,tstat,file = "save.Rdata")
# dosyayi kaydettik ve ileride kaldigimiz yerden devam edebilecegiz
load("save.Rdata")
df
# kaldigimiz yerden devam edebiliriz.


install.packages("xlsx")
library(xlsx)
write.xlsx(df,file="excel.xlsx")
# excel seklinde kaydeder.

png(filename = "sarapalkol.png")
pairs(df[,1:2])
dev.off()
# grafik olarak kaydediyor. parametrelere bakariz sonra


dev.copy(png,"alkonorani.png")
dev.off()
# suan cizili olan grafihi kaydeder direkt.


## sayfa 114 alistirmalari

install.packages("robustbase")
  library(robustbase)
view(lactic)
# veri setini yukledik ve inceledik

attach(lactic)

mean(X)
median(X)
mean(Y)
median(Y)

detach(lactic)

# ortalamalarini ve ortanca degerini aldik



bverim <- textConnection("
bitkiboyu bitkiverimi
120 44
152 60                         
130 64                         
135 72 ")


y <- read.table(bverim,header = T)

close(bverim)
y
# tablo olusturduk, connectionu kapattik ve tabloyu yazdirdik


bverim2 <- read_csv(file = "bboy.csv")
# csv olarak yaptigimiz dosyayi bu sekilde iceri aktardik

library(readxl)
bverim3 <- read_xlsx("bboy.xlsx")

# excel olarak ekledik


library(RCurl)
depo <- "https://vincentarelbundock.github.io/Rdatasets/csv/robustbase/milk.csv"
milk <- read.table(textConnection(getURL(depo)),header = T,sep = ",")

# milk.csv dosyasini url ile ekledik

rand1 <- rnorm(100,mean = 5,sd = 0.5)
rand2 <- runif(100,min = 35,max = 110)
rand3 <- rweibull(100,scale = 7,shape = 1.2)

randdata <- data.frame(rand1,rand2,rand3)
getwd()
setwd("C:/Users/halil ibrahim kaya/Desktop")
write.table(randdata,file = "randomdist.dat",sep = "\t")

# masaustunde randomdist.dat seklinde dosya olusturup tablonun icerigini oraya attik




sink(file = "rand.out")
cat("randdata verisetinin verileri: \n")
randdata
cat("\n ozet bilgileri \n")
summary(randdata)
sink()

# dosya olusturduk cat ile yazi yazdik verileri girdik ve ozet bilgileri de yazdiktan sonra
# sink() ile kapattik



save(randdata,file = "rand.Rdata")
# Rdata save dosyasi



## sayfa 114 alistirma sonu



kiraz1 <- data.frame(magr=c(5.8,5.7,5.4,4.9,6.7,4.8,6.3,5.9,5.3,5.9,7.1,6.2,6.4,6.5,6.4,6.3,5.8,7.2,6.7,6.6,5.2,6.5,6.2,7.1,5.5,5.6,6.2,5.5,6.3,6.4),
                     grup=c(rep('A',10),rep('B',10),rep('K',10)))


kiraz2 <- data.frame(magr=c(5.9,6.6,6.5,6.6,7.0,7.1,6.3,6.2,6.1,5.9,10.5,8.4,9.6,10.5,6.9,8.7,10.8,9.4,10.1,6.1,4.0,5.1,6.1,4.9,5.1,5.1,5.2,4.8,4.6,6.4),
                     grup=c(rep('A',10),rep('B',10),rep('K',10)))
# 2 dataframe olustuduk

kiraz1$grup <- as.factor(kiraz1$grup)
kiraz2$grup <- as.factor(kiraz2$grup)
str(kiraz1)
str(kiraz2)

head(kiraz1)
head(kiraz2)
tail(kiraz1)
tail(kiraz2)
# verileri kontrol ettik

kiraz1$grup <- ordered(kiraz1$grup,levels=c("K","A","B"))
kiraz2$grup <- ordered(kiraz2$grup,levels=c("K","A","B"))

# levelleri siraladik. Kontrol grubu, 20ppm A grubu, 50 ppm B grubu (asit dozlari)


par(mfrow=c(1,2))
hist(kiraz1$magr,col = "gray",prob=T,
     xlab = "Agirlik (g)",
     ylab = "frekans",
     main = "deneme grafigi")
# grafigimizi olusturduk
lines(density(kiraz1$magr),lwd=3,col="orange")
# yogunluk cizgimiiz cektik

hist(kiraz2$magr,col = "gray",prob=T,
     xlab = "Agirlik (g)",
     ylab = "frekans",
     main = "deneme grafigi")
# grafigimizi olusturduk
lines(density(kiraz2$magr),lwd=3,col="orange")
# yogunluk cizgimiiz cektik
http://127.0.0.1:43819/graphics/plot_zoom_png?width=1587&height=705


par(mfrow=c(1,2))

hist(kiraz1$magr[kiraz1$grup=="A"], col = "gray", probability = T)
lines(density(kiraz1$magr[kiraz1$grup=="A"]),lwd=3,col="orange")

hist(kiraz1$magr[kiraz1$grup=="B"], col = "gray", probability = T)
lines(density(kiraz1$magr[kiraz1$grup=="B"]),lwd=3,col="orange")
# kiraz1 df icerisindeki A ve B gruplarinin grafigi




par(mfrow=c(1,2))
boxplot(kiraz1$magr,col = "gray",border = "blue",main ="deneme",xlab="agirlik",horizontal = T)

boxplot(kiraz2$magr,col = "gray",border = "blue",main ="deneme",xlab="agirlik",horizontal = T)

# kiraz1 daha ortada ve normal dagilis gosterirken kiraz2 daha daginik gpruntuye sahip duruyor



qqnorm(kiraz1$magr,main = "q-q grafigi",
       xlab = "Teorik kantiller",
       ylab = "orneklem kantiller")
qqline(kiraz1$magr,col="purple",lwd =2)

# qq grafigi ile inceleyelim. kriaz 1 normal gibi duruyor. cziginin cevresinde hep

qqnorm(kiraz2$magr,main = "q-q grafigi",
       xlab = "Teorik kantiller",
       ylab = "orneklem kantiller")
qqline(kiraz2$magr,col="purple",lwd =2)

# qq grafigi ile inceleyelim. kriaz 2 normal degil gibi duruyor. cizginin disindalar hep




model1 <- lm(magr~grup, kiraz1)
model2 <- lm(magr~grup, kiraz2)
qqnorm(residuals(model1)) ; qqline(residuals(model1))
qqnorm(residuals(model2)) ; qqline(residuals(model2))

# yine grafik ciziyoruz. ilk cizdigimiz grafikler normal degerler grafigi olurken
# ikinci cizdigimiz yogunluk grafigidir. 


summary(kiraz1$magr)
# ozet bilgileri verir

install.packages("psych")
require(psych)

describe(kiraz1$magr)
# daha detayli ozet bilgileri verir

describeBy(x=kiraz1$magr,group = kiraz1$grup)
# gruplarin ozet bilgilerini verir

describeBy(kiraz2$magr,group = kiraz2$grup,digits = 2)
# kiraz2 icin grup ozet bilgileri


install.packages("Rmisc")
library(Rmisc)

summarySE(data = kiraz1,"magr",groupvars = "grup",conf.interval = 0.95)
# yine gruplara ait ozet bilgileri iceren baska bir paketteki fonksiyon

install.packages("FSA")
library(FSA)

Summarize(magr~grup, data=kiraz1)
# baska paketteki ozet bilgiler fonksiyonu yine


library(dplyr)

group_by(kiraz1,grup) %>%
  summarise(
    count = n(),
    mean = mean(magr,na.rm = T),
    var = var(magr, na.rm = T),
    sd = sd(magr, na.rm =T),
    median = median(magr, na.rm = T),
    IQR = IQR(magr, na.rm=T)
  )

# gruplarin icerisindeki verileri veriyor.


install.packages("gvlma")
library(gvlma)

model1 <- lm(magr~grup, kiraz1)

gvkont1 <- gvlma(model1, alphalevel = 0.05)

summary(gvkont1)

# analiz icin gerekli sonuclari veriyor.
# bu testlerde global stat varsayimlarin kabul edilebilir veya edilemez seklinde
# olan kismini verir diger satirlarda capiklik basiklik falan degerlerinin
# varsayimlar icin olan durumlarini p degeri ile veriyor.
# 0.05 ustu olanlar kabul edilebilir seklinde sunuluyor
# aynisini kiraz 2 icin de yapalim

model2 <- lm(magr~grup, kiraz2)

gvkont2 <- gvlma(model2, alphalevel = 0.05)

summary(gvkont2)

# burda daha kotu sonuclar geldi mesela global stat 0.07
# yani evet 0.05den buyuk ama yine de varsayimlar icin
# sinirda olan bi deger. carpiklik 0.04 cikmis yani kabul edilebilir degil
# onemli olarak verilmis. bu sebeple kiraz2 verisi parametrik testlere uygun degil diyoruz
# bununla beraber genel testin sonucu 0.05den buyuk oldugundan parametrik testler uygun 
# oldugunu gostermektedir. kitaptan dikret alinti burasi biraz cakisiyo gibi geldi.
# anlamadim



















