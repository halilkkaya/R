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









