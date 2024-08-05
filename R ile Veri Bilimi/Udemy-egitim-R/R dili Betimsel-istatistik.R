#### betimsel istatistikler toplam deger ve ortalama####
x <- c(23,45,67,43,56)

length(x) #kac adet veri bulundugunu soyler

sum(x) #tum elemanlarin sayisi

ortalama <- sum(x)/length(x)
ortalama #ortalama 

mean(x) #ortalama fonksiyonu buymus avg degilmis

#### serbestlik derecesi nedir? olcme nasil?####
#gozlem sayisinin 1 eksigi = (N-1)
#mesela 7 sapka var hepsini 1 gun kullancan daha kullanmican
#en son cumartesi 2 tane kaldi 1 tane sectin pazar icin secme sansin yok
#elinde 1 tane kaldi. bu da serbestlik degil oluyo

x <- c(1,2,3,4,5)
length(x)

sd<- length(x)-1
sd #serbestlik derecesi


#### standart sapma####
x <- c(12,34,56,34,23,45)
length(x)

ort<-mean(x) #ortalama

ss<-sd(x) #standart sapmayi hesapladi

#standart sapma degerlerin ortalamadan ne kadar saptigini gosteren bir istatistiktir

# ortalamaya standart sapmayi ekleyip cikaralim yani;
ort-ss #18.44 cikti 
ort+ss #49.55 cikti
#anlami su bizim genel degerlerimiz 18.44 ile 49.55 arasinda oluyor
x
#degerlere bakiyoruz ve evet genel olarak o aralikta.

#### varyans hesaplama ####

# standart sapmanin karesidir

#gozlemlerin degisikligini tanimlayan numerik bir degerdir

#baska bir degisle gozlemlerin orneklemde ne kadar yayildigini gosteren deger

#standart sapmadan farki standart sapma veri set icerisindeki gozlemlerin
#ortalamadan farkliligini tanimlar varyans ise degiskenligi tanimlar

x <- c(12,14,10,11,13,17,16)
length(x)

sd1 <- sd(x) #standart sapma 2.56

vr1<- sd(x)**2 #varyansim 6.57. standart sapmanin karesiyle de hesaplaniyor.

sd1
vr1

var(x) #varyansimi bulan fonksiyon. 6.57


y <- c(12,25,60,56,35,24,45)
length(y)
sd2<-sd(y)
vr2<-var(y)

sd1;vr1;sd2;vr2 #hepsini tek tek calistir demek
#standart sapma buyukse degerler arasi buyuktur



#### medyan, aciklik ve ceyreklikler####


#medyan ortanca degerdir
#acikliik max degerle min deger arasindaki fark
#ceyreklik serimizi 4 esit parcaya boldugumuzde q1 q2 q3 q4 parcalaridir.

x <- c(12,34,56,23,34,12,35)

median(x) #ortanca deger 
mean(x) #ortalamasi 29.42


max(x)-min(x) #aciklik degeridir.
range(x) #en kucukle en buyuk elemani verir ama cikartma yapmaz

quantile(x) 
#  0%  25%  50%  75% 100% 
# 12.0 17.5 34.0 34.5 56.0 
# degerini veriyor.

quantile(x,probs = c(0.25,0.5,0.75)) #burdaki ucluyu verir

quantile(x, probs = c(0.10,0.6,0.95) ) #burda da diger isteklerimize gore yanit geliyor










x <- c(12,13,14,15,16,100,120)
x

mean(x) #41 olarak bulduk. ??o??luklu deger a??a????da saedece 2 sayi yuzunden artti.
#boyle durumlarda median kullanmak daha mantikli
median(x) #15 cikti


y <- c(12,15,17,18,20,24,26)
 mean(y) #18.85 verdi ortalama
median(y) #18 verdi 
#birbirine yakin degerlerse bu sekilde medyan ve ortalama yakin cikiyor



hist(x) #xi grafiklestiriyor


install.packages('e1071') #capraz katsayi icin indirdik kutuphane
library(e1071) #calistirdik

skewness(x) #carpiklik katsayisi 0.78 buldu
#ne dedik 0-1 arasi saga carpik demektir
#yani veriler solda daha cok demektir. burda da 0.78 buyuk bi sayidir

z <- c(1,2,3,60,70,60,70,80,70)

hist(z) #grafik inceledik

skewness(z)#-0.50 cikti
#-1 ile 0 arasindaysa sola carpik dedik yani veriler sagda yogun  






#### siklik/frekans hesaplama ####

x <- c(12,12,13,13,14,13,12,12,15,16)

length(x)


unique(x) #tekrarlananlari vermez

table(x) #hangi veri kac kere tekrarlandiggi yaziyo. burdan da 
#sikligini bulabiliriz

t <- table(x)
t
names(t) #hangi elemanlar var karakter olarak verir

t[1] #ilk elemani verir.
t[12] #deger olarak yazarsan vermez
t['12'] #bu sekilde calisir.

t[['12']] #tekrar sayisina ulasiriz direkt

c <- c('a','a','a','b','b','C')
c
t1<-table(c)
t1['a'] #a ve kac kere tekrar ettigi geliyo
t1[['a']] #sadece tekrar sayisi

#### kayip gozlemlerin hesaplamalara etkisi####

x <- c(12,13,14,15,NA)
sum(x) #sonuc NA geldi. NA degeri olmadigi icin toplanamiyo
sum(x, na.rm = T) #<NA degerlerini kaldir dedik. hesapladi
mean(x) #yine NA dedi
mean(x , na.rm = T) # calisti. ortalama 13.5
#ortalama alirken NA yi eleman olarak aldi mi? bakalim
(12+13+14+15)/4 #13.5 geldi. NA degerlerini almiyor eleman sayisi olarak!!!
(12+13+14+15)/5 #10.8 geldi. NA eleman olarak sayilsaydi boyle cikacakti

sd(x) #standart sapma hesabi da NA geldi
sd(x, na.rm = T)#NA degerlerini almadik ve 1.29 bulduk

median(x) #yine NA
median(x, na.rm = T) #13.5 verdi

is.na(x) #false false true seklinde hangisi na ona true yaziyo
which(is.na(x))# hangi eleman NA onu bulduk

y <- c(12,13,14,15,NA,NA,14,1,15,NA)
which(is.na(y)) #5-6-10. indislerde na var diyor


any(is.na(y)) #indexleri elde etmim sadece var mi ypk mu bak demek icin fonksiyon
#true dondu. yani icerisine girdigimiz kosul dogru cikt,
#iceriye de na var mi diye bak dedik. bakti ve tek sonuc dondurdu
#varmis


t <- c(1,2,3,4,5)
any(is.na(t))
#false dedi yani na degeri yok

mean(x, na.rm = any(is.na(x))) # once kontrol ettik.
#na var mi yok mu kendimiz yazmak yerine kendi kendine yazdirdik
#varsa oraya true yazdircak ve NAlari alma dicek
#yoksa zaten NA yok dondurcek


#na lar yerine 0 degeri atamasi ornekleri
is.na(x)
which(is.na(x))
x[which(is.na(x))] <- 0
x

is.na(y)
which(is.na(y))
y[which(is.na(y))] <- 0
y

y <- c(12,13,14,15,NA,NA,14,1,15,NA)
y[is.na(y)]
y[is.na(y)] <- 0
y

#### normal dagilan veriler####  

r <- rnorm(100) #100 tane normal dagilan veri veriyor
r

length(r) #100 verdi
hist(r) #ortalamaya yakin degerler var hep
#rnorm default olarak alindiginda ortalama 0 alinir
#ve standart sapma 1 alinir ama istersek degisebilirz

r1 <- rnorm(30, mean = 10, sd = 3)
#istedigimiz degerler cercevesinde verdi bize
hist(r1) #10 cevresinde dagilmis bi veri. normal dagilim.


####odev####
#Bu odev ile ilgili sorular
#100 adet rastgele normal dagilan bir degisken (vektor) olusturunuz ve bu vektorun degerlerini 
#round() donksiyonu kullanarak en yakin tam sayisa yuvarlayiniz ve bu islemlerin kodlarini cevap olarak belirtiniz.

rastgele<-rnorm(100)

tamsayi <- round(rastgele,0)
tamsayi


#Bir onceki soruda elde ettiginiz vektorun ortalama, standart sapma, medyan, 
#birinci ve ucuncu ceyreklik degerlerini bulunuz ve bu islemleri gerceklestirdiginiz kodlari cevap olarak belirtiniz.

mean(tamsayi) #ortalamasi -0.22
sd(tamsayi) #standart sapma 1.0008
median(tamsayi) #0 verdi

quantile(tamsayi, probs = c(0.25,0.75)) #1. ve 3. ceyrekler

#Elde ettiginiz medyan ve ortalama degerini bir nesnede kaydederek arasindaki farki hesaplayiniz. (Cevap olarak kodlari belirtiniz.)

medyan <- median(tamsayi)
ortalama <- mean(tamsayi)

ortalama - medyan #-0,22 verdi
medyan - ortalama #0.22 verdi



