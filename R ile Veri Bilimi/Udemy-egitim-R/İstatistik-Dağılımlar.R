#### bernoulli da????l??m??####   

install.packages('Rlab')

# dagilimlar icin gerekli paketi yukledik

library(Rlab)


?dbern


dbern(x = 0, prob = 0.6)
#x hangi olasigilin gelmesini istedigini belirtirken prob ise uyguladigimiz deneyin
# basarili olma olasiligidir
# ustteki 0.4 cikti cunku 0.6 gerceklesme olasiligiydi . biz x =0 diyerek
# gerceklesmeme olasiligini sorduk


dbern(x = 0, prob = 0.7)
#basarili olma olasigili 0.7 dedik ve basarisiz olma olasiligini istedik. 
# 0.3 cevabini aldik


dbern(x=1, prob = 0.6)
#basarili olma olasiligini istedik ve prob olarak 0.6 belirledik
# 0.6 verdi

dbern(x=2, prob = 0.2)
# x 1 ve 0 olabilir sadece 2 verdik ve bize 0 dedi yani 2 olma ihtimali yok


#bir torbadan siyah top alma olasiligim 0.4
dbern(x=0, prob = 0.4)
# basarisiz olma ihtimalim 0.6 cikti


pbern()#k??melatif olarak olasiliklari hesaplar.

pbern(q= 1, prob = 0.7, lower.tail = TRUE)
# 1 sonucunu verdi peki neden?
# 1den daha dusuk olma olasiliklarina bakar
# yani 0 ve 1
# basarili olma olasiliklariyla basarisiz olma olasiliklari toplar
# dbernden gelen sonuclari toplar yani. soyle ornek verelim??

dbern(x = 0, prob = 0.7)+dbern(x=1,prob = 0.7)
# 1 cevabini aldik

#lower.tail true ise kucuk ve esitlere false ise buyuklere bak demek

pbern(q= 1, prob = 0.7, lower.tail = FALSE)
#burda da 1den daha buyuk olma olasiliklarina bakar. lower.tail false dedigimiz icin
# cevabi 0 verdi
# 1den daha fazla olamaz bernoullide
# lower.tail false ise esitlik yok. true ise var

pbern(q= 0, prob = 0.7, lower.tail = FALSE)
# lower.tail false oladugu icin sifirdan buyuk olma ihtimaline bakicaz
# yani sadece 1 olma yani basarili olma ihtimalinbe bakicaz
# o da prob degeri olan 0.7 geldi

pbern(q= 0, prob = 0.7, lower.tail = TRUE)
# lower.tail true dedik yani 0 veya 0dan kucukleri sec dedik.
# true oldugu icin esitlik de var unutma
# 1 olma ihtimali yok cunku 0dan kucukleri dedik
# 0 olma ihtimali de 0.3 geliyor cunku 1 olma ihtimali 0.7ydi


qbern(p = 0.5, prob = 0.7, lower.tail = TRUE)
# 0.5 olasiliktan daha kucuk olasilikla biz kaci elde edebiliriz
# 1 cevap
# yani ben 0.5 veya daha az olasilikla  1 veya daha az sonucu elde edebilirim

qbern(p = 0.3, prob = 0.7, lower.tail = TRUE)
# 0 verdi. 
# 0.3 veya daha az olasilikla 0 gelir demek

qbern(p = 0.31, prob = 0.7, lower.tail = TRUE)
# 1 verdi

#aradaki fark su bak p ile prob topla 1 ederse 1 etmezse 0 ama lower.tail true iken


qbern(p = 0.2, prob = 0.7, lower.tail = FALSE)
# lower.tail false oldu yani artik buyuktur degerlere bakicaz
# 0.2 ve daha fazla olasilikla 1i elde ederiz demek

qbern(p = 0.8, prob = 0.7, lower.tail = FALSE)
# 0.8 veya daha fazla olasilikla 0i elde ederiz demektir


rbern(n = 50, prob = 0.7)
# basarili olma ihtimali her bi denemede 0.7 olan 50 deneme yap demek

r <-rbern(n = 50, prob = 0.7)
r
table(r)
# 50 veri uretmistik 15 0 35 tane 1 

table(rbern(n = 50, prob = 0.3))
# 0 degeri daha fazla oldu







##### binom dagilimi#####

?dbinom


# bir olayda basarili olma olasiligi 0.7
# basarisiz olma 1-0.7den 0.3 
dbinom(x = 5, size = 10, prob = 0.7)

# x kac kere basarili olma ihtimalini bulmak istedigimiz
# size kac kere deneyecegimiz
# prob her deneyde ayri ayri basarili olma ihtimali
# tam 5 kere basarili olma ihtimali [1] 0.1029193'dir. kuc??-uk veya buyuk yok tam 


dbinom(x = 7, size = 10, prob = 0.7)
#0.2668279 verdi


dbinom(x = 9, size = 10, prob = 0.7)
#0.1210608 verdi

#bu degisim nasil oluyor bunu grafik cizip ogrenelim.

plot(x = 1:30, y= dbinom(x=1:30,size = 30, prob = 0.7),
     bty='L',
     pch=19)
#grafigi inceledigimizde 30 denemede sadece 1 kere 1 gelme olasiligi 0a cok yakin
# 2,3,4,5,6.... 15e kadar boyle. 0.7 olasilik buyuk bi olasilik oldugu icin yarisindan cogunun
# 1 gelmesini bekkeriz. grafik de 15-20 arasinmda artmaya devam edip zirveye cikarken
# 20-25 arasinda yine yuksek olmasina ragmen dususe geciyor. 0.7 ihtimal evet yuksek
#ama 25 denemeden fazlasininin 1 gelme olasiligini dusurecek bi olasilik. 25-30 arasi
#tekrardan dususe geciyor. 
#x ekseni deneme sayilarini verirken y ekseni o kadar denemede 1 gelme olasiligini veriyor

lines(dbinom(x=1:30,size = 30, prob = 0.5))
# basarili olma olasigili 0.5 olan bi cizgi cizdik ve 0.7 ile 0.5 arasindaki farki gorduk

lines(dbinom(x=1:30,size = 30, prob = 0.3))
# daha da dusurunca daha sola kaydi



pbinom(q = 5, size =30, prob = 0.7, lower.tail = TRUE)
#lower.tail true dedik yani 5 ve 5den kucuk denemenin 1 gelme olasiligini sorduk
# bu da 2.207714e-09 gibi cok kuuckj bi sayi geldi.
#sebebi su 0.7 olasilikla 1 gelcek ama topalm 5 veya daha az denemede 1 gelcek
# 0.7 yuksek ihtimal olduguhndan sayi kucuk geldi


pbinom(q = 5, size =30, prob = 0.7, lower.tail = FALSE)
# false dedik yani 5den fazla denemenin 1 gelme olasiligi
# cevap 1 geldi yani 5den fazla denemenin 1 gelmesine kesin gozuyle bakiliyor




pbinom(q = 16, size =30, prob = 0.7, lower.tail = TRUE)
# 16 veya daha azinin 1 gelme ihtimali
#0.04005255 geldi cevap

pbinom(q = 16, size =30, prob = 0.7, lower.tail = FALSE)
#0.9599475 diyo yani 0.95 ihtimalle 30 denemeden 16dan yuksek seviyede 1 gelecek

pbinom(q=10, size=30, prob = 0.7,lower.tail = T)
#bu demek alttaki demek #3.687008e-05

dbinom(x = 10, size = 30, prob = 0.7)+
dbinom(x = 9, size = 30, prob = 0.7)+
dbinom(x = 8, size = 30, prob = 0.7)+
dbinom(x = 7, size = 30, prob = 0.7)+
dbinom(x = 6, size = 30, prob = 0.7)+
dbinom(x = 5, size = 30, prob = 0.7)+
dbinom(x = 4, size = 30, prob = 0.7)+
dbinom(x = 3, size = 30, prob = 0.7)+
dbinom(x = 2, size = 30, prob = 0.7)+
dbinom(x = 1, size = 30, prob = 0.7)+
dbinom(x = 0, size = 30, prob = 0.7)

#10 ve daha az denemelerdeki sayi kadar gelme olasiligi dedik
#10 ve daha az gelme olasiliklarini tek tek yazip topladik ve cevap ayni
#3.687008e-05

#ornek :
#bir e ticaret sitesine gelen ortalama 4 musteriden 1i alis veris yapiyor.
#bu e ticaret sitesinde bir gun icin 30 musteri girmesi bekleniyor.
#en az 10 alisveris yapilma olasiligi nedir


pbinom(q = 9, size = 30, prob = 1/4, lower.tail = FALSE)
# 0.1965934 olasilikla 30 kisiden 10 veya daha fazlasi alisveris yapar.


#olasilik degerine gore kac kere basarili olacagimizi elde etme

qbinom(p = 0.7, size = 30, prob = 1/4, lower.tail = FALSE)
# 0.7 olas??l??kla 30 musteriden 6dan fazlasi alisveris yapar

qbinom(p = 0.7, size = 30, prob = 1/4, lower.tail = TRUE)
# yuzde 70 ihtimalle 30 musteriden 9 ya da daha azi alisveris yapar demek



# rastgele binom dagilan veriler

rbinom(n=50, size = 30, prob = 1/4)

# 50 kere uret dedik
# 30 kisi var sayal??m. 30 kisi her geldiginde kac kisi alisveris yapar
# sonucunu veriyor. yani cikan sonuclara gore 
# 30 kisi geldiginde 8 kisi yapar
# 30 kisi geldiginde 4 kisi yapar
# 30 kisi geldiginde 10 kisi yapar
# gibi 50 tane sonuc urettik. 0.25 ihtimalle 30 kisiden kaci alisveris yapar
# 0.25 ihtimalle tura gelen 30 kez atilan yazi turadan kaci tura gelir


hist(rbinom(n=50, size = 30, prob = 1/4))
# cogunlugun sol tarafta olan bi grafik



hist(rbinom(n=50, size = 30, prob = 1/2))
# duzgun dagilan bi grafik oldu. yani yakin 



##### poisson dagilimi ######

?dpois


# 1 saatte 15 tane araba geciyor

# 1- bu kopruden 1 saatte 20 araba gecme olasiligini bulalim
dpois(x = 20, lambda = 15)
#0.04191031 olasilik verdi. ortalama 15 geciyor biz 20 gecsin dedik zor dedi


# 2 bu kopruden 1 saatte 5 araba gecme olasiligi

dpois(x=5, lambda = 15)
#0.001935788 ortalama 15 geciyodu 5 tane zor daha cok gecer diyo


plot(x = 1:30, y=dpois(x=1:30, lambda = 15),
     pch =19,
     bty='L',
     col = 'orange')
# 1den 30a kadar her ihtimali tek tek deneyip veren grafiktir.
# tam olarak 15 araba gecme olasiligim 0.10 veriyor.

lines(dpois(x=1:30, lambda = 20),
      pch = 19,
      lty= 'dotted'
      )


# 1 saate 20 araba geciyor
# dakika olarak hesab??n?? yapal??m 
# dakika = 20/60 = 1/3 yani lambda 1/3 olacak

dpois(x =2, lambda = 1/3)
#bir dakikada 2 araba gecme olasiligi



dpois(x=1, lambda = 1/3)
# bir dakikada 1 araba gecme olasiligi



## bir saatte xden az veya xden cok seklinde k??melatif olasilik??

ppois(q = 5, lambda = 15, lower.tail = T)
# bir saatte 5 veya daha az araba gecme olasiligi

ppois(q=5, lambda = 15, lower.tail = F)
# bir saatde 5den fazla araba gecme olasiligi


ppois(q=2, lambda = 1/3, lower.tail = F)
# 1 dakikada 2den fazla araba gecme ihtimali 0.004



ppois( q = 30 , lambda = 20, lower.tail = F)
#hocain verdigi ornek cevabi. soru kisaca 20 urun aliniyo 1 saatte
# 30 urunden fazla alma orani nedir


ppois( q = 20 , lambda = 20, lower.tail = F)
# 20den fazla yapma olasiligimiz 0.44


ppois( q = 15 , lambda = 20, lower.tail = F)
# 15den fazla yapma olasiligim 0.84


## bir saatte su olasilikla kac araba gecer

qpois(p = 0.7, lambda = 15, lower.tail = T)
# 0.7 ihtimalle saatte 15 gecen yerden 17 veya daha az araba gecer dedi

qpois(p = 0.7 , lambda = 15, lower.tail = F)
# 0.7 ihtimalle 13 tane arabadan daha cok araba gecer

qpois(p = 0.2 , lambda = 15, lower.tail = T)
# 12 araba ya da daha az gecer dedi 

qpois(p = 0.9 , lambda = 15, lower.tail = F)
# yuzde 90 ihtimalle 10 arabdan fazla dedi


rpois(n = 30, lambda = 15)
# 30 kere denedik. saatte 15 araba gecen yerde her bi saatte kac tane araba gecer
# olasiligini veri. 30 tane verdi hepsi 1 saat icin bi tahmin

rpois(n=30, lambda = 1/3)
# dakikada 1/3 araba gecen yerde 1 dakikada kac tane gecer dedik 30 kere dene dedik



##### hipergeometrik dagilim######

?dhyper

dhyper(x = 4,m = 21,n = 31,k = 10)
# 4 tane k??rm??z?? cek, 21 tane k??rm??z?? var, 31 tane de k??rm??z?? d??s?? var
# 10 deneme yap??caz
# 0.2785484 ihtimalle 4 tanesi kirmizi gelirmis

dhyper(x = 5, m = 30, n=5, k = 5)
# 5 denemede 30 tane siyah top ve 5 tane beyaz top icinden 
# 5 tane siyah top cekme ihtimalim %43mus


dhyper(x = 5, m = 5, n = 30, k=5)
# 5 denemede 5 tane siyag top ve 30 tane beyaz top icinden
# 5 tane siyah top cekme ihtimalim %0.0000030larda baya dusuk

plot(1:50, dhyper(x =1:50, m=26,n=26,k=10),
     bty='L',
     pch = 19,
     col='orange')
lines(dhyper(x=1:50, m=40, n=12,k=10))

phyper(q = 4 , m= 5, n= 30, k =5, lower.tail = F)

# 5 siyah 30 beyaz toptan 5 denemede 4den fazla siyah cekme olasiligim
# yani daha da sadelestirirsen zate  5 siyah topum var 4den fazla dersek direkt 5 top demek
# mantikli dusnunce o da %0.000003lerde bisi


phyper(q=4, m=20, n=14, k=5, lower.tail = FALSE)
# 20 mavi 14 yesil top var 5 secim yapcaz 4ten cok mavi gelme olasiligini sorduk
#%5
phyper(q=4, m=20, n=14, k=10, lower.tail = FALSE)
# 20 mavi 14 yesik top var 10 secim yapcaz 4ten cok mavi gelme olasiligini sorduk
#%85

# 10000 kisi var , 5000 kisi ihtiyac sahibi 3000 kisiye 1 kere gosterilecek 
# en az 1500 kisi gorme ihtimali
phyper(q = 1499, m=5000, n=5000, k=3000, lower.tail = F)
#0.5 ihtimal


qhyper(p = 0.7, m = 20, n=14 , k=10, lower.tail = T)
# yuzde 70 ihtimalle 20 10 secimden 7 veya daha azi mavi olurmus.

qhyper(p = 0.5, m= 30, n=45, k=20, lower.tail = F)
# yuzde 50 ihtimalle 30 mavi 45 yesil top aras??nda 20 secimde 8den fazlasi mavi gelirmis

qhyper(p = 0.99, m= 30, n=45, k=20, lower.tail = F)
# yuzde 99 diyerek daha gercekci bi sayi istedik ve 4den fazla gelir dedi



rhyper(nn=20,m=24,n=42,k=10)
# 20 kere deneme yapiyor. her denemede 10 top cekiyor. 24 mavi 42 yesil arasindan
# 10 denemede mavi gelme sayilarini veriyor.  


###### uniform dagilim#####

?dunif 

dunif(x = 3, min = 0, max = 6)
# zar ornegidir. 0 ve 6 arasinda 3 gelme olasilgi nedir
# 1/6 
#hepsinin gelme olasiligi ayni olan dagilimdir

dunif(x=5, min = 0,max = 10)
dunif(x=4, min = 0,max = 10)
dunif(x=3, min = 0,max = 10)
dunif(x=2, min = 0,max = 10)
dunif(x=1, min = 0,max = 10)
# hepsi 0.1dir. uniform dagilim budur.



plot(1:40, dunif(1:40, min = 1, max = 40))
# duz bir cizgi verdi. tum olasiliklar ayni cunku

punif(q = 4, min = 0,max = 6,lower.tail = TRUE)
# zar att??n 4 veya daha az gelme olasiligi
#ayni aslinda su demek;

dunif(x=4,min = 0,max = 6)+
dunif(x=3,min = 0,max = 6)+
dunif(x=2,min = 0,max = 6)+
dunif(x=1,min = 0,max = 6)
# bu ile ustundeki ayni anlamdadir



punif(q=5, min = 0,max = 20,lower.tail = TRUE)
# 20 sayidan 5 ve daha azinin gelme olasiligi

punif(q=5, min = 0,max = 20,lower.tail = FALSE)
# 5den fazlasinin gelme olasiligi


qunif(p= 0.6, min = 0,max = 6,lower.tail = T)
#%60 tahmin 3.6 veya daha az gelir diyo

qunif(p= 0.6, min = 0,max = 30,lower.tail = F)
# % 60 tahmin 12den fazla gelir

qunif(p= 0.8, min = 0,max = 30,lower.tail = F)
# 6dan fazla gelir %80

runif(n=50,min = 0,max = 100)
# 50 deneme yaptik 0 ile 100 arasinda 50 tane sayi verdi

d <- runif(n=50,min = 0,max = 100)
d
as.integer(d) #yuvarlamaz sadece integer yeri alir
round(d) #yuvarlar ve tamsayi verir




###### ustel (exponential) dagilim#####

?dexp

#otobus 10 dakkada bi geliyomus. gelmesinin bir dakikadan daha
#uzun surme ihtimali nedir diye sorduk. poisson ile
dpois(x=0, lambda = 1/10)
# %90 1 dakika sonra otobus gelir



#otobus 10 dakkada bi geliyomus. gelmesinin bir dakikadan daha
#uzun surme ihtimali nedir diye sorduk. ustel ile
pexp(q=1, rate = 1/10, lower.tail = FALSE)
# %90 1 dakikadan sonra gelir
#rate ile lambda ayni


dexp(x=2, rate = 1/10)
#otobusun gelme ihtimali 2. dakika surmesi 
dexp(x=3, rate = 1/10)
# 3. dakikada gelme olasiligi daha da dustu
dexp(x=4, rate = 1/10)
dexp(x=5, rate = 1/10)
#olasilik gitgide dusuyor

plot(1:10, dexp(x=1:10, rate = 1/10))
#otobusub dakikalar bazinda gelme olasiliklari.
# 1. dakikada gelme olasiligi daha yuksekken 10. dakikada en dusuk oluyor

# soyle dusunelim. duraga gittik ve otobus belki de coktan kalkti
# tam diger otobus gecmisse 10 dakika sonra digeri gelecek seklinde 
# dusunelim. #yani 10 dakka sonra zaten kesin gelcek. 1 dakika bekkeyince
# gelme olasiligi dedigimiz sey bu yuzden yuksek. zaman surekli akiyor

pexp(q =5, rate = 1/20, lower.tail = FALSE)
# 5 dakikadan sonra gelme olasiligi

pexp(q =1, rate = 1/20, lower.tail = FALSE)
# 1 dakikadan sonra gelme olasiligi


pexp(q =19, rate = 1/20, lower.tail = FALSE)
# 19 dakikadan sonra gelme olasiligi

pexp(q =19, rate = 1/20, lower.tail = TRUE)
# 19 veya daha kisa surede gelme olasiligi


qexp(p = 0.7, rate = 1/20, lower.tail = FALSE)
# %70 ihtimalle 7.13. dakikadan sonra gelirmis otobus

qexp(p = 0.2, rate = 1/20, lower.tail = TRUE)
# %20 olasilik??la 20 dakikada bi gelen otobus 4.5 dakkadan once gelir

qexp(p = 0.9, rate = 1/20, lower.tail = TRUE)
# %90 olasilikla 46. dakikadan az surede gelir. yani 46 dakikadan
# once kesin gelir demek



rexp(n=20, rate = 1/10)
# 10 dakikada 1 gecen arabalarin tahmini olarak 20 kere kacinci dakkalarda gecer




##### normal dagilim #####

?dnorm
dnorm(x = 9, mean = 10, sd = 2)
# ortalamasi 10 standart sapmasi 2 olan bi yerde
# 9 sayisinin gelme olasiligi %17

dnorm (x = 20, mean = 20, sd = 1)
# ortalamam 20 standart sapmam 1 olam yerde 
# 20 gelme olasiligim %40

dnorm (x = 50, mean = 20, sd = 1)
# 50 gelme olasiligi nerdeyse sifir. cok kucuk ihtimal verdi
# sifirin yanina virgulden sonra 196 tane daha sifit atti

plot(1:60, dnorm(x = 1:60, mean = 30, sd = 5),
     bty = 'L',
     pch = 19)
#1den 60a kadar sayi var xde. o sayilarin gelme olasiligi
# yde yaziyor. normal dagilmis

pnorm(q = 5, mean =10, sd = 2, lower.tail = TRUE)
# ortalamasi 10 standart sapmasi 2 olan bi yerde 5 veya daha kucuk
# sayi gelmne olasiligim 0.006 

pnorm(q = 5, mean =10, sd = 2, lower.tail = FALSE)
# 5den buyuk sayi gelme olasiligim %99.4 geldi


qnorm(p = 0.7, mean = 10, sd = 2, lower.tail = FALSE)
# % 70 ihtimalle ortalama 10 standart sapma 2 olan yerde 
# 9dan buyuk sayi gelir

qnorm(p = 0.7, mean = 10, sd = 2, lower.tail = TRUE)
# 11 ve 11den kucuk sayi gelr

qnorm(p= 0.70 , mean = 180, sd = 10, lower.tail = FALSE)
# %70 ihtimalle 174den uzun ogrenci/sayi

qnorm(p= 0.80 , mean = 180, sd = 10, lower.tail = FALSE)
# %80 ihtimalle 171den uzun ogrenci/sayi


qnorm(p = 0.9, mean = 10, sd = 2, lower.tail = TRUE)
# %90 12.5den kucuk gelir. kesinlik arttikca sayi buyudu cunku


qnorm(p = 0.9, mean = 10, sd = 2, lower.tail = FALSE)
# %90dan 7.5dan buyuk sayi verir



rnorm(n=20, mean = 2, sd = 1)
# bana 20 sayi ver. hepsinin ort 2 strandart sapma 1 olsun dedik



# ornek

pnorm(q=160 , mean = 180, sd = 10, lower.tail = FALSE)
# rastgele sectigim birinin 160 cmden uzun olma olasiligi
# %97.8 dedi

pnorm(q=160 , mean = 180, sd = 10, lower.tail = TRUE)
# 160dan daha az olmasi
# %2.2 dedi


pnorm(q=170 , mean = 180, sd = 10, lower.tail = TRUE)
# 170den daha az olma olasiligi
# %15.8


pnorm(q=180 , mean = 180, sd = 10, lower.tail = F)
# 180den daha fazla olma olasigli
# %50 dedi




##### standart normal dagilim #####

# ortalama 0 standart sapma 1
# yani veriler -1 ile 1 arasindadir

dnorm(x = 20, mean = 15, sd = 5)
# normal dagilim


dnorm( x = 20, mean = 0, sd = 1)
#standart normal dagilim
dnorm( x = 0.6, mean = 0, sd = 1)
dnorm( x = -0.6, mean = 0, sd = 1)
# ayni olasiliktir ikisi


vec <- seq(0,1, by=0.001)

plot(vec, dnorm(x = vec, mean = 0, sd = 1),
     bty='L',
     pch=19)
# normal dagilimin sadece sag tarafi gibi cunku 0-1 arasini yazdirdik


vec2 <- seq(-1,1, by=0.001)

plot(vec2, dnorm(vec2, mean = 0, sd = 1), 
     bty='L',
     pch = 19)
# -1,1 arasi dedik normal dagilim grafigini verdi



rnorm(n = 100, mean = 0, sd = 1)
# 100 tane standart normal dagilan sayi uret dedik

##### merkezi limit teoremi#####

z <- c(10,15,14,18,10,23,23,23,56,34,13,19,19,45,45,34)

hist(z)
# normal dagilmayan bi grafik elde ettik. saga carpik yani verilerin cohgu solda

# rastgele ornek secimi
sample(z, size = 4)

# ortalama vectoru olusturma
sonuc <-  numeric(50)
for (i in 1:50) {
  ornek <- sample(z, size = 4)
  sonuc[i] <- mean(ornek)
  
}
sonuc


par(mfrow = c(1,2))
hist(z)
hist(sonuc)

#normal dagilim gosteren bi grafige dondu.
 

##### sinav #####
#M????terilerin web sitesinde bulunan reklama t??klama olas??l?????? 0.6 d??r. 
#Bu durumda sayfay?? ziyaret eden 500 m????teriden en az 300'n??n 
#bu reklama t??klama olas??l?????? nedir? 
#L??tfen bu i??lemi R ??zerinde yap??n??z ve do??ru se??ene??i i??aretleyiniz.
pbinom(q=300, size = 500, prob = 0.6, lower.tail = FALSE)


pbinom(q=100, size = 700, prob = 1/10, lower.tail = FALSE)+ #canta
pbinom(q=100, size = 700, prob = 1/6, lower.tail = FALSE) #ayakkabi



pexp(q=10, rate = 1/6,lower.tail = TRUE)
pexp(q=10, rate = 1/2,lower.tail = TRUE)

abline(lm(mtcars$cyl~mtcars$gear))


pexp(q=3, rate = 1/5, lower.tail = TRUE)

pexp(q=6, rate=1/5, lower.tail = FALSE)

dexp(x=8, rate = 1/5)





