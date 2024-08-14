##### isaret testi (sing test) ####
install.packages("BSDA")
library(BSDA)

df <- student_placement_data_with_NA


View(df)

View(df[1:14])

library(mice)
library(VIM)


dolurma <- mice(df,m = 10, maxit=10, method = "pmm")

doldurulmus <- complete(dolurma,10)

View(doldurulmus)
# verilerimizi doldurdum. burasi bi veri seti olusutup verilerimizi analkiz etmek icindi


df <- doldurulmus[1:14]
# sadece sayisal verileri aldim

df <- as.data.frame(df)
# test edebilmek icin data.frame olmalidir

pdeger <- numeric(length = 10)
for (i in 1:10) {
  orneklem <- sample(df$Acedamic.percentage.in.Operating.Systems, 300)
  test <- shapiro.test(orneklem)
  p <- test$p.value
  pdeger[i] <- p
}
mean(pdeger)
# normal dagilmayan veri. isaret testi kullanilir

hist(df$Acedamic.percentage.in.Operating.Systems)
# cok da belli normal dagilmadigi

#BSDA kutuphanesinden
SIGN.test(df$Acedamic.percentage.in.Operating.Systems,md = 70,alternative = "greater",conf.level = 0.95)
# Isletim Sistemlerinde Akademik Yuzde degiskeninin verilerini analiz ediyoryuz
# hipotezlerimi olusturayim. alternative greater yani buyuktur demisiz
# H0: Isletim Sistemlerinde Akademik Yuzde degerinin ortanda degeri 70den kucuk veya esittir
# Ha: Isletim Sistemlerinde Akademik Yuzde degerinin ortanca degeri 70den buyuktur
# calistirdim ve p.value 0.05den cok cok kucuk bi sayi geldi bu da H0 reddedilip Ha kabul edildi diyor
# yani ortanca degerim bekledigim degerden buyukmus bunu da soyle yorumlayabiliriz
# bekledigim deger 70 ve biz gercek ortanca degeri bilmedigimizi varsayalim. yani burdaki puanlarimizin cogu 70den cokmus
# basari oranini buna gore b??analiz edebiliriz. gecme skoru 70 varsaydigimiz bi yerde dersi gecenlerin dersi gecmeyenlerden cok oldugunu
# bu sekilde bi analiz ile gostermis olduk. Isletim Sistemlerinde Akademik Yuzde degiskeninin bir de grafigini cizip yorumlayalim



par(mfrow=c(1,2))
hist(df$Acedamic.percentage.in.Operating.Systems,col="orange",prob = F,
     xlab= "Puanlar",ylab="Ogrenci Sayisi Yogunlugu")
hist(df$Acedamic.percentage.in.Operating.Systems,col="orange",prob = T,

       xlab= "Puanlar",ylab="Ogrenci Sayisi Yogunlugu")
lines(density(df$Acedamic.percentage.in.Operating.Systems),lty="dotted",lwd=9)

# hem yogunluk hem de norma?? grafikleri histogram grafigi olarak cizdirdik.
# verilere baktigimizda 60-70 skor arasi alan bir cok kisi bulunuyor. genel degerlerim
# 60-95 arasinda dagildigini gozlemliyorum (95+60)/2 = 77.5 sonucu herkes normal dagilirsa tahmini medyani verebilirdi
# ancak 60-70 arasindaki yogunluk bu 77.5u biraz asagi cekebilir
 

 
# isaret testi icin bir ornek daha yapalim

SIGN.test(df$Percentage.in.Mathematics,md = 65,conf.level = 0.95,alternative = "greater")
# Matematik Yuzdesi degiskeninin verilerini analiz ediyoryuz. Hipotezlerimi yazayim
# H0: Matematik Yuzdesi degerinin ortanda degeri 65den kucuk veya esittir
# Ha: Matematik Yuzdesi degerinin ortanca degeri 65den buyuktur
# p.value 0.05den cok cok kucuk cikti bu durumda H0 yani sifir hipotezim reddedildi Ha kabul edildi
# bu durumda ortanca degerim 65den buyuktur diyebiliriz. bu durumda bizim i??in ba??ar?? skoru 65 varsayal??m
# verilerimizde 65den buyuk veri sayisi 65den kucuk veri sayisindan daha cok diyebiliriz. 


par(mfrow=c(1,2))
hist(df$Percentage.in.Mathematics,col="orange",prob = F,
     xlab= "Puanlar",ylab="Ogrenci Sayisi Yogunlugu")
hist(df$Percentage.in.Mathematics,col="orange",prob = T,
     xlab= "Puanlar",ylab="Ogrenci Sayisi Yogunlugu")
lines(density(df$Percentage.in.Mathematics),lty="dotted",lwd=9)
# burda da grafiklerimiz var ayni sekilde yorumlayabiliriz.


# bu veriler kitaba ait degil ancak kitapta anlatilan signed test kullanilarak yapilmis calismadir.



SIGN.test(df$Percentage.in.Mathematics,md = 85,conf.level = 0.95,alternative = "greater")
# H0: Matematik Yuzdesi degerinin ortanda degeri 85den kucuk veya esittir
# Ha: Matematik Yuzdesi degerinin ortanca degeri 85den buyuktur
# p degerim 1 yani H0 kabul edildi. yorumlayal??m
# basari sayilma skorumun 85 oldugunu varsayalim. ortanca degerimin 85 skorundan kucuk oldugunu ogrendim
# yani 20k skorum var ve bunlarin siralanmis halinde tam ortadaki deger 77. bizim gecme notumuzu da biz 85 belirledik
# ve 85>77 oldugunda dolayi bunu su sekilde yorumlayabiliriz. dersimizi gecemeyen ogrenci sayisi dersimizi gecen ogrenci sayisindan fazladir

# --------------------77--------85-------- bu dogrumuz olsun 77 tam ortadaki deger burdan anladigimiz sekilde 85in alti daha cok diyebilriz.




library(tidyverse)

a <- df %>% select(Percentage.in.Mathematics) %>%filter(Percentage.in.Mathematics >85)
nrow(a)
# 5136
# yukarida md=85 olan isaret testimizin s degeri de bu yani 85den buyuk olan deger sayim 5136
# s degeri burada less greater ya da two.sided hangisi yaparsak yapalim girdigimiz sayidan buyuk olan deger sayisini verir.
SIGN.test(df$Percentage.in.Mathematics,md = 85,conf.level = 0.95,alternative = "less")
# bak yine 5136 verdi

# 20000 verimizden 5136si 85den buyukse burada da yukarida yaptigimiz aciklamanin aynisini yapabiliyoruz. 85den kucuk olanlar daha cogunlukta
# S+ sayisi diyoruz buna


g <- sign.test(df$Percentage.in.Mathematics,mu = 85,alternative = "less")
g$p.value
# 0 verdi.
# H0: ortanca deger 85den buyuk veya esittir
# ha: ortanca deger 85den kucuktur
# H0 reddedildi Ha kabul edildi
# kitaptan bakarak yaptigimiz fonksiyonlari kullandik ve ayni sonucu verdi



g <- sign.test(df$Percentage.in.Mathematics,mu = 79,alternative = "greater")
g$p.value


n <- 9
s <- 4

res <- signtest.prob(n=n,s=s,alternative = "two.sided")
res$p.value
#1 verdi
# n ortanca deger atilinca elde ettigim tum veri sayisi,
# s minimum s+- sayilarindan olan s+=4,s-=5 idi.


# isaret testi icin gerekli kutuphaneler

install.packages("BDSA") #SING.test
install.packages("DescTools") #SignTest
install.packages("signmedian.test") #signmedian

library(DescTools)
library(signmedian.test)


##### wilcoxon sira sayilar testi #####

x <- c(5,3,4,2,4,2,2,4,1,2)
# ciftcilerin ankete verdigi ceavplar bunlardir. eski ankete verdikleri cevabin ortanca degeri 3 idi

# ciftcilerin goruslerinin artmasi ihtimalini arastiriyoruz
wilcox.test(x,mu = 3,alternative = "greater",exact = F,conf.int = T,conf.level = 0.95)
# H0: ciftcilerin gorusleri artmamistir(ortanca degerim 3 veya daha azdir)
# Ha: ciftcilerimin gorusleri artmistir(ortanca degerim 3den buyuktur)
# p degerim 0.62 yani ciftcilerimin gorusleri icin olumlu yonde bir degisim var diyemeyiz

# exact kesinlilk degerimdir. hata veriyor diye false yaptim

# ciftcilerimin goruslerinin azalma ihtimali
wilcox.test(x,mu=3,alternative = "less",exact = F,conf.int = T,conf.level = 0.95)
# H0: ciftcilerimin gorusdu azalmamistir(ortanca degerim 3veya buyuktur)
# Ha: ciftcilerimin gorusu azalmistir(ortanca degerim 3ten kucuktur)
# p degerim 0.42 yani ciftcilerimin goruslerinin olumsuz yonde degistigini soyleyemeyiz


wilcox.test(x,mu = 3,alternative = "two.sided",exact = F,conf.int = T,conf.level = 0.95)
# H0: ciftcilerin goruslerinde degisim olmamistir(ortanca deger 3e esittir)
# Ha: ciftcilerin goruslerinde degisim olmustur(ortanca deger 3e esit degildir)
# p degerim 0.85 ciftcilerin goruslerinde bir degisim olmamistir diyoruz.

# hoca kitapta yorumlari bu sekilde yapmaktadir ona gore yorumlara bu sekilde anlam katmayi ogren


a <- wsrt.exact(x,mu = 3,alternative = "two.sided",conf.level = 0.95)
a$p.value
# kendi yaptigimiz wilcoxon testi. p value 0.91 cikti bunda
# H0: ciftcilerin goruslerinde degisim olmamistir(ortanca deger 3e esittir)
# Ha: ciftcilerin goruslerinde degisim olmustur(ortanca deger 3e esit degildir)
# H0 kabul edildi. ciftcilerin goruslerinde degisim olmamistir.

a$pop.med.CI
# guven araligidir.
a$wneg
# negatif sayilar toplami
a$wpos
# pozitif sayilar toplami
a$n
# toplam degisken sayisi, m=m0'a esit olanlar haric
a$n0
# m=m0'a esit olan sayilar toplami
a$w
# alternative teste gore secilecek olan negatif toplamlar veya pozitif toplamlar
# two.sided ise min(pozitif,negatif)
# less ise pozitif toplamlar
# greater ise negatif toplamlari verir

# alistirmalar
x <- c(1,1,2,3,3,3,4,5,5,6,6,7,8,10,21,22,25,27,33,40,42,50,55,75,82)

library(BSDA)
SIGN.test(x = x,md = 30,alternative = "less",conf.level = 0.95)
# H0: x vectorunun ortanca degeri 30dan kucuk degildir
# Ha: x vectorunun ortanca degeri 30dan kucuktur
# p.value 0.02 H0 reddedildi Ha kabul edildi.
library(DescTools)
SignTest(x = x,mu = 30,alternative = "greater",conf.level = 0.95)
# H0: x vectorunun ortanca degeri 30dan buyuk degildir
# Ha: x vectorunun ortanca degeri 30dan buyuktur
# p.value 0.99 H0 kabul edildi. 

# kendi olusturudugumuz fonksiyon. fonksiyonlar klasorumde
# calismasi icin ayni klasrodeki signtest.prob'u da calistirmak gerek
sign.test(x = x,mu = 30,alternative = "two.sided",conf.level = 0.99)
# H0: x vectorunun ortanca degeri 30a esittir
# Ha: x vectorunun ortanca degeri 30a esit degildir
# p degerim 0.04 H0 reddedildi




x1 <- c(2.5,2.4,3.1,3.0,2.3,2.6,1.5,3.3,3.1,2.5,2.8,3.3,3.2,2.6,3.4)

# veri sayim 30dan az oldugundan andersondarling kullandik
AndersonDarlingTest(x1)
# H0: x veri setim normallik gosteriyor
# Ha: x veri setim normallik gostermiyor
# p degerim 0'a cok yakin H0 reddeidldi Ha kabul edildi

library(car)
durbinWatsonTest(x)
# H0: bagimsiz verilerdir. otokorelasyon etkisizdir.
# Ha: bagimli verilerdir. otokorelasyon etkilidir
# p.value 0.06 H0 kabul edildi


hist(x,probability = T,col = "orange",main = "E.coli Bakteri Sayilari")

# gorsel uzerinde pek normal gibi durmuyor

# Varsayimlar: Normallik gostermiyor. Bagimsizlik var
# signtest kullanacagiz

# yine de ttestina bi bakalim
t.test(x = x, mu = 3.5, alternative = "less",conf.level = 0.95)
# H0: bakteri sayimda dusus yoktur
# Ha: bakteri sayimda dusus vardir
# p.value 0a cok yakin H0 reddedildi Ha kabul edildi
# bakteri sayimda dusus varmis

SignTest(x = x, mu = 3.5, alternative = "less",conf.level = 0.95)
# H0: bakteri sayimda dusus yoktur
# Ha: bakteri sayimda dusus vardir
# p.value cok cok ufak sekilde artti ama yine de onemsiz derecede H0 reddedildi Ha kabul edildi

install.packages("fuzzyRankTests")
library(fuzzyRankTests)
# bulanik isaret testi oluyor bu
fuzzy.sign.test(x = x,alternative = "less",mu = 3.5)
# H0: bakteri sayimda dusus yoktur
# Ha: bakteri sayimda dusus vardir
# SignTest ile ayni sonucu verdi



x <- c(76,87,85,94,60,73,64,76,97,59,53,63,87,100,92)
# fonksiyonlar kutuphanedem ekledim
wsrt.exact(x,mu = 70,alternative = "two.sided",conf.level = 0.95)
# H0: x vectorunun ortanca degeri 70dir
# Ha: x vectorunun ortanca degeri 70den farklidir
# p degerim 0.1 H0 kabul edildi


# 7 puanli anket sistemi almanlarda ortanca deger 5 imis bu puanlar turklerin
# aralarindaki farki olc. varsayim testlerini yap once
puan <- c(1,3,1,7,5,5,5,1,2,2,7,5,4,4,5,2,5,5,7,5)

hist(puan)
# normallik gostermiyor gibi

# veri sayim 30dan az oldugundan andersondarling kullandik
AndersonDarlingTest(puan)
# p degerim 0a cok yakin normallik gostermiyor.

durbinWatsonTest(puan)
# 0.30 veriler bagimsiz

# isaret (sign) testi mi yoksa wilcoxon testi mi?
# wilcoxon kullanacagiz sira sayilari testi uygulamamiz gerek
# dagilim simetrik

wilcox.test(puan,mu = 5,alternative = "two.sided",conf.level = 0.95,conf.int = T,exact = F)
# H0: Turkler ile Almanlar ayni dusunceye sahip
# Ha: Turkler ile Almanlar farkli dusunceye sahip
# p degerim 0.048 0.95 dogruluk oraninda H0 reddedildi Ha kabul edildi


# aralik ayi adana yagis kg/m2 degeri. uzun yillardir ortanca 118.2 imis 
# haberlere artti demislerler kontrol et bakalim
yagis <- c(119.2,110.7,150.1,201.5,308.5)


# veri sayim 30dan az oldugundan andersondarling kullandik
AndersonDarlingTest(yagis)
# p degerim 0.00012 normallik gostermiyor degerlerim

SignTest(yagis,mu = 118.2,alternative = "greater")
# H0: son 5 yilda yagis miktarimda artis olmadi
# Ha: son 5 yilda yagis miktarimda artis oldu
# p degerim 0.18 H0 kabul edildi. son 5 yilda yagis miktarimda artis olmamis

# isaret testi kullanma sebebim simetrik bi dagilim gostermemesi


##### eslestirilmis orneklem testleri ####

## iki orneklem eslestirilmis isaret testi
library(BSDA)

kediOnce <- c(157.5,84.5,134.0,74.0,108.0,107.5,106.0,163.0,54.0) # once durgun suda 2 gun bekleyen kedilerin su icme miktari
kediSonra <- c(164.5,51.5,250.0,139.0,113.0,124.5,95.5,70.5,30.5) # sonra akarsuda 2 gun bekleyen ayni kedilerin su icme miktari

AndersonDarlingTest(kediOnce)
AndersonDarlingTest(kediSonra)
# ikisi de normal dagilim gostermiyor

SIGN.test(kediSonra, kediOnce, alternative = "greater",conf.level = 0.95)
# H0: akarsuda su icme miktarinin ortanca degeri durgun suda su icme miktarindan fazla degildir
# Ha: akarsuda su icme miktarinin ortanca degeri durgun suda su icme miktarindan fazladir
# p degerim 0.5 geldi H0 kabul edildi
# yani akarsuda su icme miktari durgun suda su icme miktarindan fazla diyemeyiz
# bir baskan bakis acisiyla kedilerin akarsuya gecmesi su icme miktarlarini arttirdi diyemeyiz



## wilcoxon testi
wilcox.test(x = kediSonra, y = kediOnce, alternative = "greater", conf.level = 0.95)
# H0: akarsuda su icme miktarinin ortanca degeri durgun suda su icme miktarindan fazla degildir
# Ha: akarsuda su icme miktarinin ortanca degeri durgun suda su icme miktarindan fazladir
# p degerim 0.5
# H0 kabul edildi 
1 - psignrank(22 - 1, 9)

install.packages("exactRankTests")
library(exactRankTests)

wilcox.exact(x = kediSonra, y = kediOnce, alternative = "greater", conf.level = 0.95)
# H0: akarsuda su icme miktarinin ortanca degeri durgun suda su icme miktarindan fazla degildir
# Ha: akarsuda su icme miktarinin ortanca degeri durgun suda su icme miktarindan fazladir
# p degerim 0.5
# H0 kabul edildi




grup1 <- c(39,30,35,35,30,43,48,56,40,34,32,59,43,43,52,52,44,53,52,38) # mantar kompost atigi verilen muz grubu
grup2 <- c(35,25,36,33,25,34,31,39,24,38,29,39,31,31,34,29,38,29,32,24) # ciftlik gubresi verilerm muz grubu
# soru: mantar kompost atigi veriler grubun agirliklari ciftlik gubresine verilen grubun agirligindan fazla midir? %1 onem duzeyinde bulunuz

muz <- data.frame(agirlik = c(grup1,grup2),
                  grup= c(rep(1,20), rep(2,20)))
View(muz)
muz$grup <- as.factor(muz$grup)

shapiro.test(muz$agirlik)
# normal dagilim gostermiyor

wilcox.test(x = muz$agirlik[muz$grup==1], y = muz$agirlik[muz$grup==2],alternative = "greater",conf.level = 0.99)
# H0: mantar kompost atigi verilen muz grubunun agirligi ciftlik gubresi verilenden fazla degildir
# Ha: mantar kompost atigi verilen muz grubunun agirligi ciftlik gubresi verilenden fazladir
# p degerim 0a cok yakin geldi. H0 reddedildi Ha kabul edildi
# yorumu: mantar kompost uygulamasiyla gelistirilen cuce muzlarimin meyve eti agirligi daha fazladir
# ya da mantar kompost uygulamasi cuce muz uzerinde meyve eti agirligini arttirmaktadir


zcetvel()

wilcox.test(agirlik~grup,data = muz,alternative = "greater",conf.level=0.99,
            exact = F, correct = T)
# H0: mantar kompost atigi verilen muz grubunun agirligi ciftlik gubresi verilenden fazla degildir
# Ha: mantar kompost atigi verilen muz grubunun agirligi ciftlik gubresi verilenden fazladir
# p degerim 0a cok yakin geldi. H0 reddedildi Ha kabul edildi