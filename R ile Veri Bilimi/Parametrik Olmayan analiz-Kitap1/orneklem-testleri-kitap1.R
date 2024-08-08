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


