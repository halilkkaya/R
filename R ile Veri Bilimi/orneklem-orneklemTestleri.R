##### normallik ve shapiro wilk testi######

View(iris)


hist(iris$Sepal.Length)
#yaklasik normal gibi duruyor. test yapalim

shapiro.test(x = iris$Sepal.Length)
# p : 0.01 verdi. yani normal degil. 0.05den buyuk olmaliydi



hist(iris$Sepal.Width)
# normal gibi duruyor

shapiro.test(x = iris$Sepal.Width)
# p : 0.1 geldi. yani normal dagilim d??yebiliriz. p, 0.05den buyuk

par(mfrow=c(1,2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
# iki grafigi yanyana actik

###### buyuk veri ve normallik testi#########
df <- StudentsPerformance

nrow(df)
# 1000 satirmis verimiz

summary(df)
# ozet bilgiler

hist(df$math.score)
# biraz sola carpik, sagda cok veri olan bi grafik.


# H0 : dagilim normaldir
# Ha : dagilim normla degildir
# p > 0.05
shapiro.test(df$math.score)
# p : 0.00001 cikti. normal degil diyor bu teste gore
# Ha 


clean <- df$math.score[-which(df$math.score<30)]
hist(clean)
# daha normal dagilim duruyor

shapiro.test(clean)
# 0.003 cikti yani normal degil diyor hala
# sebebi genelde dusuk sayidaki veriler icin uybun. 300 ustu verilerde yaniliyor
# veri sayimizi dusurelim bakalim

# ilk 100 ornek
shapiro.test(clean[1:100])
# 0.80 degerini verdi. yani normal dedi ilk 100 veri icin

# 100 ile 200 arasi
shapiro.test(clean[100:200])
# 0.08 cikti. bu da normal olarak sayiliyor

# 200 ile 300 arasi
shapiro.test(clean[200:300])
# 0.19 yani bu da normal.

#buyuk verilere baktigimizda normal degildi ama kucuk kucuk bakinca normal
# merkezi limit teoremi aklimiza geliyor. verileri grup grup alip ortalamalarini
# alirsak onlar normal dagilan olabilir
# shapiro daha kucuk verilerde kullan. kucuk parcalar al oyle yap


sample1 <- sample(clean, size=100)

shapiro.test(sample1)
# 100 tane sayi aldik ve p = 0.7 cikti yani normal dagilimdir



p <- numeric(length = 100)

for (i in 1:100) {
  
  df <- sample(clean, size = 100)
  st2 <- shapiro.test(df)
  p[i] <- st2$p.value
  ort <- mean(as.numeric(p))
    
}
names(shapiro.test(clean))
# p.value de alinabilir 2 de yazilabilir

p
mean(p)
# 0.48 cikti yani normal dedi
# 2. kez calistirdim 0.44 yine normal dedi

# burda da 30dan kucuk verileri de dahil edip yeniden denedik
p <- numeric(length = 100)
for (i in 1:100) {
  b <-  sample(a$math.score,100 )
  c <- shapiro.test(b)
  d <- c$p.value
  p[i] <- d
  
}
mean(p)
# p degeri 0.36 geldi ortalama olarak. yani H0 kabul edildi.
# normal dagilim dedik



##### tek orneklem t-testi ve ortalama guven araligi#####

# T-testi
# surekli veri olmasi gerek, normal olmasa bile normale yakin daiglim gostermelidir

View(iris)

hist(iris$Sepal.Length)
# yaklasik olarak normal dagilim diyebiliriz. gorsel olarak tahmin yani kabul edilebilir

shapiro.test(iris$Sepal.Length)
# 0.01 cikti. normal degil. ama normale yakin arada 0.04 var tolera edilebilir

df <- iris$Sepal.Length
p <- numeric()
for (i in 1:100) {
  z <- sample(df,100)
  a <- shapiro.test(z)
  b <- a$p.value
  p[i] <- b
}
mean(p) # normal dagilim gosteriyormus
df
?t.test
t.test(x = iris$Sepal.Length, mu = 3, alternative = "two.sided",
       conf.level = 0.95)
# H0: ortalama = test degeri (3)
# Ha: ortalama != test degeri (3)
#p degeri 0.05den kucuk yani h0 hipotezini reddet ha kabul et

# yani ortalama deger testine esit degildir. yani sepal.length ortalamasi 3e esit deigldir
# degerlerimizin ortalamasi  5.709732-5.976934 arasindaymis %95 ile
# mu degeri ortalama demek. yani biz sepalleng ortalamasi 3 mu diye kontrol ettik



sonuc <- t.test(x = iris$Sepal.Length, mu = 3, alternative = "two.sided",
                conf.level = 0.95)

names(sonuc)
sonuc$statistic
# t degerini verdi

sonuc$p.value
# p degerini verdi. cokkk kucukkkkk

sonuc$conf.int
#degerlerimiz 5.70 ile 5.97 arasindaymis
# %95 dogruluk faktoru



t.test(x = iris$Sepal.Length, mu = 3, alternative = "less",
       conf.level = 0.95)

# H0: ortalama >= test degeri (3)
# Ha: ortalama < test degeri (3)
# ortalama 5.84 yabi 3den buyuk H0 kabul ediliyor
# p:1 yani 0.05den buyuk yani H0 kabul
# %95 ihtimalle orta??ama 3den buyuk dedik


t.test(x = iris$Sepal.Length, mu = 3, alternative = "greater",
       conf.level = 0.95)
# H0: ortalama <= test degeri (3)
# Ha: ortalama > test degeri (3)
# ortalama 5.84 yabi 3den kucuk Ha kabul ediliyor
# %95 ihtimalle ortalama 3den buyuk dedik



t.test(x = iris$Sepal.Length, mu = 5.5, alternative = "two.sided",
       conf.level = 0.95)
# H0: ortalama = test degeri (5.5)
# Ha: ortalama != test degeri (5.5)
# p degerin yine cok dusuk yine ha kabul edildi


t.test(x = iris$Sepal.Length, mu = 5.7, alternative = "two.sided",
       conf.level = 0.95)
# H0: ortalama = test degeri (5.7)
# Ha: ortalama != test degeri (5.7)
# p degerim 0.03 yine h0 reddedildi, ha kabul edildi.
# yani ortalamam 5.7ye esit degildir demek


t.test(x = iris$Sepal.Length, mu = 5.8, alternative = "two.sided",
       conf.level = 0.95)
# H0: ortalama = test degeri (5.8)
# Ha: ortalama != test degeri (5.8)
# p degerim 0.52 verdi yani h0 kabul edildi ortalamam 5.8 olarak kabul edildi
# ortalama degerlerimiz 5.70 ile 5.97 arasindaymis mu degerine bu aralikta
# yazarsak hipotezimiz kabul edilecektir


t.test(x = iris$Sepal.Length, mu = 5.9, alternative = "two.sided",
       conf.level = 0.95)
# H0: ortalama = test degeri (5.9)
# Ha: ortalama != test degeri (5.9)

# degerlerimiz 5.70 ile 5.97 arasindaydi ve bunda da h0 kabul etti
# p: 0.4 verdi

# yani mu degerine girdigimiz degeri %95 dogruluk payinda ortalamayla kiyasliyor
# ona gore de degerleri veriyor demek bu test
# buyuk verilerde yanlis sonuclar verebilir. buyuk verilerde ortalama kullanmak 
# sacma gelebilir o yuzden farkli yontemler kullanilabilir




##### buyuk verilerde t testi ve guven faktoru#####


df <-  StudentsPerformance

nrow(df)
# 1000 gozlem var. t testi icin buyuk sayilir


hist(df$math.score)

mean(df$math.score)
# 66.089 ortalamas??


t.test(df$math.score, mu = 70, alternative = 'two.sided', conf.level = 0.95)
# H0: ortalama deger = deger(70)
# Ha: ortalama degeri != deger(70)
# p degeri 0a yakin yani h0 kabul edilmedi. bu da ortalama 70 degerinden farklidir dedi
# 65 ile 67 arasinda ortalaman var dedi  ve bunlarda 70 yok dedi
# buyuk verilerde t testi yanlis sonuc verebilir
# random verilerle yapmak lazim


sample1 <-  sample(df$math.score, size=50)

t.test(sample1, mu=70)
# H0: ortalama = deger(70)
# Ha: ortalama != deger(70)
# p degeri 0.06 geldi yani h0 kabul edildi yani ortalama 70e esittir
# 62 ile 70 arasi guven araligi oldu


sample2 <-  sample(df$math.score, size=50)
t <- t.test(sample2, mu=70)
# H0: ortalama = deger(70)
# Ha: ortalama != deger(70)
# p degerim 0.0005 geldi yani h0 reddedildi. ha kabul edildi yani 70e esit degildir dedik
# 59-66 arasi guven araligi


names(t)
# p.value alincak

# iki farkli test 2 farkli sonuc. bunu daha fazla yapip gelen sonucu alalim

p <- numeric(length = 30)
for (i in 1:30) {
  df1 <- sample(df$math.score, size = 50)
  tt <-  t.test(x = df1, mu= 70)
  p[i] <- tt$p.value

}
p
mean(p)
# 0.19 yani h0 kabul edildi.
# 2. deneme 0.16
# 3. deneme 0.19
# ortalamamiz 70 diyenilriiz gooolll
# soyle bi yontemi de hoca yapti


pt <- character(length = 100)
for (i in 1:100) {
  df1 <- sample(df$math.score, size = 50)
  tt <-  t.test(x = df1, mu= 70)
  pp <- tt$p.value 
  
    if (pp>0.05) {
      pt[i] <- 'KABUL'
    }else{
      pt[i] <- 'RED'
    }
}
pt
table(pt)
# 58 kabul 42 red verdi. yarisindan cogu kabul edilmis demek. bi daha calistiralim
# 54 kabul 46 red
# 58 kabul 42 red

# 100 kere yaptim ve 50 ornek aldim her bi denemede. cogunluk kabul cikti
# yani ortalama degerimiz 70e esittiri kabul etti


# daha dogru sonuclar icin bu sekilde yapmamiz gerek



##### wilcoxen signed rank test #######

View(iris)


hist(iris$Sepal.Length)

shapiro.test(iris$Sepal.Length)
# yaklasik normal


View(warpbreaks)

hist(warpbreaks$breaks)
# saga carpik duruyor. veriler solda yogunlukta

nrow(warpbreaks)
# 54 satir yani shapiro icin yeterli

shapiro.test(warpbreaks$breaks)
# p: 0.0001 verdi yani normal degil diyor

?wilcox.test
# ortalama yerine medyan uzerinden i??lem yapar. carpik dagilimlarda ortalama 
#yerine medya kullanirdik unutma

wilcox.test(x = warpbreaks$breaks, 
            mu = 40, alternative = 'two.sided',
            conf.level = 0.95)
# H0: ortanca deger 40'a esittir
# Ha: ortanca deger 40'a esit degildir
# p degerin 0a cok yakin geldi. yani h0 reddedildi ha kabul edildi.
# yani esit degildir dendi


mean(warpbreaks$breaks)
# ortalamasi 28

median(warpbreaks$breaks)
# medyani 26 cikti


wilcox.test(x = warpbreaks$breaks, mu = 40,
            alternative = 'less')
# H0: ortanca deger 40 veya 40dan buyuktur
# Ha: ortanca deger 40dan kucuktur
# p degerim 0a yakin geldi yani H0 reddedildi Ha kabul edildi.
# 40 degeri ortanca degerden buyuk cikti

median(warpbreaks$breaks)


wilcox.test(x = warpbreaks$breaks, mu = 40,
            alternative = 'greater')
# H0: ortanca deger 40 veya 40dan kucuktur
# Ha: ortanca deger 40dan buyuktur
# p degerim 1 cikti yani H0 kabul edildi.
# 40 degerim ortanca degerden buyuktur


t.test(x = warpbreaks$breaks, mu = 40, alternative = 'less')
# H0: ortalama degeri 40 veya 40dan buyuktur
# Ha: ortalama degeri 40dan kucuktur 
# p degeri 0a yak??n geldi yani Ha kabul edildi. ortalama 40dan kucuktur.
# ortalamamiz 28.14mus zaten
# t testi ile wilcox testi bu veriler icin hemen hemen ayni sonucu verecektir
# sebebi ortalama ile ortanca deger aras??ndaki fark??n az olmas??


shapiro.test(df$math.score)
# normal degil dedi

p <- numeric()
for (i in 1:100) {
  orneklem <- sample(df$math.score, size = 100)
  test <- shapiro.test(orneklem)
  pdeger <- test$p.value
  p[i] <- pdeger
}
mean(p)
# aslinda studentperformance verilerindeki math.score degiskeni normalmis bunu anladik.

median(df$math.score)
# ortanca deger 66

mean(df$math.score)
# ortalamasi da 66

# bi an heyecanlandim farkli sonuclar alabiliriz belki diye ama ayni cikti aq

shapiro.test(df$reading.score)
# bu yakin bunu da bosver

p <-numeric()
for (i in 1:1000) {
  orneklem <- sample(df$reading.score, size = 200)
  test <- shapiro.test(orneklem)
  pdegeri <- test$p.value
  p[i] <- pdegeri
}
p
mean(p)
# 0.16 degeri geldi. 1000 kere yaptik bir de yani bu normal dagilim diyebiliriz.
# aykiri degerler kafanmizi karistirmasin

shapiro.test(df$writing.score)
# aha p baya kucuk bunu deneyelim
# bakalim verileri bolersek ne cikacak

p <- numeric()

for (i in 1:1000) {
  orneklem <- sample(x=df$writing.score, size = 200)
  test <-  shapiro.test(orneklem)
  pdeger <- test$p.value
  p[i] <- pdeger
}
p
mean(p)
# 0.13 verdi. yani normal dagilan diyor

mean(df$writing.score)
# 68 ortalama
median(df$writing.score)
# 69 ortanca deger


t.test(x = df$writing.score, mu = 69, alternative = 'two.sided')
wilcox.test(x = df$writing.score, mu = 69, alternative = 'two.sided')

# yine  hemen hemen ayni sonuclari elde ettik. 
# ama sonuclari yaklastirdikca wilcox daha dogru yanitlar verdi



t.test(x = df$writing.score, mu = 69, alternative = 'less')
wilcox.test(x = df$writing.score, mu = 69, alternative = 'less')

# mesela burda aradaki fark az ama wilcox dogru cikti 

##### homojen varyanslilik bartlett testi #####

View(iris)

df <-  subset(iris, subset = (Species != 'setosa'))

df # sadece versicolor ve virginicayi aldik

boxplot(df$Sepal.Width~ as.character(df$Species))

?bartlett.test


bartlett.test(x = df$Sepal.Width, g = df$Species)
bartlett.test(df$Sepal.Width~ as.character(df$Species))
#iki giris sekli de olur

# H0: varyanslar homojendir
# Ha: varyanslar homojen degildir
# p  degerimiz 0.85 geldi
# yani homojendir. H0 kabul edildi



boxplot(df$Sepal.Length~as.character(df$Species))
bartlett.test(df$Sepal.Length~as.character(df$Species))
# H0: varyanslar homojendir
# Ha: varyanslar homojen degildir
# p degerimiz 0.14 geldi
# homojendir. H0 kabul edildi

shapiro.test(df$Sepal.Width[df$Species=='versicolor'])
# 0.33 verdi nomral verdi versicolor icin



shapiro.test(df$Sepal.Width[df$Species=='virginica'])
# 0.18 verdi normal dedi virginica icin

# veriler surekli olcak ve normal dagilim olucak o zaman bartlett kullanilabilir.


####### homojeb varyansl??l??k levene's test ########

# tek bagimsiz degiskene yapilir. normal olmasa bile iyi sonuc verebilir


install.packages("car")

library(car)

df <- subset(iris, subset = (Species != 'virginica'))
df

boxplot(df$Sepal.Width~as.character(df$Species))
# yaklasik varyans ayni diyebilirz

leveneTest(df$Sepal.Width~as.character(df$Species))

# H0: varyanslar homojendir
# Ha: varyanslar homojen degildir
# p degeri 0.44 yani homojendir H0 kabul edildir

shapiro.test(df$Sepal.Width[df$Species=='versicolor'])
shapiro.test(df$Sepal.Width[df$Species=='setosa'])
# ikisi de normal cikti

leveneTest(df$Sepal.Width~as.character(df$Species))
bartlett.test(df$Sepal.Width~as.character(df$Species))
# levene p degeri 0.44 bartlett 0.18 cikti
# p degeri ne kadar yuksekse o kadar homojen demektir
# leveneye gore daha homojendir diyor.
# bartlett daha keskindir cunku normallige onem veriyor

##### homojen varyanslilik fligner killeen testi #####

# normal dagilim degilse ya da kesikliyse kullanabilirsin

View(warpbreaks)

shapiro.test(warpbreaks$breaks[warpbreaks$wool=='A'])
# normal degildir. p 0.01
shapiro.test(warpbreaks$breaks[warpbreaks$wool=='B'])
# normal degildir. p 0.03
# ikisi de normale yakin ama normal degildir

boxplot(warpbreaks$breaks~warpbreaks$wool)


fligner.test(warpbreaks$breaks~warpbreaks$wool)
# H0: varyanslar homojendir
# Ha: varyanslar homojen degildir
# p degeri 0.24 yani H0 kabul edildi. homojen
# normallige duyarli degildir

library(car)

leveneTest(warpbreaks$breaks~warpbreaks$wool)
# H0: varyanslar homojendir
# Ha: varyanslar homojen degildir
# p degeri 0.12 yani H0 kabul edildi. homojen
# p degerinin daha dusuk cikma sebebi normallige daha duyarlidir


bartlett.test(warpbreaks$breaks~warpbreaks$wool)
# H0: varyanslar homojendir
# Ha: varyanslar homojen degildir
# p degeri 0.008. Ha kabul edildi. homojen degildir
# sebebi ise normal dagilmayan oldugu icin boyle bir sonuc cikti 



# hangi veriler icin hangi testi kullanman gerektigini bilmen gerek
# once normallik testi yap. 
# normalligi cok yuksekse bartlett kullan
# normallik derecesi dusuk veya yakinsa levene de kullan??labilir
# normal degilse fligner testini kullan
# arada kalirsan 3unu de kullan ona gore degerlendir bakalim.


##### bagimsiz iki orneklem t testi uygulamasi #####

View(iris)

df <- subset(iris, subset = (Species != 'virginica'))
class(df$Species)
df$Species <-  as.character(df$Species)
class(df$Species)
View(df)

par(mfrow =c(1,2))
hist(df$Sepal.Width[df$Species=='setosa'])
hist(df$Sepal.Width[df$Species=='versicolor'])


shapiro.test(df$Sepal.Width[df$Species=='setosa'])
# H0 normal dagilimdir
# Ha normal dagilim degildir
# p degeri 0.27 yani normal dedik

shapiro.test(df$Sepal.Width[df$Species=='versicolor'])
# p degeri 0.33 yani normal dedik

# 1. varsayim tamamdir. iki grup da normal dagiliryor



bartlett.test(df$Sepal.Width~df$Species)
# H0 homojen varyansl??d??r
# Ha homojen varyansl?? degildir
# p degeri 0.18 yani homojen varyanslidir


# 2. varsay??m homojen varyanslilik tamamdir.


?t.test

t.test( formula = df$Sepal.Width~df$Species, mu=0, var.equal = T)
# mu = 0 ikisi esit mi demek icin
# var.egual varyanslari homojense T yap

# H0: iki grup ortalamasi birbirine esittir = farki 0dir
# Ha: iki grup ortalamasi birbirine esit degildir
# p degerim 0a yakin geldi yani esit degildir dedik.
# iki grubun arasindaki fark %95 ihtimalle 0.51 ile 0.79 arasinda olacak demektir
# setoda ort 3.42, versicolor ort 2.770



t.test( formula = df$Sepal.Width~df$Species, mu=0.7, 
        var.equal = T)
# H0: iki grup arasindaki ortalama farki 0.7ye esittir
# Ha: iki grup arasindaki ortalama farki 0.7ye esit degildir
# p degerim 0.54 verdi. yani aradaki fark 0.7 diyebiliriz
# setosa versicolordan 0.7 daha fazla ort sahip diyebiliriz


t.test( formula = df$Sepal.Width~df$Species, mu=2, 
        var.equal = T,alternative = 'less')
# H0: iki grubun ortalamasi arasindaki fark 2den buyuk veya esittir
# Ha: iki grubun ort arasindaki fark 2den kucuktur
# p degeri 0a yakin geldi yani Ha kabul edildi

t.test( formula = df$Sepal.Width~df$Species, mu=2, 
        var.equal = T,alternative = 'greater')
# H0: iki grubun ortalamasi arasindaki fark 2den kucuk veya esittir
# Ha: iki grubun ort arasindaki fark 2den buyuktur
# p degerim 1 geldi yani H0 kabul edildi.




t.test( formula = df$Sepal.Width~df$Species, mu=0.7, 
        var.equal = T,alternative = 'greater')
# H0: iki grubun ort aras??ndaki fark 0.7den kucuk veya esittir
# Ha: iki grubun ort arasindaki fark 0.7den buyuktur
# p degeri 0.7 geldi yani H0 kabul edildi

# ayni zamanda su sekil kullanim da bulunuyor

t.test(x = df$Sepal.Width[df$Species=='setosa'],
       y = df$Sepal.Width[df$Species=='versicolor'],
       mu = 2,
       var.equal = T,
       alternative = 'less')
# H0: iki grubun ortalamasi arasindaki fark 2den buyuk veya esittir
# Ha: iki grubun ort arasindaki fark 2den kucuktur
# p degeri 0a yakin geldi yani Ha kabul edildi

t.test(x = df$Sepal.Width[df$Species=='setosa'],
       y = df$Sepal.Width[df$Species=='versicolor'],
       mu = 2,
       var.equal = T,
       alternative = 'greater')
# H0: iki grubun ort aras??ndaki fark 2den kucuk veya esittir
# Ha: iki grubun ort arasindaki fark 2den buyuktur
# p degeri 1 geldi yani H0 kabul edildi



##### non-parametric - wilcoxen rank sum test (sirali toplamalar)######
# bagimsiz iki orneklem t test normal dagilim icindi
# bagimsiz iki orneklem eger normal dagilim degilse wilcox testi kullanilir

# normal dagilmayan veriler uzerinde kullanilabilir

View(warpbreaks)
df <- warpbreaks

# dagilimlari inceleyelim
shapiro.test(df$breaks[df$wool=='A'])
# 0.01 yani normla dagilim degildir

shapiro.test(df$breaks[df$wool=='B'])
# 0.03 yani normal dagilim degildir

p <- numeric()

?wilcox.test

for (i in 1:1000) {
  orneklem <- sample(df$breaks, size = 10)
  test <- shapiro.test(orneklem)
  pdegeri <- test$p.value
  p[i] <- pdegeri
}
p
mean(p)
# normal dagilim oldu bu sekilde.


library(car)

leveneTest(df$breaks~df$wool)
# 0.12 veri yani homojen varyans d??yebiliriz

bartlett.test(df$breaks~df$wool)
# 0.008 buna gore homojen degil ama bu test veriler normalse kullaniliyordu

fligner.test(df$breaks~df$wool)
# normal olmayan verilerin homojenligini sorguluyordu. 
# p degeri 0.24 yani homojen diyebiliriz.

# 2 test homojen 1 test degil dedi homojen sayabiliriz.

# 1) normal dagilim gostermiyo
# 2) homojenlik sagliyor / normal dag??ilim gostermemesi yetiyor


?wilcox.test

wilcox.test(df$breaks~df$wool, mu = 0)

wilcox.test(x=df$breaks[df$wool=='A'],
            y = df$breaks[df$wool=='B'], mu=0,
            conf.int = T)

# ikisi de ayni sonucu verir.
# wilcox testi ortalama degil medyani baz alirdi. normal olmayan degerler icin bu
# boyleydi unutma.
# H0: Gruplarin lokasyonlar??n??n kaymas?? 0'a esittir
# Ha: Gruplarin lokasyonlar??n??n kaymas?? 0'a esit degildir
# p degeri 0.25 dedi
# yani ikisi degerin ortanca degerleri esit diyebiliriz. 
# guven araligim -3 ile 10 arasindaidir

wilcox.test(x=df$breaks[df$wool=='A'],
            y = df$breaks[df$wool=='B'],
            mu = 15,
            conf.int = T)
# H0: Gruplarin lokasyonlar??n??n kaymas?? 15'e esittir
# Ha: Gruplarin lokasyonlar??n??n kaymas?? 15'e esit degildir
# p degeri 0.001 yani H0 hipotezim reddedil??istir. Ha kabul edilmistir

wilcox.test(x=df$breaks[df$wool=='A'],
            y = df$breaks[df$wool=='B'],
            mu = 15,
            conf.int = T,
            alternative = 'less')
# H0: iki grup lokansyonlari arasindaki kayma 15e e??it veya buyuktur
# Ha: iki grup lokansyonlari arasindaki kayma 15den azdir
# h0 reddedildi. Ha kabul edildi.

wilcox.test(x=df$breaks[df$wool=='A'],
            y = df$breaks[df$wool=='B'],
            mu = 8,
            conf.int = T,
            alternative = 'greater')
# H0: iki grup lokansyonlari arasindaki kayma 8den kucuk veya esittir
# Ha: iki grup lokansyonlari arasindaki kayma 8den fazladir
# p degeri 0.9. H0 kabul edildi


# alternative hypothesis yazan kisim Ha icin gecerli kisimdir unutmaaaaaaaa
# cok onemli bi detaydir burasi




##### bagimli iki orneklem t-testi uygulamasi#####

ins <- InsPostComparison

duration <- c(ins$RPostsDuration,ins$LPostsDuration)
duration

groups <- c(rep('rastgele',50), rep('begenilen',50))
groups

df <- data.frame('duration'=duration, 'group'=groups)
View(df)

# gruplara ayirabilmek icin bi grup kismi bi sayisal kisim yaptik

shapiro.test(df$duration[df$group=='rastgele'])
# 0.07 dedi yani normal dagilim
shapiro.test(df$duration[df$group=='begenilen'])
# 0.05 dedi yani normal dagilim

bartlett.test(x = df$duration,
              g=df$group)
# p degerim 0.54 cikti. yani varyans homojendir

t.test(df$duration~df$group, mu=0, paired = T)

# paired eger bagimli degiksen varsa TRUE olarak isaratlenecek
# ve cok dikkat paired kullanmak icin gruplari bu sekilde ayirman gerekecek
# formul ile yazdiginda paired gecersiz diyor



group1 <- df$duration[df$group == 'begenilen']
group2 <- df$duration[df$group == 'rastgele']

t.test(group1, group2, mu = 0, paired = TRUE,var.equal = T)
# H0: begenilen ve rastgele arasindaki fark 0dir
# Ha: begenilen ve rastgele gruplari arasindaki fark 0 degildir
# p degeri 0a yakin geldi. Ha kabul edildi. fark 0dan farkli

# x - y yapiyor. format olarak koysak alfabetik olarak hangisi ondeyse o ilk olurdu


group1 <- df$duration[df$group == 'begenilen']
group2 <- df$duration[df$group == 'rastgele']

t.test(group1, group2, mu = 5, paired = TRUE, 
       alternative = 'less',var.equal = T)

# H0: begenilen ile rastgele arasindaki fark 5 veya 5den fazladir
# Ha: begenilen ile rastgele arasindaki fark 5den azdir
# p degeri 0.34 yani H0 kabul edildi. 5 veya 5den fazla fark vardir


t.test(group1, group2, mu = 5, paired = TRUE, 
       alternative = 'greater')
# H0: iki grup arasindaki fark 10 veya daha azdir
# Ha: iki grup arasindaki fark 5den fazladi
# p degerim 0.6 yani H0 kabul edildi




t.test(group1,group2, mu = 10, paired = T)
# H0: aradaki fark 10
# Ha: aradaki fark 10a esit degil
# p degerim 0a yakin cikti yani Ha kabul edildi,,,,,


##### normal dagilmayan bag??ml?? iki orneklem wilcoxon testi#######


views <- c(ins$RPostsViewed,ins$LPostsViewed)
views

groups <- c(rep('Rastgele',50),rep('Begenilen',50))
groups

df <- data.frame('views'=views,'groups'=groups)
df


# data frameimizi olusturduk. simdi normallik testlerimizi yapalim

shapiro.test(df$views[df$groups=='Rastgele'])
# p degeri 0.001 yani normal dagilim degil

shapiro.test(df$views[df$groups=='Begenilen'])
# p degeri 0.0007 yani normal dagilim degildir

class(df$groups)

df$groups <- as.factor(df$groups)

leveneTest(df$views~df$groups)
  

fligner.test(df$views~df$groups)

bartlett.test(x = df$views, g=df$groups)
# hepsine gore de homojen varyans amk olesine kontrol ettik


?wilcox.test

begenilen<- df$views[df$groups=='Begenilen']
rastgele <- df$views[df$groups=='Rastgele']

wilcox.test(rastgele,begenilen, mu = 0,
            paired = T, conf.int = T)
# H0: lokasyonlari esit medyan(rastgele - begenilen)
# Ha: lokasyonlari esit degil medyan(rastgele - begenilen)
# p degeri 0a yakin yani Ha kabul edildi


wilcox.test(rastgele,begenilen, mu = 5,
            paired = T,
            conf.int = T,
            alternative = 'less')
# H0: lokasyonlar arasi fark 5 veya daha fazla medyan(rastgele - begenilen)
# Ha: lokasyonlar arasi fark 5den kucuk medyan(rastgele - begenilen)
# p degeri 1 yani H0 kabul edildi

wilcox.test(rastgele,begenilen, mu = 8,
            paired = T,
            conf.int = T,
            alternative = 'greater')
# H0: lokasyonlar arasindaki fark 8 veya daha az medyan(rastgele - begenilen)
# Ha: lokasyonlar arasindaki fark 8den fazla medyan(rastgele - begenilen)
# p degerimiz 0a yakin. Ha kabul edildi


# yani rastgele goruntulenme sayisi begenilen gruntulenme sayisindan daha fazladir

median(ins$RPostsViewed) #24
median(ins$LPostsViewed) #14



##### odev #####



df <- cwurData %>% filter(country %in% c("USA" , "United Kingdom"))  %>% 
  select(country , quality_of_education) %>%
  mutate(country = as.character(country))
df              


hist(df$quality_of_education)

shapiro.test(df$quality_of_education[df$country=='USA'])
shapiro.test(df$quality_of_education[df$country=='United Kingdom'])
# normal degil


wilcox.test(df$quality_of_education~df$country, mu=50,
            alternative='less')
# H0: lokasyon fark?? 50 veya daha ??oktor
# Ha: lokasyon fark?? 50den azd??r
# p degerimiz 0a ??ok yak??n. yani Ha hipotezimin kabul edilmi??tir


