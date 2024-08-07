## istatistik testleri
####### normalliik testi #####

## shapiro wilk

kiraz1 <- data.frame(magr=c(5.8,5.7,5.4,4.9,6.7,4.8,6.3,5.9,5.3,5.9,7.1,6.2,6.4,6.5,6.4,6.3,5.8,7.2,6.7,6.6,5.2,6.5,6.2,7.1,5.5,5.6,6.2,5.5,6.3,6.4),
                     grup=c(rep('A',10),rep('B',10),rep('K',10)))


kiraz2 <- data.frame(magr=c(5.9,6.6,6.5,6.6,7.0,7.1,6.3,6.2,6.1,5.9,10.5,8.4,9.6,10.5,6.9,8.7,10.8,9.4,10.1,6.1,4.0,5.1,6.1,4.9,5.1,5.1,5.2,4.8,4.6,6.4),
                     grup=c(rep('A',10),rep('B',10),rep('K',10)))

# p-value 0.05 ve 0.05den buyuk ise H0 hipotezim
# p-value 0.05den kucukse Ha hipotezlerim kabul gorur
shapiro.test(kiraz1$magr)
# H0: normal dagilim gosteren veriler
# Ha: normal dagilim gostermeyen veriler
# p.value 0.63 cikti yani H0 hipotezim kabul edildi.


shapiro.test(kiraz2$magr)
# H0: normal dagilim gosteren veriler
# Ha: normal dagilim gostermeyen veriler
# p.value 0.006 H0 hipotezim reddedildi. Ha hipotezim kabul edildi


model1 <- lm(magr~grup, kiraz1)
model2 <- lm(magr~grup, kiraz2)
# bu modellerin residual degerlerini de kontrol edelim


shapiro.test(model1$residuals)
# H0: normal dagilim gosteren veriler
# Ha: normal dagilim gostermeyen veriler
# p.value 0.65 H0 kabul edildi

shapiro.test(model2$residuals)
# H0: normal dagilim gosteren veriler
# Ha: normal dagilim gostermeyen veriler
# p.value 0.03 Ha kabul edildi


# bir veri setinin residual verileri de eger normal dagilan cikiyorsa o veri seti kesinlikle normal dagilim diyebiliriz
# kiraz1'in residual degerleri de normal dagilan cikti
# kiraz2'nin resiual degerleri de normal dagilmayan seklinde cikti

# kiraz1 kesin normal, kiraz2 kesin normal dagilmayan cikti.


install.packages("nortest")
library(nortest)

# bir baska normallik testi deneyelim

## anderson darling testi

ad.test(kiraz1$magr)
# H0: normal dagilim gosteren veriler
# Ha: normal dagilim gostermeyen veriler
# p.value 0.56 yan?? normal dagilan verilerdir

ad.test(kiraz2$magr)
# H0: normal dagilim gosteren veriler
# Ha: normal dagilim gostermeyen veriler
# p.value 0.002 yani normal dagilim gostermeyen verilerdir



install.packages("fBasics")
library(fBasics)


# bir baska normallik testini deneyelim
##jarque-bera testi

# onemli bi nokta bu test carpiklik ve basikliga gore yapar testi

jarqueberaTest(kiraz1$magr)
# H0: normal dagilim gosteren veriler
# Ha: normal dagilim gostermeyen veriler
# p.value 0.72 yani normal dagilan verilerdir


##### homojen varyanslik testi#####

# oncelikle homojen varyanslilik nedir?
# varyans bi tablodaki verilerin gosterdigi degisiklik dersek eger homojen varyanslilik
# iki tablonun sekli benzer demek diyebiliriz
# yani veriler arasindaki degiskenlik ayni ya da benzer diyebilmek homojen varyansliliktir



## levene testi

# car kutuphanesinde bulunuyor

library(car)

leveneTest(kiraz1$magr~kiraz1$grup)
# H0: gruplar homojen varyanslilik gosteriyor
# Ha: gruplar homojen varyanslilik gostermiyor
# p.value 0.53 H0 kabul edildi

# bu testin ozelliklerinde biri normal dagilmayan icin de iyi sonuc vermesi
# sinirdaki veriler icin kullanilabilecek bi testtir

# her bir grubun homojenligini olcuyor
# kiraz1 verisetine baktigimda k a b gruplari var o gruplarin icerisinde bulunan
# magr sayilarinin homojenligine bakiyor.

leveneTest(kiraz2$magr~kiraz2$grup)
# H0: gruplar homojen varyanslilik gosteriyor
# Ha: gruplar homojen varyanslilik gostermiyor
# p.value 0.01 yani kiraz2 gruplari arasinda homojenlik gostermiyor
# kiraz2 normallik gostermeyen bi gruptu hatirlayalim


# bir abska testi deneyelim

## Brown Forstyh testi

install.packages("HH")
library(HH)

hovBF(magr~grup, kiraz1)
# H0: gruplar homojen varyanslilik gosteriyor
# Ha: gruplar homojen varyanslilik gostermiyor
# p.value 0.53 H0 kabul edildi

# levene testiyle ayni sonuclari verir. beraberlerdir yani


# bir baska test
## bartlett testi

bartlett.test(magr~grup, kiraz1)
# H0: gruplar homojen varyanslilik gosteriyor
# Ha: gruplar homojen varyanslilik gostermiyor
# p.value 0.52 H0 kabul edildi

# normallige onem verir. normal olmayan verilerde yanlis sonuclar verir
# ama yine de normal dagilmayan kiraz2 verisinde deneyelim

bartlett.test(magr~grup, kiraz2)
# H0: gruplar homojen varyanslilik gosteriyor
# Ha: gruplar homojen varyanslilik gostermiyor
# p.value 0.0006 cikti ama dedigim gibi normal dagilmayan veriler oldugundan bartlett yerine fligner, levene ya da hovbf kullanabilriiz


## fligner testi

# normal dagilmayan verilerde uygulanir. normallige duyarliligi yoktur
fligner.test(magr~grup, kiraz2)
# H0: gruplar homojen varyanslilik gosteriyor
# Ha: gruplar homojen varyanslilik gostermiyor
# p.value 0.03 H0 reddedildi, Ha kabul edildi


# kiraz1 normal dagilim gosteriyor ama yine de kontrol edelim
fligner.test(magr~grup, kiraz1)
# H0: gruplar homojen varyanslilik gosteriyor
# Ha: gruplar homojen varyanslilik gostermiyor
# p.value 0.47 ama dedigim gibi normal dagilan oldugundan bartlett kullanmaliyiz


##### bagimsizlik konttolu #####

install.packages("car")
library(car)

# orneklem grubu icinde yer alan her bir verinin digerlerinden bagimsiz olmasi yani aralarinda iliski olmamasi demektir


model1 <- lm(magr~grup, kiraz1)
model2 <- lm(magr~grup, kiraz2)

durbinWatsonTest(model1)
# H0: gruplardaki birimler birbirinden bagimsiz
# Ha: gruplardaki birimler birbirine bagimli
# p.value 0.08 H0 kabul edildi

durbinWatsonTest(model2)
# H0: gruplardaki birimler birbirinden bagimsiz
# Ha: gruplardaki birimler birbirine bagimli
# p.value 0.90 H0 kabul edildi



# soyle aciklayalim: Autocorrelation degerimiz sifira yakin 
# otokorealsyon en demektir? birbirlerine bagimli olma durumu demektir
# - ise negatif + ise pozitif bagimlilik vardir sifirsa yakin falan 
# ancak p degerimize baktigimizda otokorelasyonun aslinda etkili olmadigini goruruz.
# 0.05den buyuk o degerlerinde kabul goren H0 hipotezimiz otokorelasyonun onemsiz oldugunu
# yani bagimsiz oldugunu soyluyor.



# parametrik testler icin gerekli varsayimlar saglanmadiginda direkt olarak parametrik olmayan testlere gecis yapilmaz
# veri donusumuyle cozum bulunmaya calisilmalidir. eger yine de cozum olmadiysa o zaman parametrik olmayan testlere gecilmelidir
# sayfa 140da bu formulleri matematiksel olarak vermistir ancak bveri donusumu cok genis bi konu oldugundan detayli islenmemis
# 2. kitap olan veri biliminde r ve veri onisleme kitabinda islenmis duruyor bunu o kitaba gecince tekrar edecegim



# sayfa 141 alistirmalari

x <- rnorm(100, mean = 50, sd = 2.5)
y <- rgamma(100, shape = 4,scale = 3)
z <- rlnorm(100, meanlog = 1,sdlog = 0.6)

shapiro.test(x)
shapiro.test(y)
shapiro.test(z)




par(mfrow=(c(1,3)))
hist(x, xlab = "x degerleri",probability = T)
lines(density(x))
hist(y, xlab = "y degerleri",probability = T)
lines(density(y))
hist(z, xlab = "z degerleri",probability = T)
lines(density(z))



m <- lm(y~x)
summary(m)
shapiro.test(m$residuals)
mm <- lm(z~x)
summary(mm)
shapiro.test(mm$residuals)



df <- data.frame(deger=c(4.53,4.48,3.83,5.14,5.44,5.49,5.95,3.11,5.03,3.93,6.60,5.64,3.58,4.82,8.03,4.53,2.40,7.46,7.61,4.41,
                         5.12,3.67,2.78,3.56,4.00,4.25,4.11,4.79,3.98,4.21),
                 grup=(c(rep("Grup A",10),rep("Grup B",10),rep("Grup C",10)))) 
df

# varyans homojenligi soruyo once normallik yapak


shapiro.test(df$deger[df$grup=="Grup A"])
shapiro.test(df$deger[df$grup=="Grup B"])
shapiro.test(df$deger[df$grup=="Grup C"])
# hepsi normal dagilim
shapiro.test(df$deger)
# geneli de normal dagilim

hist(df$deger~df$grup)
# ohaaa direkt 3 grafigi birden tekte verdi cok iyimisss

bartlett.test(df$deger~df$grup)
# H0: homojen varyanslilik gosterir
# Ha: homojen varyanslilik gostermez
# p.value 0.005 yani Ha kabul edildi



df1 <- data.frame(deger=c(3,7,5,5,4,6,3,3,7,1,4,4,6,6,2,8,7,8,3,9,4,7,13,25,49,87,173,345,10,10,25,25,30,30,40,40,50,50),
                  grup=c(rep("Grup A",20),rep("Grup B",8),rep("Grup C",10)))

df1

hist(df1$deger~df1$grup)

shapiro.test(df1$deger[df1$grup=="Grup C"])
# A ve C grubu normal B grubu Anormal dagilim gosteriyor
# genel dagilim da Anormal dagilim 


model <- lm(deger~grup, data = df1)
durbinWatsonTest(model)
# H0: gruplar birbirinden bagimsizdir, otokorelasyon onemsizdir
# Ha: gruplar birbirine bagimlidir, otokorelasyon onemlidir
# p.value 0.02 yani H0 reddedildi. gruplar birbirine bagimli cikti aga
# otokorelasyon degerim 0.37 bu da pozitif yonlu bi bagimlilik var demektir.




df2 <- data.frame(deger=c(11.23,9.80,9.05,9.66,8.40,10.06,10.60,8.74,8.96,8.67,27.46,24.60,23.10,24.32,21.80,25.12,26.20,22.48,22.92,22.34,
                          16.75,11.75,11.30,15.13,14.21,14.25,14.78,18.52,13.98,13.64),
                  grup=c(rep("Grup A",10),rep("Grup B",10),rep("Grup C",10)))
df2

hist(deger~grup, df2)

shapiro.test(df2$deger)
shapiro.test(df2$deger[df$grup=='Grup C'])
# hepsi kendi icinde normal ama genel olarak baktiginda degil


modell <- lm(deger~grup,df2)

durbinWatsonTest(modell)
# H0: gruplar birbirinden bagimsizdir, otokorelasyon onemsizdir
# Ha: gruplar birbirine bagimlidir, otokorelasyon onemlidir
# p.value 0.96 yani kessssinlikle H0 kabul edildi sakin laaa yani iliski yok ha







df3 <- data.frame(meyveagr=c(57,46,54,55,43,40,55,49,62,52,50,41),
                  yontem=c(rep(1,4),rep(2,4),rep(3,4)))
df3


library(psych)

describeBy(df3$meyveagr, group = df3$yontem,digits = 2)
# inceleyip normallikler, homojenlikler hakkinda dusunelim


shapiro.test(df3$meyveagr)
# normal

bartlett.test(df3$meyveagr~df3$yontem)
# homojen varyans


mod <- lm(meyveagr~yontem,df3)
summary(mod)

durbinWatsonTest(mod)
# H0: gruplar birbirinden bagimsizdir, otokorelasyon onemsizdir
# Ha: gruplar birbirine bagimlidir, otokorelasyon onemlidir
# bagimsizdir p degeri 0.23


hist(df3$meyveagr)
hist(meyveagr~yontem,df3,prob = T,ymax = 0.2)

par(mfrow=c(1,3))
hist(df3$meyveagr[df3$yontem==1],probability = T)
lines(density(x = df3$meyveagr[df3$yontem==1]))

hist(df3$meyveagr[df3$yontem==2],probability = T)
lines(density(x = df3$meyveagr[df3$yontem==2]))

hist(df3$meyveagr[df3$yontem==3],probability = T)
lines(density(x = df3$meyveagr[df3$yontem==3]))

qqPlot(df3$meyveagr~df3$yontem)


library(ggplot2)
qqplot(df3$yontem,df3$meyveagr)



# na degerlerime 0 atayip diger degerlerimin rankini 2den baslatma sorusu cozumu

# Veri seti
x <- c(10, 16, NA, 34, 14, 30, 7, 11, NA, 24, 22, 15, 7, 18, 20)

# NA de??erleri en ba??a alarak s??ralama
ranks <- rank(x, na.last = "keep", ties.method = "first")

# NA de??erlerinin s??ralamas??n?? 0 olarak ayarlay??n
ranks[is.na(x)] <- 0

# NA olmayan de??erlerin s??ralamas??n?? 2'den ba??latacak ??ekilde d??zenleyin
ranks[!is.na(x)] <- rank(x, na.last = "keep", ties.method = "first")[!is.na(x)] + 1

# Sonu??lar?? g??r??nt??leyin
print(ranks)




