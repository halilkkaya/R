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






