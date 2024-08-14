#### nem orani tahmin modeli####

# saat 9daki nem oranini tahmin edicez
# albury sehri

library(tidyverse)

df_albury <- weatherAUS %>% select(Humidity9am,MinTemp,MaxTemp,WindSpeed9am,Pressure9am,Temp9am,Location) %>% 
  filter(Location=="Albury")

df_albury <-df_albury[-7]
# location ksimini cikarttim 


cor(df_albury)
# korelasyon testimi yaptim NA degerlerim oldugunda NA sonuclari geldi
# sonuclar matris olarak geldi

cor(na.omit(df_albury))
# simdilik na.omit ile yaptik sonrasinda veri doldurma yapariz.
# humidty yani nemi tahmin etmeye calistigimizdan onun iliskilerini yorumlayalim
# minTemp ile ters ortanti var ve orani 0.57 minTemp arttikca nem azalir gibi
# maxTemp ile ters oranti var ve orani 0.74 maxTemp arttikca nem azalir gibi
# windSpeed9am ile ters oranti var ve orani 0.31 yani saat 9daki ruzgar hizi arttikca nem azalir gibi
# Pressure9am ile dogru oranti var ve orani 0.30 yani saat 9daki basinc arttikca nem artar gibi
# Temp9am ile aralarinda ters oranti var ve orani 0.74 yani saat 9daki sicaklik arttikca nem azalir gibi

# diger degiskenlerin kendi aralarinda da baglanti olmamali yani az olamli
# onlari inceledigimizde minTemp ile maxTemp arasinda yuksek bi baglanti var 0.80 oraninda
# 0.91 ve 0.93 gibi Temp9am ile minTemp, maxTemp arasinda cok yuksek baglanti var
# simdilik dursun sonra VIC yapariz

pairs(df_albury, pch= 19)
# kesisim noktalari o iki degisken arasindaki tabloyu gosterir dikkat et
# inceledigimizde genelde nemim sicaklikla baglantisini oldugunu gorduk
# sicakliklari kullanip bisiler yapabilriz

# rainfall da etkili olabilirdi bi de onu ekleyip bakalim


df_albury <- weatherAUS %>% select(Humidity9am,MinTemp,MaxTemp,WindSpeed9am,Pressure9am,Temp9am,Location,Rainfall) %>% 
  filter(Location=="Albury")

df_albury <-df_albury[-7]
# rainfall ile dogru oranti var ve orani 0.21 dusuk bi oran verdi
# belki bunu da katabiliriz rainfall da denenebilir yani
# digerleriyle de cok iliskisi yok bunu da kullanabiliriz o yuzden


### mice paketiyle kayip gozlemleri dolduralim

library(mice)

md.pattern(df_albury,plot = F)
# 1 kayip gozlem yok
# 0 kayip gozlem var demektir
# 2984 satirda hic kayip gozlem yok
# 12 satirda rainfall'da kayip gozlem var
# 1 satirda minTempde kayip gozlem var 
# gibi gibi yorumlanacak
# toplam 39 kayip gozlemim var



?mice

imp <- mice(data = df_albury,m = 15,maxit = 15,method = "pmm")

names(imp)

imp$imp
# bu listede imputation sonuclari var. 
# biz 3. imputationi secelim

df_alburyImp <- complete(imp, 13)

md.pattern(df_alburyImp)
# kayip gozlem kalmadigini soluyor



## model olusturma

set.seed(145)
sampleIndex <- sample(1:nrow(df_alburyImp),size = 0.8*nrow(df_alburyImp))

trainSet <- df_alburyImp[sampleIndex,]
testSet <- df_alburyImp[-sampleIndex,]
# %80 ve %20 gibi bi oranla test ve tahmin verilerimi ayirdim

names(trainSet)
model1 <- lm(Humidity9am~ MinTemp + MaxTemp + Temp9am + Rainfall + WindSpeed9am +
               Pressure9am, data = trainSet)

model1 <- lm(Humidity9am~ . , data = trainSet)
# . demek bagimli degisken yani yazilan disindakileri al demek kolay bi yontem

model1

summary(model1)
# r kare degerlerim 0.74 gayet iyi
# p degerim 0.05den baya dusuk yani anlamli bi etki var modelde
# pressure9am degerimdeki p degeri 0.77 gelmis onun icin anlamli bi etki yok diyebilriz
# basincin nem ustunde anlamli bi etkisi yokmus onu cikarabiliriz tablodan

model2 <- lm(Humidity9am~ MinTemp + MaxTemp + Temp9am + Rainfall + WindSpeed9am,
                data = trainSet)
# noktayi kullanamayiz pressure cikartcaz cunku

summary(model2)
# r kare degerlerim ayni gibi duruyor ufak degisiklik var
# gerisi ayni zaten




AIC(model1, k = 8)
AIC(model2, k = 7)


BIC(model1)
BIC(model2)

# nodel 2 daha anlamli gozukuyor ama cok ufak bi azalis. anlamsiz degiskeni cikarinca model daha iyi oldu yani

# artik plotlari
plot(model2)
# ilk tablo artiklarim rastgele dagilis gostermis istedigimiz bir sey
# ikinci tablo normallige bakiyoruz qq ile normal dagilisda duruyor gibi artik degerlere bakiyoz
# ucuncu tablo cook standartlastirilmis artiklari gosteriyo yaklasik olarak dogru giden bi hat cizgisi var
# son tablo artik baskinligini goruyoz aykiri degerlerimde baskinmlik var gibi duruyor



# model2 kullancaz pressure cikaralim o yuzden
testSet2 <- testSet[-5]

pred <- predict(model2, testSet2)
pred


library(caret)

# aralarindaki farki bulcaz

R2(pred,testSet2$Humidity9am)
RMSE(pred,testSet2$Humidity9am)
MAE(pred,testSet2$Humidity9am)

# sonuclari yorumlayaklim
# r2 degerimiz 0.72 yani iyi bi tahmin yapmis
# rmse 9.23 ne kadar azsa o kadar iyi. bu degerin kucuk mu buyuk mu olduguna karar vermek
# icin nem degerlerimize bakmamiz gerek ondan sonra karar vercez
# mae 7.3 cikti


summary(testSet2$Humidity9am)
# araligimiza baktigimizda min deger 21 max deger 100
# rmse degerimiz 9.23 cikmisti aslinda iyi bi tahmin yapiyor diyebilriz 
# ortalama hatamiz 7.3 gayet az sayilabilir



# aykiri degerleri tespit edip atip model olusturup deneyelim
# cook's distance ile yapcaz

dist <- cooks.distance(model2)
dist
# her bir satir icin cook distancelari verdi 0'a yakin sayilar ayni p degeri gibi
# simdi hangi degerden sonrasi icin aykiri sayicam ona bakalim 2 yontem var
# 1 herhangi bi distance ortalamalarin 3 katindan daha fazlaysa o aykiridir
# 2 herhangi bi nokta 4/butunuzunluktan fazlaysa ise aykiri degerdir

olcut1 <- mean(dist)*3
olcut2 <- 4/length(dist)
olcut1
olcut2
# sayilar da zaten hemen hemen yakin ama tabi aldigimiz degerler de dusuk oldugunda onemi olabilir


olc1Index <- which(dist>olcut1)
olc2Index <- which(dist>olcut2)
length(olc1Index)
length(olc2Index)
# aralarinda 10 sayi fazla bulunmus
# 1. olcut daha fazla aykiri deger atmis 1i secelim


plot(1:length(dist),dist,type="p", ylim=range(dist)*c(1,0.07))
# aykiri noktalarin grafigini cizip baktik
abline( h= olcut1, col="red")
# cizgiden sonrasi aykiri deger sayiliyor


trainSetRemoved <- trainSet[-olc1Index,]
# aykiri degerleri cikarttik

# yeni model olusturup performanslari karsilastiralim
# model2 kullanacaktik usttekinde unutmayak da
# . koyabilmek icin trainsetten basinci cikaralim
View(trainSetRemoved)

trainSetRemoved <- trainSetRemoved[-5]
# . ile tum sutunlari secmek icin cikarttim 
names(trainSetRemoved)
model3 <- lm(Humidity9am ~ . , data=trainSetRemoved)

summary(model3)
# r kare degerlerim 0.78e kadar artmis gozukuyor 0.74 idi
# resiudallerim dusmus yine 0a yaklasmis yani. iyi bi sey


summary(model2)
# karsilastirmak icin baktim cogu degerde iyilesme gozukuyor



AIC(model2, k = 7)
AIC(model3, k = 7)

BIC(model2)
BIC(model3)

# model 3 baya bi dusmus yabi cok daha iyi sonuclar vermis gibi duruyor
# model 3 daha iyi diyebiliriz yani

pred1 <- predict(model3, testSet2)

R2(pred1,testSet2$Humidity9am)
RMSE(pred1,testSet2$Humidity9am)
MAE(pred1,testSet2$Humidity9am)
# bu sonuclar model2 ile hemen hemen ayni geldi
# R2 degerimiz ayni 0.72
# RMSE 9.29 cikti
# MAE 7.30
# sonuclar yakin gibi duruyor
# eski modele bakalim
R2(pred,testSet2$Humidity9am)
RMSE(pred,testSet2$Humidity9am)
MAE(pred,testSet2$Humidity9am)
# rmse icin model2
# r2 icin aynilar sayabilriz
# mae icin model3
# asagi yukari hepsi ayni gibi ama


# dogruluk testi yapalim
# gercek deger ile tahmin edilen degeri cikartip gercek degere bolduk ve ortalamasini aldik


mean(abs(testSet2$Humidity9am-pred1)/testSet2$Humidity9am)
mean(abs(testSet2$Humidity9am-pred)/testSet2$Humidity9am)
# ayni sonucalri verdiler la
# aic bic icin model3 daha iyi dedik ama r2 rmse mae ve dogruluk testlerinde
# aralarinda fark yok sonucu cikti



##### VIF varyans siskinlik faktoru####

library(car)
?vif

vif(model3)
# degiskenlerin isimleri ve varyans siskinlik skorlari mevcut
# skor ne kadar buyukse o kadar cok coklu baglantililik sorunu teskil ediyor diyebiliriz
# esik degerimiz nedir? sinirimiz 10
# 10dan buyukse bu degiskenler coklu baglantililik sorununa sebep oluyor diyebiliriz
# en dusuk skorumuz da 1dir. 1den dusuk skor elde ediyorsak o da sorundur
# minTemp ve temp9am cok asmis maxtemp ise sinirda
# temp9am modelden cikarip tekrar inceleyelim
modelvif1 <- lm(Humidity9am~MinTemp+MaxTemp+WindSpeed9am+Rainfall,
                data = trainSetRemoved)

vif(modelvif1)
# vif skorlarim baya dustu istedigimiz yere geldiler
# minTemp ve maxTemp 4.1 ve 3.8e dustu
# korelasyonlarda zaten bunu tespit etmistik. sadece bi tanesini bile cikarinca
# azalma oldu bu degerlerimde
# modeli normal kontrol edelim


summary(modelvif1)
# r2 degerim 0.72 iyi geldi degismemis gibi
# standart hatamiz 8.9
# katsayilarimda fln degisimler oldu
summary(model3)
# karsilastiralim
# r2 0.78mis 0.06lik degisiklik olmus
# standart hatam da 7.8.  1.1 daha artmis modelimdeki hata artmis yani


# diger degiskenleri cikarmayi deneyelim

modelvif2 <- lm(Humidity9am~MinTemp+Temp9am+WindSpeed9am+Rainfall,
                data = trainSetRemoved)

vif(modelvif2)
# minTemp ve Temp9am 9 civarlarinda tam sinirdalar

summary(modelvif2)
# r2 degerim 0.77 diger vif modeline gore artmis normal cikarilmamis modelimle de neredeyse ayni(0.01 fark)
# standart hatam 0.79 modelvif1'e gore daha az hata model3'e gore de neredeyse ayni
# residuallerim de iyi gibi


#bir de ayni model uzerinden minTemp cikaralim

modelvif3 <- lm(Humidity9am~Temp9am+WindSpeed9am+Rainfall,
                data = trainSetRemoved)

vif(modelvif3)
# hepsi 1e yakin temp9am ile mintemp de birbiriyle baglantili oldugundan cikarinca
# 1e dustu hepsi

summary(modelvif3)
# r2 degerim dusmus 0.67
# standart hatam artmis 9.6
# residuallerim artmis

# performansi kotulesmis. minTemp iyi bir etki yapiyormus modelim icin


# test veri seti uzerinden model degerlenmdirme

# Temp9am yokken
predVif1 <- predict(modelvif1, testSet2)
R2(predVif1,testSet2$Humidity9am)
RMSE(predVif1,testSet2$Humidity9am)
MAE(predVif1,testSet2$Humidity9am)


# maxTemp yokken
predVif2 <- predict(modelvif2, testSet2)
R2(predVif2,testSet2$Humidity9am)
RMSE(predVif2,testSet2$Humidity9am)
MAE(predVif2,testSet2$Humidity9am)


# minTemp ve maxTemp yokken
predVif3 <- predict(modelvif3, testSet2)
R2(predVif3,testSet2$Humidity9am)
RMSE(predVif3,testSet2$Humidity9am)
MAE(predVif3,testSet2$Humidity9am)
# sonucalr kotulesmis modelvif3 icin


# r2 degerleri icin en iyi sonucu modelvif2 verdi
# rmse degerleri icin en iyi sonucu modelvif2 verdi
# mae degerleri icin en iyi modelvif2 verdi

# modelvif2>modelvif1>modelvif3 tahminlerin iyilegine gore

# hem vif hem de diger durumlara gore modelvif2 en iyisini sectik 


#####  asamali regresyon(stepwise)####


# bagimsiz degiskenlerimizi en iyi sekilde secebilmek
# bagimsiz degiskenleri daha dinamik sekilde cikartma ve ekleme islemi yapiyor
# isimizi kolaylastiriyor


model1 <- lm(Humidity9am~ MinTemp + MaxTemp + Temp9am + Rainfall + WindSpeed9am +
               Pressure9am, data = trainSet)
# icinde tum degiskenlerimiz oldugu bi modeli aldik

step(lm(Humidity9am~1, data = trainSetRemoved), direction = "forward",scope = ~ MinTemp + MaxTemp + 
       Temp9am + Rainfall + WindSpeed9am + Pressure9am)

# uzun bir sonuc verdi.
# start: AIC demek yani biz Humidity~1 dedik ya sadece humidity kullaniliyor burda
# onun sonuclarini verdi. degiskenlere bakiyo + ekleme anlamina geliyo maxTemp eklenirse AIC degerleri ne olur
# vs onlari veriyo. ikisini birden degil tek tek ekleme olarak dusun
# en kucuk AIC degeri hangisiyse onu modelimize ekliyor maxTemp idi ve onu ekledi
# step diyere onu ekledikten sonraki diger degiskenleri de 2. olarak tek tek ekleyip denedi yine en kucuk
# AIC degerini 2. olarak ekledi bunu en duusk AIC degerini elde edene kadar yapti ve istedigimiz modeli
# buldu
# herhangi bi degiskeni eklediginde AIC degerimde artis olsaydi onu eklemeyecekti
# onemli nokta o 


# aykiri degerleri cikartmadigimiz, train ve test olarak ayirmadigimiz bi veri setiyle deneyeklim



step(lm(Humidity9am~1, data = df_alburyImp), direction = "forward",scope = ~ MinTemp + MaxTemp + 
       Temp9am + Rainfall + WindSpeed9am + Pressure9am)
# burda da ayni islemler devam ediyor ancak pressure degerini eklediginde AIC degeri arttigini goruyor
# ve o degiskeni son modelimize eklemiyor. bunu gormek icin denemistik


modelstep <- step(lm(Humidity9am~1, data = df_alburyImp), direction = "forward",scope = ~ MinTemp + MaxTemp + 
                    Temp9am + Rainfall + WindSpeed9am + Pressure9am)

modelstep
# burda da en son sectigi model geliyor


step(lm(Humidity9am~MinTemp + MaxTemp + Temp9am + Rainfall + WindSpeed9am + Pressure9am,
        data = df_alburyImp), direction = "backward")

# bu da cikartma islemi yaparak gidiyo. AIC degeri hangisini cikarirsam daha dusuk olur diye bakarak gidiyo
# pressure cikarinca hicbirini cikartmazsam daha dusuk olur diye kaliyo ve cikartma yapmiyo



# cift yonlu olarak kontrol etme. trainset uzerinden bakalim
step(lm(Humidity9am~1, data = trainSetRemoved), direction = "both",scope = ~ MinTemp + MaxTemp + 
       Temp9am + Rainfall + WindSpeed9am + Pressure9am)
# burada birini eklediginde daha sonra bi taen daha eklediginde ilk ekledigi degiskeni cikartmak AIC degerini
# dusururse diye onu cikartmayi da kontrol ediyor


# diger veri setiyle cift yonlu
step(lm(Humidity9am~1, data = df_alburyImp), direction = "both",scope = ~ MinTemp + MaxTemp + 
       Temp9am + Rainfall + WindSpeed9am + Pressure9am)
#ayni mantikla ilerledi

# kimi zamanlar cok yararli olabiliyor. cok degiskenli veri setlerinde cok zaman kazandirir



modelstep <- step(lm(Humidity9am~1, data = trainSetRemoved), direction = "both",scope = ~ MinTemp + MaxTemp + 
       Temp9am + Rainfall + WindSpeed9am + Pressure9am)
# modelimizi de bu sekilde kaydediriz

# prediction islemini burdan gerceklestirelim alistirma olsun
predstep <- predict(modelstep,testSet)

R2(predstep,testSet2$Humidity9am)
RMSE(predstep,testSet2$Humidity9am)
MAE(predstep,testSet2$Humidity9am)

# degerleri kontrol ettik iyi sonuc verdi sayilir

##### dummy degiskenler  #####

# ciftcinin arazisi ve sehiriyle beraber yaptigi uretimi tahmin etme modeli mesela
# burda sehirlere numeric yapmamiz gerek

# dummy = k - 1
# simdi 3 sehir var diyelim samsun amasya adana
# burda sehirAdana ve sehirAmasya diye 2 dummy degiskenim olur

# sehirAdana = 0, sehirAmasya = 0 => samsun'u verir
# sehirAdana = 1, sehirAmasya = 0 => adana'yi verir
# sehirAdana = 0, sehirAmasya = 1 => amasya'yi verir

# diye gerceklesir

# kimi zaman dummy degisken sayisi kategori sayisi kadar da olabiliyo bu durumda her bir
# kategori icin 1 degeri digerleir icin 0 degeri aliyor




##### kategorik veri ile model olusturma #####

weatherAUS$Location <- as.factor(weatherAUS$Location)
levels(weatherAUS$Location) 

df <- weatherAUS[weatherAUS$Location=="Albury"|
                   weatherAUS$Location=="Bendigo"|
                   weatherAUS$Location=="Sydney",]
nrow(df)
# 9382 verimiz var

names(df)
df <- df[c("Humidity9am","Location","MinTemp","MaxTemp","WindSpeed9am",
           "Pressure9am","Temp9am","Rainfall")]
View(df)


df$Location <- as.character(df$Location)
df$Location <- as.factor(df$Location)
# verileri cekmeden once factor oldugundan diger verilerin numarasi da duruyordu o yuzden sifirladik


imp <- mice(data = df,m = 15,method = "pmm")
imp$imp

df <- complete(imp,15)
df
anyNA(df)
# tum degerlerim doldu imputation ile hoca na.omit ile cikartti onlari

set.seed(120)
idx <- sample(1:nrow(df),size = 0.8*nrow(df))

trainSet <- df[idx,]
testSet <- df[-idx,]

names(trainSet)
# dummy seklinde degiskenler kendi kendine olusacak kendimiz olusturmaya gerek yok
model1cat <- lm(Humidity9am ~ Location + MinTemp + MaxTemp + Temp9am+Rainfall+WindSpeed9am+
                  Pressure9am, data=trainSet)

summary(model1cat)
# r2 degerim 0.63 biraz dusme var
# residullaerimde de ufak artmalar var
# standart hatam 10.14
# burda farkli olarak locationBendigo ve locationSydney var dikkat edelim
# peki bunlari nasil yorumlicaz?
# albury yok k - 1 oldugundan dolayi yok
# ikisinin de sifir oldugu zaman albury degeri gelir  
# LocationBendigo   0.53856 
# LocationSydney   -4.95356
# ikisinin sifir oldugu durumda albury sehrinde olmasi nemi nasil etkilermis yorumlayalim
# sydney -4.9 oldugundan 0 verdigimizde +4.9 oluyo
# bendigo 0.54 oldugundan 0 verdigimizde -0.54 oluyor
# yani ikisinin de sifir oldugu furumda albury'nin neme etkisi 4.9-0.54 oluyor
# bendigo'nun p degeri 0.07 yani 0.05den kucuk oldugundan 0.05 onem duzeyinde etkisiz yorumlanir
# ancak onem duzeyini 0.1 yaparsak %90 derecesinde onemli diyebilrizi o yuzden tolere edebilecegimiz bi deger



predCat <- predict(model1cat, testSet)

library(caret)

R2(predCat, testSet$Humidity9am)
# 0.6296
RMSE(predCat, testSet$Humidity9am)
# 10.29
MAE(predCat, testSet$Humidity9am)
# 8.14

plot(model1cat)
# grafik 1 icin dagilim rastgele giderken sonlara dogru kaymis gozukuyor
# qq grafigimde artik degerlerim uclarda biraz kacmis duruyor
# yine aykiri degerlerimi cook distance grafigimde gorebiliyorum



dist <- cooks.distance(model1cat)

olcut1 <- mean(dist)*3
olcut2 <- 4/length(dist)

index1 <- which(dist>olcut1)
index2 <- which(dist>olcut2)
length(index1)
length(index2)
# index 1'de yaklasik 100 tane daha fazla aykiri deger verdi

trainSetRemoved <- trainSet[-index1,]

names(trainSetRemoved)
model2cat <- lm(Humidity9am~ Location+MinTemp+MaxTemp+WindSpeed9am+
                  Pressure9am+Temp9am+Rainfall, data = trainSetRemoved)
summary(model2cat)

predCat2 <- predict(model2cat, testSet)

R2(predCat, testSet$Humidity9am)
R2(predCat2,testSet$Humidity9am)
# 2. modelimde R2 degerim azalmis cok cok az.

RMSE(predCat2,testSet$Humidity9am)
RMSE(predCat,testSet$Humidity9am)
# RMSE degerimde model 2 icin artmis biraz kotu durum

MAE(predCat2,testSet$Humidity9am)
MAE(predCat,testSet$Humidity9am)
# model 2 icin cok cok az artmis. 

AIC(model1cat, k = 9)
AIC(model2cat,k = 9)

BIC(model1cat)
BIC(model2cat)

# model2cat daha iyi sonuclar verdi bu testlerde
# r2, rmse ve mae degerleri model1 icin iyiken aic ve bic degerleri model2 icin iyi

mean(abs(testSet$Humidity9am-predCat)/testSet$Humidity9am)
mean(abs(testSet$Humidity9am-predCat2)/testSet$Humidity9am)
# aralarinda ufak bi fark var

# bi kere daha aykiri deger atma islemi yapalim bakalim

dist2 <- cooks.distance(model2cat)

olcut1_ <- mean(dist2)*3
olcut2_ <- 4/length(dist2)
olcut1_
olcut2_

indis1 <- which(dist2>olcut1_)
indis2 <- which(dist2>olcut2_)

length(indis1) # 555
length(indis2) # 181
# olm cok fazla cikti 1de la neyse yine 1i alak



trainSetRemoved1 <- trainSetRemoved[-indis1,]

model3cat <-  lm(Humidity9am~ Location+MinTemp+MaxTemp+WindSpeed9am+
                   Pressure9am+Temp9am+Rainfall, data = trainSetRemoved1)
summary(model3cat)
# R2 artmis residualler dusmus satndart hata dusmus
# grafikleri inceleyek

plot(model3cat)
# ilk grafikte daha iyi bi dagilim var cokiyiiii
# normallik uclarda yine kotu ama daha iyi duruyor
# standartlastirilmis grafigimde cizgi duzelmis biraz daha
# cooksdistance grafigimdeki ortadaki cizgi duzgunlesmis

predCat3 <- predict(model3cat,testSet)

R2(predCat, testSet$Humidity9am)
R2(predCat2, testSet$Humidity9am)
R2(predCat3, testSet$Humidity9am)
# r2 degerim giderek dustu

RMSE(predCat, testSet$Humidity9am)
RMSE(predCat2, testSet$Humidity9am)
RMSE(predCat3, testSet$Humidity9am)
# giderek artti kotu duruyor yine

MAE(predCat, testSet$Humidity9am)
MAE(predCat2, testSet$Humidity9am)
MAE(predCat3, testSet$Humidity9am)
# ufak artislar soz konusu buda kotu bir durum


AIC(model3cat,k = 9)
AIC(model2cat,k = 9)
AIC(model1cat,k = 9)
# AIC degerlerimde baya bi dusus var bubda iyi bir gosterge

BIC(model1cat)
BIC(model2cat)
BIC(model3cat)
# ayni sekilde BIC degerlerimde de dusus var bu da iyi



# AIC ve BIC testlerimde verilerim daha iyiye giderken test esnasinda daha kotuye gidiyor 
# ama test esnasindaki degisiklikler cok cok cok ufak
# AIC ve BIC ise cok yuksek degisiklikler
# overfitting durumu oluyor bu da

# varians ve bias furumunu
# overfitting ve underfitting durumunu 
# L1 ve L2 duzgunestirmelerini dosyaya yazdim ordan okucam tekrar icin






