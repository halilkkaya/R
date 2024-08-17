##### veri onisleme ####

install.packages("nnet")
library(nnet)
library(tidyverse)

# heart.csv verisetini kullanacagiz
View(modelData)
# degiskenlere gore cp tahmin etme yapacagiz
# target kullanmayacagiz

modelData <- modelData[,-which(names(modelData)=="target")]
View(modelData)


table(modelData$cp)
# 4 taen sinif var burda da en dusuk sinifim 23 en yuksek 143 veri var
# verilerimizi yaklastirmamiz gerek model olustururken
# 3. sinifim 23 veri var ve cok az o yuzden cikaralim ve deneme yapalim
# daha gercekci bi veri olsaydi bu daha fazla veri alip 3. sinifi arttirmamiz gerekecekti
# egitim amacli oldugundan ciikaralim


modelData <- modelData[modelData$cp != 3,]

table(modelData$cp)
# 3. sinifim cikarildi diger siniflar verildi

modelData <- modelData %>%  mutate(cp = as.factor(cp),
            slope= as.factor(slope),ca = as.factor(ca), 
            thal=as.factor(thal), restecg = as.factor(restecg))
# birden fazla sinifa ait olan numeric degiskenlerimi olmasi gerektigi gibi
# faktore cevirdik


### train ve test ayrimi

min(table(modelData$cp))
table(modelData$cp)[1]

trainTestSplit <- function(data, dvName, seed){
  tbl <- table(data[,dvName])
  # tabloyu sectik
  classes <- names(tbl)
  # adlarini aldik
  minClass <- min(tbl)
  # en kucuk sayiya sahip classi sectik
  lengthClass <- length(tbl)
  # toplam class sayisini aldik
  
  train <- data.frame()
  test <- data.frame()
  # test ve train verisetlerimizi sonrasinda icini doldurmak icin bos sekilde olusturduk
  
  for (i in 1:lengthClass) {
    selectedClass <- data[,dvName] == classes[i]
    # siniflari tek tek seciyoruz
    set.seed(seed)
    sampleIndex <- sample(1:nrow(data[selectedClass,]), size = minClass*0.8)
    # siniflara gore en az sayiya sahip olan sinifin uzerinden belli rastgele indexler aliyoruz
    
    train <- rbind(train, data[selectedClass, ][sampleIndex, ])
    # for ile gelen siniflara ait verilerimi trainin icerisine atiyor
    test <- rbind(test, data[selectedClass, ][-sampleIndex, ])
  }
  return(list(train,test))
}



trainTestSplit(data = modelData,dvName = "cp",seed = 125)
# liste olarak geldi ilk eleman train ikinci eleman test verisetimdir

train <- trainTestSplit(data = modelData,dvName = "cp",seed = 125)[[1]]
test <- trainTestSplit(data = modelData,dvName = "cp",seed = 125)[[2]]

table(train$cp)
# 40-40-40 olarak geldi cok iyi
table(test$cp)
# bu da daha farkli geldi bu cok normal 
# 0 degerimiz cok fazla oldugundan 0'i elde etme dpgrulugu daha fazla olurken
# 1 degerimiz 10 tane var sadece bunlari da biraz yaklastirabiliriz


## kesfedici veri analizi

par(mfrow=c(2,2))
plot(train$cp, train$age, main = "age")
plot(train$cp, train$trestbps, main = "trestbps")
plot(train$cp, train$chol, main = "chol")
plot(train$cp, train$thalach, main = "thalach")


# kendi kendine boxplot olarak acti cunku 
# x eksenim factor ve 3 seviyesi var

# yorumluyoruz burdaki degiskenler acaba etkiliyor mu diye
# age cok etkilemiyor gibi duruyor
# trestbps de cok etkilemiyor gibi
# chol da ayni seviyede gosteriyor
# farkli seviyede olsalardi chol artar azalirken cp soyle degisir fln diyebilirdik
# thalach'da 0'i tahmin etmede onemli olabilir orda degisiklik var cunku


dev.off()
plot(train$cp, train$oldpeak, main = "oldpeak")
# burda da 0'i tahmin etmek icin ise yarar ama digerler 1 ve 2 birbirine yakin gibi
# oldpeak'de etkili olabilir.
# gorseller uzerinden kesfedici bi analiz yapiyoruz bu sekilde


# kategorik verileri birbiri arasinda karsilastiralim burda plot grafikleri 
# cizdiremiyoruz o yuzden table() fonk kullanabiliriz

table(train$cp, train$sex)
# cikan sonuclara gore soyle bi yorum yapalim
# cinsiyeti 0 olanlarin cp degeri gittikce artarken
# cinsiyeti 1 olarak cp degeri gittikce azaliyor
# ya da cp bazinda su kadar 0 cinsiyetiine sahip kisi
# su kadar 1 cinsiyetine sahip kisi var da diyebiliriz

# chisquare testitle de bakabiliriz bunlara
# bu daha kolay

chisq.test(table(train$cp, train$sex))
# hipotezleri kuralim
# H0: degiskenlerim birbirinden bagimsizdir, etki yoktur
# Ha: degiskenlerim birbirine bagimlidir, etki vardir
# p degerim 0.51 geldi yani H0 kabul edildi
# degiskenlerim arasinda bi bagimlilik yok diyoruz
# cinsiyetin cpye bi etkisi yokmus yani


chisq.test(table(train$cp, train$exang))
# H0: degiskenlerim birbirinden bagimsizdir, etki yoktur
# Ha: degiskenlerim birbirine bagimlidir, etki vardir
# p degerim 0a cok yakin H0 red Ha kabul
# yani exang degerlerimin cp uzerinde bi etkisi varmis
# cp'yi tahmin etmek icin exang kullanilabilir


chisq.test(table(train$cp, train$slope))
# H0: degiskenlerim birbirinden bagimsizdir, etki yoktur
# Ha: degiskenlerim birbirine bagimlidir, etki vardir
# p degerim 0.004 H0 red Ha kabul
# aralarinda bi etki varmis
# bu da kullanilabilir dursun
# juyari mesaji aldik icerisinde 0 degerinde cok az veri var diye 
# ki kjare testinde bi degiskende 5den az veri varsa uyari veriyordu unutma

chisq.test(table(train$cp, train$ca))
# H0: degiskenlerim birbirinden bagimsizdir, etki yoktur
# Ha: degiskenlerim birbirine bagimlidir, etki vardir
# p degerim 0.08 geldi H0 kabul
# ama sinirda kalmis bu tarz sonuclar elde ettiysek 0.90dan alsaydik mesela
# aralarinda iliski vardir dicektik yani olabilir bu dursun kullanavilirz


chisq.test(table(train$cp, train$fbs))
# H0: degiskenlerim birbirinden bagimsizdir, etki yoktur
# Ha: degiskenlerim birbirine bagimlidir, etki vardir
# p degerim 0.18 H0 kabul edildi 
# ama %80 oraninda anblamli bi iliski avr diyebilriz




chisq.test(table(train$cp, train$thal))
# H0: degiskenlerim birbirinden bagimsizdir, etki yoktur
# Ha: degiskenlerim birbirine bagimlidir, etki vardir
# p degerim 0.05 H0 kabul
# ama yine de %95 anlamli etki var diyebiliriz

chisq.test(table(train$cp, train$restecg))
# H0: degiskenlerim birbirinden bagimsizdir, etki yoktur
# Ha: degiskenlerim birbirine bagimlidir, etki vardir
# p degerim 0.44 H0 kabul
# aralarinda bi bagimlilik yok, etki yok



##### model olusturalim ####

library(nnet)
library(e1071)
library(caret)
library(tidyverse)



modelBase <- multinom(cp ~ . , data = train)
# iterasyon seklinde yapiyor burda bu konuya yapay sinir aglari kisminda dahab hakim olucaz
# simdilik multinominal logistic model boyle olusur diye bilelim

summary(modelBase)
# dummy degisken seklinde buyun degiskenlerimi vermis
# baktigimizda 2 model olusturmus bu da teori dersinde gordugumuz sekilde 3 sinifimiz varsa
# 2 model olusturucak seklinde yazmistik dosyayi kontrol edersin tekrar ederken

# Residual Deviance: 191.1686 gelmis dusmesi gerek 
# AIC degerim 271 bunun da dusmesi lazim
# baska model kurup bunlari karsilastirma icin kullanicaz


modelBase$fitted.values
# tahmin edilen degerlerimin olasiliklari
# yani 0 1 2 var ve altlarinda her bi deger icin olasiliklar mevcut
# hangisi en buyukse o secilecek demek burda ilk degerimiz 122. indistekine bakalim
# en yuksek oran 1'de olan oran yani cp degerim 1 olacak dedik


modelBase$decay
# 0 cikti
# bu curume demek yani regularixation islemi gibi dusunebiliriz (L1,L2 gibi)
# buna verilen oran regularization islemi icin kullanilabiliyor 
# caret ile bunu yapcaz

##### yeni modeller kurup karsilastirma yapalim ####

names(train)
# yukarida grafigi yorumlarken alalim dediklerimizi alcaz
model2 <- multinom(cp ~ sex + fbs + restecg + thalach + exang + oldpeak + slope +
                   ca + thal, data = train)

summary(model2)
# Residual Deviance: 194.8161 
# AIC: 262.8161 
# residual deviance artmis aic azalmis cok da bilgi vermedi sadece on bilgi
# testing kismi daha onemli 

# ki kare testlerine gore bakip degisken ekleyelim/cikaralim modele simdi

model3 <-  multinom(cp ~ thalach + 
                      exang + oldpeak + slope +ca + thal, data = train)


summary(model3)
# Residual Deviance: 203.7401 artmis kotu durum sebebi degisken sayisi azaldi diye olabilir
# AIC: 255.7401 azalmis iyi durum
summary(modelBase)
summary(model2)

# AIC degerine gore model3
# Residual deviance gore modelBase
# daha iyi sonuclar vermis


##### modeller uzerinde  tahminlerimiz yapalim #####

library(caret)

varImp(modelBase)
# overall skorlarini verdi. en yuksek olani model icin en onemlisi oldugunu gosteriyor
# modele degisken ekleyip cikarmak icin kullanilabilir


predModelBase <- predict(modelBase, test)
predModelBase

predModel2 <- predict(model2, test)
predModel2

predModel3 <- predict(model3, test)
predModel3


confusionMatrix(predModelBase,test$cp, mode = "prec_recall")
# accuracy deger??m 0.56 bbunu 0.75 ustune cikmasi gerek
# no informati??n rate 0.64 
# p value 0.97 cikti kotu bi model bu demektir bu
# kappa degerim de kotu byaa
# precision degerlerime baktigimda 0.81 degerini almis 0 sinifi icin
# yani 0 sinifini tahmin etmede iyi bi model olmus ancak 1 sinifina baktigimizda
# 0.15 gostermis burda da baya kotu bunun sebebi veri azligimiz
# recall hatirlamaydi o yine iyi sayilir
# sinif 2'yi tahmin etmemiz de kotumus
# balanced accracy degerlerim de 0.7 civarinda ucunun de


confusionMatrix(predModel2,test$cp, mode = "prec_recall")
# accuracy degerim 0.53 cikti dusmus bu da kotu bi durum
# NIR degeri?? 0.64 olmus ayni kalmis
# p value baya baya artmis baya kotu bi sonuc elde etmisiz p value dusmesi lazim
# yani accuracy degerimin NIR degerimden dyuksek olmasi lazim da demek bu
# balanced accuracy degerlerime baktigimda da dusmeler goruyorum
# model 2 kotu bi sonuc vermis bize




confusionMatrix(predModel3,test$cp, mode = "prec_recall")
# Accuracy : 0.5625 gelmis model 2'ye gore artmis modelBase ile ayni gibi
# NIR degerim hepsinde ayni
# p value yine cok yuksek bu da kotu olmus
# model 3 ile modelBase neredeyse 

# modelden cikarttigimiz degiskenler iyi yonde sonuc vermedi
# hatta hic sonuc vermedi ayni nerdeyse 


#### oran esitlemeleri
table(test$cp)
# burdaki oranlardan bahsediyoruz 
# 1 degerimiz cok az oldugundan 1 icin dogru sonuclari alamadik muhtemelen
# oranlari yakinlastirmaya calisicz ve modelleri oyle karsilastiricaz




View(test)


class(test$cp)

test[test$cp=="0",]
testOranlarEsit <- data.frame()

# set.seed kullanmicaz her seferinde farkli veriler kullanmak icin
sampleIndex_0 <- sample(1:nrow(test[test$cp=="0",]), size = 10)
sampleIndex_2 <- sample(1:nrow(test[test$cp=="2",]), size = 10)

testOranlarEsit <- rbind(testOranlarEsit, test[test$cp=="0",][sampleIndex_0,])
testOranlarEsit <- rbind(testOranlarEsit, test[test$cp=="2",][sampleIndex_2,])
testOranlarEsit <- rbind(testOranlarEsit, test[test$cp=="1",])
View(testOranlarEsit)

table(testOranlarEsit$cp)
# evet hepsi 10 adet oldu en azi olanlari aldik.




predModelBaseEsit <- predict(modelBase, testOranlarEsit)
predModelBaseEsit

predModel2Esit <- predict(model2, testOranlarEsit)
predModel2Esit

predModel3Esit <- predict(model3, testOranlarEsit)
predModel3Esit

confusionMatrix(predModelBaseEsit, testOranlarEsit$cp, mode = "prec_recall")
# accuracy oranim artmis gozukuyor bu iyi bir sey
# p va??ue da baya dusmus bayaaa hem de cok iyi
# f1 skorlarima baktigimda sinif 2 icin biraz dusuk bi oran var 
# balanced accuracy oranlarina baktigimizfa 
# sinif 0 cok iyi tahmin edilmis 0.95
# sinif 1 ortalama 0.775
# sinif 2 kotu sayilir 0.675
# kappa degerim de 0.6 ortalama diyoruz


confusionMatrix(predModel2Esit, testOranlarEsit$cp, mode = "prec_recall")
# Accuracy : 0.6667  modelBase'e gore dusmus bu degerim
# prec ve recall degerlerimde dususler mevcut
# f1 skorlarim da dusmus modelBase model2'ye gore daha iyi gibi
# balanced accuracy degerlerim de bunu soyluyor


confusionMatrix(predModel3Esit, testOranlarEsit$cp, mode = "prec_recall")
# accuracy degerim 0.76 en iyi bunda cikti
# p degerim yine dusuk cok iyi
# en yuksek f1 skorlarimi burda aldim
# balanced accuracy degerlerim yine kayda deger artmis
# en iyi modelim diyebiliriz buna yav bir de set.seed yapmadigimiz icin 
# sample indexleri degisip denicem ama onun sonuclari ayni cikarsa biis yazmam 
# farkli cikarsa da aciklamadan yazar gecerim


# genel olarak model3 daha iyi geldi
# ama daha fazla ornege ihtiyac var bu yetersiz model oldu
# ama elimizde veri yok yapcak bisi yok


###### model tuning #####

# Decay kullanacagim bu parametreyi
# lambda gibi katsayilarimizla oynamak icin kullaniyoruz

model3 <-  multinom(cp ~ thalach + 
                      exang + oldpeak + slope +ca + 
                      thal, data = train)


library(caret)

modelTuning <- train(cp ~ thalach + 
                       exang + oldpeak + slope +ca + 
                       thal, data = train,
                     method = "multinom",
                     trControl = trainControl(method = "cv",number = 5))


modelTuning

modelTuning$bestTune
# decay degerimiz 0.1 yap diyo

modelTuning$finalModel

plot(modelTuning)
# cok da degisim olmamis biraz artmis 0.1 kullancaz o yuzden


modelTuned <- multinom(cp ~ thalach + 
                         exang + oldpeak + slope +ca + 
                         thal, data = train,
                       decay = 0.1)
modelTuned





predModelTuned <- predict(modelTuned, testOranlarEsit)
predModelTuned

confusionMatrix(predModel3Esit, testOranlarEsit$cp, mode = "prec_recall")
confusionMatrix(predModelTuned, testOranlarEsit$cp, mode = "prec_recall")
# Accuracy : 0.7 biraz daha iyi yapmis gibi duruyor modelbase ve model2 ye gore
# sinif 2 tahminleri guzellesmis
# her calistirmada degisiyor ama orneklemler de aliyoruz cunku



# tum degiskenler uzerinden
modelTuningAll <- train(cp ~ . , data = train,
                     method = "multinom",
                     trControl = trainControl(method = "cv",number = 5))

plot(modelTuningAll)
# yine 0.1 en iyisi cikmis

modelTuningAll$bestTune
# 0.1 ayni cikti


ModelTunedAll <- multinom(cp ~ . , data = train)

predModelTunedAll <- predict(ModelTunedAll, testOranlarEsit)

confusionMatrix(predModelTunedAll, testOranlarEsit$cp, mode = "prec_recall")
confusionMatrix(predModelBaseEsit, testOranlarEsit$cp, mode = "prec_recall")
# ikisi ayni sonucu verdi aq decay hic etkilemedi


















