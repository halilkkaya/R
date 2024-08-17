library(caret)
library(glmnet)
library(tidyverse)


placement <- placement %>% select(gender,ssc_p,ssc_b,hsc_p,hsc_b,hsc_s,degree_p,
                                  degree_t,workex,etest_p,specialisation,mba_p,status)


placement <- placement[, !names(placement) %in% c("sl_no", "salary")]

# ikisinin de islevi aynidir


## veri onisleme yapalim

# kullanmayacaklarimizi attik zaten

table(placement$status)
# 67 ise girmeyen 148 ise giren varmis
# aralarinda dengesizlik var bunlari yaklasik esit yapmamiz gerek train setimiz icin

place <- placement %>% filter(status == "Placed")
Notplace <- placement %>% filter(status == "Not Placed")

nrow(place)
# 148
nrow(Notplace)
# 67

set.seed(155)
placeIndex <- sample(1:nrow(place), size = nrow(Notplace)*0.75)
set.seed(155)
NotplaceIndex <- sample(1:nrow(Notplace), size = nrow(Notplace)*0.75)


trainPlaced <- place[placeIndex,]
trainNotPlaced <- Notplace[NotplaceIndex,]

train <- rbind(trainPlaced,trainNotPlaced)
# verilerimi birlestirdim
table(train$status)
# 50 not placed 50 placed aldim
# tarin icin ogretme islemi icin esit olmasi tahmin yapabilmek adina cok onemlidir


testPlaced <- place[-placeIndex,]
testNotPlaced <- Notplace[-NotplaceIndex,]


test <- rbind(testPlaced,testNotPlaced)
# verileri birlestirdim
table(test$status)
# burda verilerin daginik olmasi sorun degil. ogretirken dogru ogretip
# sonra bu sekilde test edicez zaten


##### model olusturma ####

?glm


## modelLogic <- glm(status~ ., data = train, family = binomial(link = "logit"))
modelLogic <- glm(status~ ., data = train, family = "binomial")
# ikisi de ayni

modelLogic
# kendi kendime dummy degiskenligini uyguladi
# mesela eger gender M ise secilecek kisinin secilme puani 4.6 artiyo ssc_p
# ssc_p puani 0.38 etkiliyormus gibi gibi yorumla
# workex yes ise 2.47 etki yapiyomus


summary(modelLogic)
# yildizli olanlarin anlamli etkileri vardir demek. yani p < 0.05 olanlaar
# anlamli etki birakiyor demektir



##### anova ile degisken etkilerini ehsaplama ve degiskenlerin onemi #####

anova(modelLogic)
# sadece bu kadar onmeli olan yorumlamak. hadi yorumlayalim
# deviance skorlarim cok onemli ne kadar dusuk olursa modelim o kadar iyi tahmin
# yapiyor demektir. null deviance hic degisken yokken demektir
# deviance resid. yazan kisim modelimde o degisken gelince ne kadar deviacne goturdugunu
# yaziyor. gender yokken 138.5 imis gender 6.5 gondermis ve 132 olmus gibii
# ssc_p yokken 132 imis ssc_p 52 indirmis bayyyaa onemli mesela bu degisken
# 79a dusmus sonra.
# p degerim 0.05den dusuk olanlar daha cok dusuruyo gibi gozlemkledim
# ne kadar buyukse p degerim o kadar az olyor gibi
# etkisi az olanla napalim? degisken sayim arttikca complexitym artiyordu bu da 
# degisken varyansliliga surukluyodu en sonunda da overfitting sorunu cikiyordu
# o yuzden karmasikligi azaltalim 

summary(modelLogic)
# karsilastimak icin koydum


# degiskenleri cikartmadan once bi model yapalim bir de cikarip yapalim
# tahmin durumlarina gore hangi modeli seccegimize karar veririz

# diger yontem de caret paketi icerisindeki paketle yapiliyor
# variable importance fonksiyonu 

varImp(modelLogic)
# overall skorlarimda yuksek olan etkili demektir
# dummy degiskenleri ile ayni kategoriye ait diger degiskenlerin biri iyi etki saglarken
# digeri kotu etki sagliyor. genel olarak birakmak kullanilir ama itsersen etkisi az olan
# kategoriyi silebilirsin ama silme islemi nasil olur bilmiom vla



##### model uzerinden tahmin yapma ####

install.packages("InformationValue")
library(InformationValue)
# bu paket yokmus ama

predict(modelLogic,test)
# sonuclarim 0 ile 1 arasinda gelmeli logistic regresyon yapmamin sebebi bu zaten
# sonra da 0.5den buyukler 1 0.5den kucukler 0 olmali seklinde caseler atamamiz gerek

predictions1 <- predict(modelLogic,test, type = "response")
# sonuclar 0-1 arasinda geldi
preedictions2 <- plogis(predict(modelLogic, test))
# bu da ayni sonuclari veriyor ikisi de kullanilabilir
class(test$status)

class(predictions1)

predictions1 <- as.factor(predictions1)

predictions1 <- predictions1 %>%
  mutate_all(~ if_else(. > 0.5, 1, 0))

predictions1 <- as.factor(ifelse(predictions1 > 0.5, 1, 0))
test1 <- as.factor(ifelse(test$status == "Placed", 1,0))
# burada Placed=1 Not Placed = 0 yaptik 
# hem predict isleminmcedki sonuclari hem de testteki sonuclari o sekilde hallettik
# videodaki gibi olmuyor cunku kutuphane kaldirilmis
table(predictions1)
confusionMatrix(test1, predictions1)
# simdi burda
#           Reference
# Prediction   0  1
#         0   13  4
#         1   23 75
# seklinde bi sonuc verdi
# ust kisimda 1= placed 0= not placed yapmistik onu unutmamak gerek. hocanin fonksiyonunda buna gerek yoktu
# ama o kaldirilmis yerine bu var o yuzden aklimizda tutmak zorundayiz
# satir satir bakmamiz gerekiyor burda 0 iken 0 gelen 13 0 iken 1 gelen 4 sonucumuz var demek bu
# 1 iken 0 gelen 23 1 iken 1 gelen sonucumuz 75 imis 
# simdi bu sonuclari yuzdelik hale getirmemiz gerek


confTest <- confusionMatrix(test1, predictions1)
confTest$table
# tabloyu/matrixi bu sekilde aliyoruz 
# hepsini toplayalim ve dogru atamalari alalim 0-0, 1-1 olanlari toplicaz
# dogru atamalari tum sayilara bolcez

(confTest$table[1,1]+ confTest$table[2,2])/sum(confTest$table)
# 0.76 cikti. dogruluk oranim 0.76 yani %76 dogru tahmin yapmisim. bunu bi fonksiyon haline getirelim

accur <- function(x){
  table <- x$table
  accuarcy <- (table[1,1]+table[2,2])/sum(table)
  return(accuarcy)
}

accur(confTest)
# normalde daha dinamik bi fonksiyon icin for dongusu ya da apply ile tablo icerisinde dolassabilirdim
# ancak buna gerek yok cunku logistic regresyonda 1 veya 0 var yani max 2x2 matrix gelebilri
# ve 1,1/2,2 kisimlari hep dogrulari verecek bu yeterli o yuzden
# sonucu kontrol ettigimizde de veriyor cevabi

1- accur(confTest)
# hata orani bu da

confTest
# aslinda dogruluk burda yaziyomus direkt la accuracy kisminda 

##### optimum esik deger ####



# basa donmem gerek cunku bazi kutuphaneler silinmis. benim de ROCR kutuphanesi icin
# verileri 1 ve 0lardan arindirip eski haline getirmem gerek 
trainNormal
testNormal
modelLogicNormal
# yukarida bunlari bu sekilde almistim predict isleminden devam edelim

predNormal <- predict(modelLogicNormal, testNormal, type = "response")
predNormal

table(ifelse(predNormal>0.5,1,0))
# hangi sayidan kac tane var gorduk 1 ve 0 olarak atamadan bu sekilde baktik

as.factor(ifelse(predNormal>0.5,1,0))

tableNormal <- confusionMatrix(as.factor(ifelse(testNormal$status=="Placed",1,0)),
                               as.factor(ifelse(predNormal>0.5,1,0)))
tableNormal
# hic sonuclari degismeden 0 ve 1ler seklinde tablomu verdi artik degerleri vs verdi
tableNormal$table
# simdi burda
# Reference
# Prediction   0  1
#         0   13  4
#         1   23 75
# seklinde bi sonuc verdi
# ust kisimda 1= placed 0= not placed yapmistik onu unutmamak gerek. hocanin fonksiyonunda buna gerek yoktu
# ama o kaldirilmis yerine bu var o yuzden aklimizda tutmak zorundayiz
# satir satir bakmamiz gerekiyor burda 0 iken 0 gelen 13 0 iken 1 gelen 4 sonucumuz var demek bu
# 1 iken 0 gelen 23 1 iken 1 gelen sonucumuz 75 imis 
# simdi bu sonuclari yuzdelik hale getirmemiz gerek
accur(tableNormal)
# dogruluk orani kendi fonksiyonumuz ile bulduk

## simdi istedigim testleri yaptim ve veriler elimde hala diger ustte yaptigima gore "ham" sayilir sekilde
# bu yuzden veriler uzerinde cutoff degerleri bulmayi oynayabilirim golll
predNormal
summary(predNormal)


install.packages("ROCR")
library(ROCR)

rocr_pred <- prediction(predNormal, testNormal$status)
rocr_perf <- performance(rocr_pred,"tpr","fpr")
plot(rocr_perf,colorize=T,print.cutoffs.as=seq(0.1,by = 0.1))

cost_perf <- performance(rocr_pred,"cost")
rocr_pred@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]
# 0.02355532 degerini aldim bunu bi yere atayalim dursun

optCutoff <- rocr_pred@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]


cmOpto <- confusionMatrix(as.factor(ifelse(testNormal$status=="Placed",1,0)),
                          as.factor(ifelse(predNormal>0.5,1,0)), threshold = optCutoff)
# bu calismiyor sebebi bu fonksiyon caret paketinde ve threshold parametresi yok
# chatgpt ile soyle bi cozum bulduk

predBinary <- ifelse(predNormal > optCutoff, 1, 0)

cmOpt <- confusionMatrix(as.factor(predBinary), as.factor(ifelse(testNormal$status=="Placed",1,0)))

accur(cmOpt)
# 0.91 dogruluk orani verdi baya bi artti hatta cok iyi 

tableNormal$table
cmOpt$table
# dogru sayilarim baya artmis



##### precision ve recall degerlerinin hesaplanmasi #####

# bu degerlerimin ne oldugu ve matematiksel olarak nasil hesaplandigini
# dosyada belirttim tekrar ederken oraya don halilim
library(caret)
library(ROCR)
library(tidyverse)


cmOpt_1 <- confusionMatrix(as.factor(predBinary),
                         as.factor(ifelse(testNormal$status=="Placed",1,0)))


cmOpt_1



colnames(cmOpt_1$table) <- c("Not Placed (Negative)","Placed (Positive)")
rownames(cmOpt_1$table) <- c("Not Placed (Negative)","Placed (Positive)")

cmOpt_1$table
# isimler istedigimiz sekilde geldi

# precision 
# true positive/ true positive+false positive

precision_1 <- cmOpt_1$table[2,2] / (cmOpt_1$table[2,1]+cmOpt_1$table[2,2])
precision_1
# 0.94 
# modelimdeki placed degerinin precision degeriydi bu unutma
# precision satir bazinda gider unutma

# recall haesaplama

# trueb pozitive / (true pozitive + false negative)
recall_1 <- cmOpt_1$table[2,2] / (cmOpt_1$table[2,2]+cmOpt_1$table[1,2])
# sutun bazinda gittik
# 0.9591 cikti

# modelim placed'i tahmin ederken gayet iyi sonuc veriyor
# ogrencinin atanirken atanir derken iyi tahmin ediyor
# ancak not placedi hesaplamadik bir de onu deneyelim


precision_2 <- cmOpt_1$table[1,1] / (cmOpt_1$table[1,1]+cmOpt_1$table[1,2])
# precision satir bazindaydi unutma. yani soyle diyelim predictionslar uzerinden bakar bu
# 0.73 verdi yani not placed tahmin etme yetenegi placedi tahmin etmesinden daha kotu diyebilir


recall_2 <- cmOpt_1$table[1,1] / (cmOpt_1$table[1,1]+cmOpt_1$table[2,1])
# recall sutun bazindaydi unutma yani test setim uzerinden
# 0.64 verdi. hatirlama orani yani dogru bilme orani 0.64



# 10 isim var bu 10 ismi 20 isim saydiktan sonra bilince su oranlar ortaya cikar
# recall (hatirlama) = 1.00
# precision ( tahmin etme) = 0.5

# yani buna gore yorumla 
# placed'i iyi tahmin ederken not placed'i iyi tahmin edemiyor



##### specificity ve sensitivity degerleri hesaplama #####

cmOpt_1$table

#sp ecificity

spec_1 <- cmOpt_1$table[1,1] / (cmOpt_1$table[1,1]+cmOpt_1$table[2,1])
#0.64

# sensitivity
sens_1 <-  cmOpt_1$table[2,2] / (cmOpt_1$table[2,2]+cmOpt_1$table[1,2])
# 0.95


##### f1 skorunun hesaplanmasi #####


f1_1 <- 2*((precision_1*recall_1)/(precision_1+recall_1))
f1_1
# 0.95 geldi. place modeli icin yani. place'i tahmin ederkenki performansim

f1_2 <- 2*((precision_2*recall_2)/(precision_2+recall_2))
f1_2
# 0.68 geldi. not place modeli icin yani. not place'i tahmin ederkenki performansim

# f1 skorumun .75-80 ustunde gelmesi daha iyi olur


##### roc curve ve auc metrikleri #####

library(pROC)

cmOpt_1 <- confusionMatrix(as.factor(predBinary),
                           as.factor(ifelse(testNormal$status=="Placed",1,0)))



?roc


rocModel_1 <- roc(ifelse(testNormal$status=="Placed",1,0) ~ predBinary)
# Setting levels: control = 0, case = 1
# Setting direction: controls < cases
# seklinde aciklama verdi controller 0'i caseler 1'i ifade ediyor gibi dusun


plot(rocModel_1)
# sensitivity ve specificity aralarindaki degisimin modele etkisini goruyroz
# bu cizilen grafigin altinda kalan alan ne kadar buyukse
# modelimiz o kadar iyi diyoruz. cunku specificity degerimiz 1 iken
# sensitivity degerimiz de artisa gecip dumduz cikarsa ikisinin de 
# yuksek oldugu zaman bizim en cok isimize yarayan zaman olur.
# bunu dikkate al
# modelimizin cizgisi ortadaki cizgiye ne kadar yaklasirsa modelimiz kotulesir
# modelimizin cizgisi ortadaki cizginin de altina giderse model cok kotu demektir


rocModel_1
# are under the curve 0.80 geldi alttaki alani soyluyor
# auc oluyo yani
# tahmin performansimiz iyi sayilir 0.85-0.75 ustunde olmasi gerekir bu sayinin

##### caret paketi ile confusion matrix #####


# ben aslinda hep caret ile gittim ama bakalim hoca napcak


cmOpt_1

# heeeeee bak burda tum degerler vaaaarrrrrr
# bunu olusturmustum ve altinda cikan digerkleri ne diyodum
# tam bunlarmis cok iyiiiiii

rocr_pred <- prediction(predNormal, testNormal$status)
rocr_perf <- performance(rocr_pred,"tpr","fpr")
plot(rocr_perf,colorize=T,print.cutoffs.as=seq(0.1,by = 0.1))

cost_perf <- performance(rocr_pred,"cost")
rocr_pred@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]
# 0.02355532 degerini aldim bunu bi yere atayalim dursun

optCutoff <- rocr_pred@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]


predBinary1 <- ifelse(predNormal > optCutoff, "Placed", "Not Placed")

cmOpt_2 <- confusionMatrix(as.factor(predBinary1), 
                         as.factor(testNormal$status))

# bu adimlari tekrar ekledim buraya
cmOpt_2

# hazir hale getirdim yeni modelimi
# pozitif class Not Placed olarak geldi dikkat et. istersen degisebiliriz
# dogruluk yani Accuracy : 0.913 gelmis
# P-Value [Acc > NIR] : 0.03731 yani accuracy degerim no information rate degerimden
# buyuk olmalidir. eger buyuk ise basarili bi modeldir olasilik degerim 0.03 gelmis
# eger bu deger 0.05den kucukse acc > nir demektir 0.03 gelmis
# yani %97 acc>nir demektir bu sonuc



# kappa ve mcnemar diger videoda islencekmis

# sensitivity 0.64 gelmis biz yukarida 0.95 bulmustuk sebebi ise su
# biz place'i pozitif almistik ancak burda not place pozitif degerimiz
# o yuzden bu fark geldi


cmOpt_3 <- confusionMatrix(as.factor(predBinary1), 
                           as.factor(testNormal$status),
                           positive = "Placed")
cmOpt_3
# bak burda direkt kendimiz belirledik ve sensitivity 0.95 geldi. dikkat et bu ayrima
# bu yeni cmOpt_3 uzerinden degerlendirmeye devam edicem cunku pozitif degerler
# istedigimiz gibi yani placed gibi olsun daha iyi yorumlayalim
# pos pred value degerimiz 0.94 bu da yani pozitiflerin dogru tahmin edilme orani
# neg pred value ise 0.73 bu da negatiflerin dogru tahmin edilme orani
# Prevalence : 0.8522 bu da pozitiflerin yogunlugunu belirtiyor. yani %85 pozitif deeger
# yogunlugu mevcut diyor
# Detection Rate : 0.8174 pozitif true / hepsi demektir
# Detection Prevalence : 0.8696 (pozitif true + pozitif false)/ hepsi demektir
# Balanced Accuracy : 0.8031 onemli bu dengeli guven duzeyi demektir

cmOpt_4 <- confusionMatrix(as.factor(predBinary1), 
                           as.factor(testNormal$status),
                           positive = "Placed",mode = "prec_recall")
cmOpt_4
# burda da sensitivity ve specificity yerine precision ve recall degerlerim geldi
# bir de f1 skorum geldi


cmOpt_4$byClass
# butun skorlari tek tek verdi




##### mcnemar ve kappa degerlendirme#####



cmOpt_3
# kappa 0.61
#  Mcnemar's Test P-Value : 0.75183 

# kappa 0.1-0.2 kotu
# kappa 0.2-0.4 kotu idare eder
# kappa 0.4-0.6 orta
# kappa 0.6-0.8 iyi
# kappa 0.8-1.0 mukemmel


cbind(predicted = as.character(predBinary1), actuals= as.character(testNormal$status))
# tahmin ve gercek degerlerim
# kappa bunlarin benzerligini kontrol ediyor

cmOpt_3
# 0.63 degerini almisiz biz iyi sayiliyor.


# mcmenar testi
# falselarin (pozitif false ve negatif false) birbirine ne kadar benzer oldugunu olcuyor
# h0: benzerdir
# ha: benzer degildir
# p degerim 0.75 yani burdaki oranlar birbirine benzer deniyor
# yanbi diyoruz ki her iki sinifta da (placed ve not placed) yanlis atama oranlarim esit mi
# bunu ogrenmek icin kullaniyorz
# sonucum iki sinif arasindaki yanlis atama oranlarim birbirine esit diyoruz



###### logistic regresyon regularization ile model tuning ####

library(glmnet)

# L1 ve L2 regularizationlari kullancaz yine


summary(modelLogicNormal)
# etkisiz elemanlari kontrol ediyoruz. etkisiz olanlarin katsayilari uzerine calisiyoruz
# daha onceden yaptigimiz gibi. min lambda degerini bulucaz. istersek de degiskenleri de
# cikarabiliriz. cross validation kullancz

# veri onisleme kismindan alicaz islemleri

head(placement)

## dummy degisken olusturma
# glm paketinde kullanabilmek icin
modelDataDummy <- model.matrix( ~ . ,data = placement)
View(modelDataDummy)

modelDataDummy <- modelDataDummy[,-1]
# intercepti cikarttik
modelDataDummy <-as.data.frame(modelDataDummy)

library(tidyverse)

placeDummy <- modelDataDummy %>% filter(statusPlaced == 1)
NotplaceDummy <- modelDataDummy %>% filter(statusPlaced == 0)

nrow(placeDummy)
nrow(NotplaceDummy)

set.seed(15)
placeIndexDummy <- sample(1:nrow(placeDummy), size = nrow(NotplaceDummy)*0.75)
set.seed(15)
NotplaceIndexDummy <- sample(1:nrow(NotplaceDummy), size = nrow(NotplaceDummy)*0.75)

 
trainPlacedDummy <- placeDummy[placeIndexDummy,]
trainNotPlacedDummy <- NotplaceDummy[NotplaceIndexDummy,]

trainDummy <- rbind(trainPlacedDummy,trainNotPlacedDummy)
table(trainDummy$status)


testPlacedDummy <- placeDummy[-placeIndexDummy,]
testNotPlacedDummy <- NotplaceDummy[-NotplaceIndexDummy,]


testDummy <- rbind(testPlacedDummy,testNotPlacedDummy)
table(testDummy$status)


## veri onisleme bitti Regularization islemlerine gecelim
library(glmnet)

y = trainDummy$statusPlaced
x = trainDummy[-(length(trainDummy))]

x = as.matrix(x)


modelLogitLassoCV <- cv.glmnet(x, y, alpha =1 , family = "binomial" )
modelLogitLassoCV
# alpha 1 oldugundan lasso dedik 0 olsa r??dge oluyodu 0-1 arasinda olsa elastic

modelLogitLassoCV$glmnet.fit

plot(modelLogitLassoCV)

lambdaMin <- modelLogitLassoCV$lambda.min
# en iyi lambda degerim

coef(modelLogitLassoCV,modelLogitLassoCV$lambda.min )
# katsayilarim geldi ve . koyulanlar kaldirilan degiskenler demek.
# hangi degiskenim hangi katsayiyla etki veriyor gibisinden

summary(modelLogicNormal)

# model olusturalim

modelLogitLasso <- glmnet(x, y, alpha =1 , family = "binomial", lambda = lambdaMin)

modelLogitLasso
modelLogicNormal

modelLogitLasso$nulldev
# 138
deviance(modelLogitLasso)
# 53

# residual deviance degerim artmis modelimi kotu etkilemis yani onceki 34 idi
# test seti uzerinden tahmin yapalim

testControl <- as.matrix(testDummy[,-15])
# bizim tahmin etmeye calistigimiz deger olan statusu cikarip digerleirni de matrix turune donusturduk
head(testControl)

actuals <- testDummy[,15]

predLasso <- predict(modelLogitLasso, testControl, type = "response")
# skorlar elde edilir. response o yuzden kondu
predLasso


# cutoff degerimi bulalim
rocr_predLasso <- prediction(predLasso, testDummy$status)
rocr_perfLasso <- performance(rocr_predLasso,"tpr","fpr")
plot(rocr_perfLasso,colorize=T,print.cutoffs.as=seq(0.1,by = 0.1))

cost_perfLasso <- performance(rocr_predLasso,"cost")
rocr_predLasso@cutoffs[[1]][which.min(cost_perfLasso@y.values[[1]])]
# 0.12 degerini aldim bunu bi yere atayalim dursun

optCutoffLasso <- rocr_predLasso@cutoffs[[1]][which.min(cost_perfLasso@y.values[[1]])]
optCutoffLasso


predActuals <- ifelse(actuals==1,"Placed","Not Placed")
predBinaryLasso <- ifelse(predLasso > optCutoffLasso, "Placed", "Not Placed")



library(caret)

confusionMatrix(as.factor(predBinaryLasso),as.factor(predActuals), positive = "Placed")
# yotumlayalim
# dogurluk 0.92 cok iyi
# p degerim 0.008 cok iyi
# kappa 0.7'ye cikmis iyi bi degisim
# mcnemar 0.75den 0.72ye dusmus kotu olmus bu
# Sensitivity degerim cok ufak artmis iyi
# specificity degerim artmis yani not place degerlerimi dogru tahmin etme oranim artmis
# Neg Pred Value degerim artmis o da aynisini soyluyo bi usttekiyle
# Balanced Accuracy degerim de artmis bu da dengeli dogruluk yani hem placed hem de not placed tahmin
# etme dogrulugum oluyo artmasi iyi bisi

confusionMatrix(as.factor(predBinaryLasso),as.factor(predActuals), positive = "Not Placed")
# not placed ile de akrsilastirdil
cmOpt_2 # not placed eski model
cmOpt_3 # placed eski model
# karsilastiralim
confusionMatrix(as.factor(predBinaryLasso),as.factor(predActuals), positive = "Placed",
                mode = "prec_recall")
cmOpt_4

# f1 skorlarina bakalim bir de
# placed icin cok cok yakin degerler ama yeni modelde daha da artmis cok iyi
# notplaced icin de bakalim

confusionMatrix(as.factor(predBinaryLasso),as.factor(predActuals), positive = "Not Placed",
                mode = "prec_recall")  # yeni

confusionMatrix(as.factor(predBinary1), 
                           as.factor(testNormal$status),
                           positive = "Not Placed",mode = "prec_recall") # eski
# not placedlar icersinde yeni modelim cok daha iyi tahmin yapabildigini gorduk
# degerler iyi bi sekilde artmis ve hepsi 70 ustune cikmis hatta 80 olan var cok iyiyiii 


# yeni modelim daha iyi gibi gozukuyor. lasso guzel yonde etkilemis gibi duruyor






