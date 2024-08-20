# teorik kisimlari dosyaya yazdim tekrar ederken ordan basla halilim

df <- breast.cancer
# diagnosis icerisindeki B iyimser, M olumcul tumor anlamina gelir

library(tidyverse)
str(df)
varNames <- names(df)
df <-  df[,-which(varNames=="X"|varNames=="id")]
varNames <- names(df)
# id ve x'i kaldirdik


modelData <- df %>% select(diagnosis,radius_mean,texture_mean,perimeter_mean,
                      area_mean,smoothness_mean,compactness_mean,concavity_mean,
                    concave.points_mean,symmetry_mean,fractal_dimension_mean)
View(modelData)

## gorsellestirmeler

plot(modelData$radius_mean, modelData$texture_mean,
     pch = 19, col = c("blue","orange")[modelData$diagnosis])
# kontrol ediyoruz ve lineer dogru isimize yarar gibi duruyor

plot(modelData$area_mean, modelData$perimeter_mean,
     pch = 19, col = c("blue","orange")[modelData$diagnosis])
# 3 daha farkli bir goruntu verdi ve bir dogruyla ayirilabilir gibi duruyor


plot(modelData$smoothness_mean, modelData$compactness_mean,
     pch = 19, col = c("blue","orange")[modelData$diagnosis])
# burda da ic ice girmis bi dagilim duruyor


plot(modelData$radius_mean, modelData$compactness_mean,
     pch = 19, col = c("blue","orange")[modelData$diagnosis])
# ilneer dogruyla ayirabilmemiz gayet mumkun

# lineer iyi gibi duruyor. radian ya da lineer olabilir kernel fonksiyonlar icerisinde
# polimon ve logistik bi g??runtu yok gibi duruyor



##### model olusturma ####


library(e1071)

?svm


table(modelData$diagnosis)
# 357-212 seklinde verilerimiz ayrilmis. fark cok yok diye boyle birakiyoruz


set.seed(125)
index <- sample(1:nrow(modelData), size = nrow(modelData)*0.8)

train <- modelData[index,]
test <- modelData[-index,]
train
test


table(train$diagnosis)
# 283-172 gayet iyi sayilari
table(test$diagnosis)
# 74-40 bu da iyi


modelLinear <- svm(diagnosis~ . , data = train, kernel = "linear")
modelRadial <- svm(diagnosis~ . , data = train, kernel = "radial")
summary(modelLinear)
# number of support vector 72 
# yani bu modelde 72 tane referans alinacak veri bulunmus
summary(modelRadial)
# number of support vector 110
# yani bu modelde 110 tane referans alinacak veri var
# referans alinacak veri ne demek? teori dersidnde gormustuk o cizdigimiz lineer cizgi var ya
# o cizgiye yakin verileri referans aliyorduk burda da o cizgiden referans alinacak veri
# sayisini vermis


modelRadial$coefs
# bunlar support vectorlerim oluyor
modelLinear$coefs
# bunlar support vectorlerim oluyor



### model uzerinden tahminler


predLinear <- predict(modelLinear,test)
predLinear
predRadial <- predict(modelRadial,test)
predRadial
# tahminleri yaptik ve yazdirdik.
# simdi kontrol edelim


library(caret)

confusionMatrix(predLinear, test$diagnosis)
# Accuracy : 0.9474  gayet iyi bir dogruluk sonucu verdi
# accuracy NIR degerimden daha buyuk oldugundan p degerim 0.05den kucuk geldi cok iyi
# kappa degerim de yuksek gelmis baya iyi
# mcnemar yanlis atama oranlarinin benzerligini kontrol ediyo 0.68 yani birbirine esittir
# demis
# Balanced Accuracy : 0.9365
# modelim gayet iyi duruyor
confusionMatrix(predRadial, test$diagnosis)
# accuracy 0.96 dogurluk oranim artmis. iyimser tumor tahminim full tutmus
#  
confusionMatrix(predRadial, test$diagnosis, mode = "prec_recall")

confusionMatrix(predLinear, test$diagnosis, mode = "prec_recall")

# radialdaki f1 skorum 0.97 linearda 0.96. radial daha iyi ama fark yok denecek kadar az

confusionMatrix(predRadial, test$diagnosis, mode = "prec_recall",positive = "M")

confusionMatrix(predLinear, test$diagnosis, mode = "prec_recall",positive = "M")

# bir de pozitif degerimizi kotucul tumore alip tahminlere f1 lere bakalim
# burda da radial ufak farkla daha onde duruyor


#### olasilik turunden tahmin sonuclari #####


modelLinearProb <- svm(diagnosis~ . , data = train, kernel = "linear",probability=T)
modelRadialProb <- svm(diagnosis~ . , data = train, kernel = "radial",probability=T)


predLinearProb <- predict(modelLinearProb, test,probability =T)
predRadialProb <- predict(modelRadialProb, test,probability =T)
# her bir tahmin icin olasilik degerlerini veriyor

predLinearProbValue <- attr(predLinearProb, "probabilities")
# olasilik degerlerini bu sekilde secebildik
class(predLinearProbValue)
# matrix array

predRadialProbValue <- attr(predRadialProb,"probabilities")
class(predRadialProbValue)
# matrix array


##### sonuclari gorsellestirelim#####


plot(modelLinear,data = train, radius_mean~texture_mean)
# x noktalari support vectorlerimiz oluyor
# bazilari ikisinin karistigi yerde oluyor

plot(modelLinear,data = test, radius_mean~texture_mean)

plot(modelLinear,data = test, perimeter_mean~area_mean)
# yine support vectoru goruntuluyoruz


plot(modelRadial,data = test, perimeter_mean~area_mean)
plot(modelRadial,data = test, radius_mean~texture_mean)
# radial modellerimde support vectorlerim daha cok belli oluyor


#### model tuning islemleri #####


# gamma ve cost degerlerini kullanarak tuning islemi yapacagiz

library(e1071)

modelLinearTune <- tune(METHOD = svm,diagnosis~., data = train,
     kernel="linear",
     ranges = list(gamma=2^(-2:2),cost=2^(-4:4)),
     tunecontrol = tune.control(cross = 10))

modelRadialTune <- tune(METHOD = svm,diagnosis~., data = train,
                        kernel="radial",
                        ranges = list(gamma=2^(-2:2),cost=2^(-4:4)),
                        tunecontrol = tune.control(cross = 10))






modelLinearTune
# gamma 0.25 cost 0.125
modelRadialTune
# gamma 0.25 cost 2

# best performance dusuk olmali hatamiz demek bu
# dusuk ise daha iyidir

modelRadialTune$performances
# denemeler ve hatalari yer ariyor


predTuneLinear <- predict(modelLinearTune$best.model, test)
predTuneRadial <- predict(modelRadialTune$best.model, test)
# best model sectigimizde direkt bize modeli veriyor cok guzel bir ozellik
# gamma ve cost degerlerityle yeni model olusturmamiza gerek kalmadi

predTuneLinear
predTuneRadial

confusionMatrix(predTuneLinear, test$diagnosis)
# Accuracy : 0.9561 olmus yuzde 1'lik bir artis var
# kappa degerim 0.90 onceki degeri 0.88idi artmis iyi bir sey
# olumlu durumlari tahmin etmesi cok iyi duruma gelmisken kotu tumoru tahmin etmesi 
# sadece 1 tane veri uzerinde kotuye gitmis
# tumor iyiyken kotu olarak tahmin etmis onu
# p degerim hala olumlu 0.05dnen kucuk
# balanced accuracy degerim 0.93 hic degismemis bu 
# tune'un model uzerimdeki etkisi ufak ve olumlu diyebilirz  

confusionMatrix(predTuneRadial, test$diagnosis)
# accuracy degerim 0.98 olmus bundan once 0.96 idi
# kappa degerim 0.96 olmus 0.92 idi
# iyi tumoru tahmin etme durumu zaten full iken simdi de full devam etmis
# kotu tumoru tahmin etme degeri artmis cok iyi olmus bu durum
# 0.90 iken 0.95 olmus yani 2 deger daha fazla dogru tahmin etti
# balanced accuracy degerim 0.97 olmus onceki deger 0.95idi
# radial modelim cok iyi perdormans gostermis dedik
# tune ile daha da iyi hale geldi


# cv islemi yaparken gamma ve cost degerlerimizin araligini arttirirsak 
# daha iyi bir sonuc aliriz





