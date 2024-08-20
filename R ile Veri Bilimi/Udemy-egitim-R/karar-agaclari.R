install.packages("rpart")
library(rpart)
install.packages("rattle")
library(rattle)
# gerekli paketleri yukledik

view(diabetes)
# outcome 0 diabet yok
# outcome 1 diabet var demektir

library(mice)
md.pattern(diabetes)
# kayip gozlemlerim bulunmuyor


table(diabetes$Outcome)
# 500:0,268:1 var
# burda yakin diyemeyiz hoca bu sekilde birakip alicak ben de olusturdugumuz
# fonksiyonla ayrim yapicam

df <- trainTestSplit(data = diabetes,dvName = "Outcome",seed = 145)
train <- df[[1]]
test <- df[[2]]

table(train$Outcome)
# 214-214 olarak aldik

table(test$Outcome)
#286-54 olarak geldi test setimiz de


##### model olusturma ####

train$Outcome <- as.factor(train$Outcome)
test$Outcome <- as.factor(test$Outcome)


modelEntropy <- rpart(Outcome~., data = train,method = "class",
      parms = list(split = "information"))

modelGini <- rpart(Outcome~., data = train,method = "class",
      parms = list(split = "gini"))

modelEntropy
modelGini


# yorumlayalim

modelEntropy
# n = 428 baslangictaki veri sayimiz
# * olanlar yaprak nodelarimiz diger adiyla terminal nodelarimizdir
# yorumlanmasi cok cok uzun sadece dinlicem ama baktin unuttun
# tekrar icin geldin udemydeki 293. videoyu ac



modelGini
# hocada herhalde verileri esit almadigindan dolayi modeller arasinda farklar cikti
# benim modelimde egitim modelimi esit ayirdigim icin daha stabil sonuclar verdigini
# tahmin ediyorum



##### karar agaci gorsellestirmesi #####
library(rattle)
fancyRpartPlot(modelEntropy)
# gorsellestirme cok rahat yorumlayabilirsin bunu halilim
# rattle paketinde bu ama unutma
# truelar hep solda olur unutma

install.packages("rpart.plot")
install.packages("rpart")

library(rpart.plot)
rpart.plot(modelEntropy)

##### model detaylari

summary(modelEntropy)
# cok uzun bir sonuc. video 295

summary(modelGini)
# ikisinin onemlilik katsayilarinda farkliliklar var ama sonucu etkilememis
# yani tamamen ayni diyemeyiz onemli



##### karar agaclari hiper parametreleri ####

?rpart.control()


modelEntropyHyper <- rpart(Outcome~., data = train,method = "class",
                      parms = list(split = "information"),
                      control = rpart.control(minsplit = 40,
                      cp = 0.02,maxdepth =5))
# sayilari kafadan attik ileride tuning ile daha iyi sonuclar alabiliriz
modelEntropyHyper
# agac ufaldi sebebi minsplit arttirdik
# maxdepth azalltik bunlar yuzunden azaldi


rpart.plot(modelEntropyHyper)

summary(modelEntropyHyper)


##### tahminler yapalim #####

predEntropy <- predict(modelEntropy, test,type = "class")
predGini <- predict(modelGini, test,type = "class")
predEntropyHyper <- predict(modelEntropyHyper, test,type="class")



predEntropy
# olasilik bazinda verdi

predEntropy <- predict(modelEntropy, test, type = "class")
predEntropy
# sinif olarak verdi. hespinin tipini sinif yapalim yukarida yapicam ben
predEntropyHyper
predGini
# hepsi direkt 1 veya 0 verdi oraqnlar aradan kalkti


confusionMatrix(predEntropy,test$Outcome)
# yorumlamayi ogrendin zaten uzun uzun aciklamicam kisa yorum yapcam
# dogrulukoranim ortalama 0.7
# p value 1 gelmis cokkkkk sikinti bu bunu istemioduk kappa da kotu gelmis baya
confusionMatrix(predEntropy,test$Outcome,mode = "prec_recall")
# 0'i tahmin etme noktasinda iyiyiz diyor f1 skorum 0.8 bir de 1i tahmin etmeye bakalim
confusionMatrix(predEntropy,test$Outcome,mode = "prec_recall",positive = "1")
# bu degerler baya kotu geldi. 1i tahmin etme konusunda kotuyuz 0.4 baya kotu

confusionMatrix(predEntropyHyper,test$Outcome)
confusionMatrix(predEntropyHyper,test$Outcome,mode = "prec_recall")
confusionMatrix(predEntropyHyper,test$Outcome,mode = "prec_recall",positive = "1")
# yine ustteki gibi degerler ufacik artmis ama yine de 1i tahmin etmede kotuyuz byaa


confusionMatrix(predGini,test$Outcome)
confusionMatrix(predGini,test$Outcome,mode = "prec_recall")
confusionMatrix(predGini,test$Outcome,mode = "prec_recall",positive = "1")
# en usttekiyle ayni hepsi bu da 0 icin iyi 1 icin cok kotu baya kotu








