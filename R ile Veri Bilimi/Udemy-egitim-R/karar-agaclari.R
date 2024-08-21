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


### model tuning islemi


library(caret)
library(rpart)
library(e1071)


trControl <- trainControl(method = "cv", number = 5, search = "random")
# cp degerini random bul dedil

modelCP <- train(Outcome~.,data= train, method = "rpart", tuneLength = 20,
                 trControl= trControl)

modelCP
# buna gore en iyi cp degeri 0.009 cikmis
modelCP$bestTune # burda da cikiyor

# bir de rpaart2 kullanalim ve maxdepth sayisini bulalim
modelMD <- train(Outcome~.,data= train, method = "rpart2", tuneLength = 20,
                 trControl= trControl)
modelMD
# maxdepth 9 diyor burda da 
modelMD$bestTune # burda verdi sayiyi

trControlGrid <- trainControl(method = "cv", number = 5, search = "grid")
# search grid yapalim bir de
modelCPGrid <- train(Outcome~.,data= train, method = "rpart", tuneLength = 20,
                 trControl= trControlGrid)
modelCPGrid
# 0.04 verdi burda da
modelMDGrid <- train(Outcome~.,data= train, method = "rpart2", 
                 tuneGrid= expand.grid(maxdepth=3:20),
                 trControl= trControlGrid)
modelMDGrid
# maxdepth 11 dedi

modelTuneMin <- tune.rpart(Outcome~., data = train,
                           minsplit = 10:15,minbucket = 5:10,cp = seq(0.0,0.2, by=0.01))

modelTuneMin
# verdigim degerlere gore minsplit 10
# minbucket 10 cp 0 degerini aldi


modelTuneMin$best.model
# best modelim ve diger modelleri karsilastiralim
modelMD$finalModel
modelMDGrid$finalModel


modelTune1 <- predict(modelTuneMin$best.model,test,type = "class") 
modelTuneMD <- predict(modelMD$finalModel, test, type="class")
modelTuneMDGrid <- predict(modelMDGrid$finalModel, test, type="class")
modelCPprec <- predict(modelCP$finalModel, test, type = "class")
modelCPGridprec <- predict(modelCPGrid$finalModel, test, type = "class")

confusionMatrix(modelTune1,test$Outcome, mode = "prec_recall",positive = "1")
# f1 skorum 0.44
confusionMatrix(modelTuneMD,test$Outcome, mode = "prec_recall",positive = "1")
# f1 skorum 0.41
confusionMatrix(modelTuneMDGrid,test$Outcome, mode = "prec_recall",positive = "1")
# f1 skorum 0.41
confusionMatrix(modelCPprec,test$Outcome, mode = "prec_recall",positive = "1")
# f1 skorum 0.42
confusionMatrix(modelCPGridprec,test$Outcome, mode = "prec_recall",positive ="1")
# f1 skorum 0.43


# 1leri tahmin etme oranimiz kotuyken iyilesmis ancak 0i tahmin etme oranimiz da 
# iyiyken kotulesmis. performansimiz genel olarak degerlendirdigimizde dusmus
# iyi bir tuning islemi yapamamisiz 

# hocanin modeli daha iyi sonuclar verdi. ikimizin farkli yaprigi tek nokta ben 
# train sonucalrimi esit aldim 0 ve 1 sonuclari elimde esit vardi
# hocada ise karisikti yani karisik secmek daha iyi sonuclar vermis gibi duruyor
# hem normal modellerde hem de tuning edilmis modellerde

# ben de sonraki konuye gecmeden sonraki videoya gecmeden (300. video) hocanin yaptigi gibi
# bir deneme yapayim ve tune isleminde daha aralikli sayilar birakayim
# bakalim doguruluk oranim nereye akdar gelecek

table(diabetes$Outcome)

index <- sample(1:nrow(diabetes),size = nrow(diabetes)*0.8)
trainDeneme <- diabetes[index,]
testDeneme <- diabetes[-index,]

table(trainDeneme$Outcome)
table(testDeneme$Outcome)
# bu sekilde deneyecegim


library(rpart)
library(rpart.plot)

trainDeneme$Outcome <- as.factor(trainDeneme$Outcome)
testDeneme$Outcome <- as.factor(testDeneme$Outcome)


modelDeneme1 <- rpart(Outcome ~ . ,data = trainDeneme,method = "class",
                      parms = list(split="information"))


modelDeneme2 <- rpart(Outcome ~ ., data = trainDeneme, method = "class",
                      parms = list(split = "gini"))

tahmin1 <- predict(modelDeneme1,testDeneme,type = "class")

confusionMatrix(tahmin1,testDeneme$Outcome,mode = "prec_recall")
# 0'i tahmin etme f1 skorum 0.79. iyi sayilir
confusionMatrix(tahmin1,testDeneme$Outcome,mode = "prec_recall",positive = "1")
# 1'i tahmin etme f1 skorum 0.51 k??t?? sayilir

tahmin2 <- predict(modelDeneme2, testDeneme, type = "class")

confusionMatrix(tahmin2,testDeneme$Outcome, mode = "prec_recall")
# 0'i tahmin etme f1 skorum 0.77
confusionMatrix(tahmin2,testDeneme$Outcome, mode = "prec_recall",positive = "1")
# 1'i tahmin etme f1 skorum 0.48


# genel olarak sadece f1 degil hepsini karsilastime yaptigimda
# information split daha iyi sonuclar vermis onun uzerinde  devam edip ona tune islemleri yapalim
# ama orda da p degerim cok yuksek geldi o da sikinti ona da dikkat edelim



modelDenemeTune <- tune.rpart(Outcome ~ ., data = trainDeneme,
              minsplit = 0:20,minbucket = 0:19,cp = seq(0.0,1, by=0.01))
# yaklasik 20 dakikalik bir tune isleminden sonra testimizi yapalim

modelDenemeTune$best.model
modelDenemeTune$best.parameters
View(modelDenemeTune$performances)
# inceledim 0.32 cp degerinden sonrasi hep ayni standart hata oranini veriyor o yuzden 
# seq fonksiyonu icerisinde 1'e kadar olan yeri 0.32'ye kadar olark degisebiliriz.
# minbucket ve minsplit fonksiyonlarini da bi arastirip yeni model ureticem
# cunku bu modelim iyi bir tahmin yapmadi

tahminTune <- predict(modelDenemeTune$best.model, testDeneme, type = "class")


confusionMatrix(tahminTune, testDeneme$Outcome,mode = "prec_recall")
confusionMatrix(tahmin1, testDeneme$Outcome, mode="prec_recall")

modelDenemeTune2 <- tune.rpart(Outcome ~ ., data = trainDeneme,
                  minsplit = 5:20, minbucket = 5:15, cp = seq(0.001, 0.1, by=0.005))


View(modelDenemeTune2$performances)
modelDenemeTune2$best.parameters

tahminTune2 <- predict(modelDenemeTune2$best.model,testDeneme,type="class")
confusionMatrix(tahminTune2,testDeneme$Outcome,mode = "prec_recall")
confusionMatrix(tahmin1,testDeneme$Outcome,mode = "prec_recall")


# bu modelimizle elde edebilfigimiz max dogruluk orani 0.72 oldu ayni veri setini baska modellerle de deneyelim

library(nnet)
modelLogit <- multinom(Outcome ~ . , data = trainDeneme)


p <- predict(modelLogit,testDeneme,type = "class")

confusionMatrix(p, testDeneme$Outcome,mode = "prec_recall", positive = "1")
# multinominal logistic regeresyon modelinde 0.78 dogruluk verd??
# 1i tahmin etme f1 skorum da 0.58 cikti en yuksegi simdilik bu


modelLogic1 <- glm(Outcome ~ ., data = trainDeneme, family = "binomial")

p1 <- predict(modelLogic1,testDeneme,type = "response")
p1

p1 <- as.factor(ifelse(p1 > 0.5, 1, 0))
# 0 veya 1 degerlerini aldi


confusionMatrix(p1,testDeneme$Outcome,mode = "prec_recall",positive = "1")
# usttekiyle ayni degerlere sahip


library(e1071)
modelLinear <- svm(Outcome ~ . , data = trainDeneme, kernel = "linear")
modelRadial <- svm(Outcome ~ . , data = trainDeneme, kernel = "radial")

p2 <- predict(modelLinear,testDeneme,type="class")
confusionMatrix(p2,testDeneme$Outcome,mode="prec_recall",positive = "1")
# yine ayni degerler bir de radiala bakalim


p3 <- predict(modelRadial,testDeneme,type = "class")
confusionMatrix(p3,testDeneme$Outcome,mode="prec_recall",positive = "1")
# 1'i tahmin etmede 1 deger daha fazla dogru cikti simdilik en iyi model bu oldu
# tune etmeyi deneyelim

modelRadialTune <- tune(METHOD = svm,Outcome~., data = trainDeneme,
                        kernel="radial",
                        ranges = list(gamma=2^(-2:2),cost=2^(-4:4)),
                        tunecontrol = tune.control(cross = 10))


p4 <- predict(modelRadialTune$best.model,testDeneme, type="class")
 
confusionMatrix(p4,testDeneme$Outcome,mode="prec_recall",positive = "1")



###### sonuc ve degerlendirme #####

# bir cok model denedim ve su sonuca ulastim
# tum modellerim 1'i tahmin etmeye calisirken cok zorlaniyor burda da 1'e ait olan veri
# sayima baktigimda yari yariya bir fark goruyorum bunun sorununun bu oldugunu dusunebiliriz
# 1'i tahmin etme konusunda da supoort vector machine modelimde kernelin radial oldugu model
# en iyi sonucu verdi
# hatta genel olarak en iyi sonucumu svm verdi diyebilirim simdi diger videoya gecebilirim




##### regression tree modeli######

# ayni veri seti uzerinden yasi tahmin etmeye calisacagiz

modelReg <- rpart(Age ~ . , data = trainDeneme)
modelReg
tahminReg <- predict(modelReg, testDeneme)
tahminReg

library(caret)
R2(tahminReg,testDeneme$Age)
# 0.29 cikti oldukca dusuk r2
RMSE(tahminReg, testDeneme$Age)
# 9.94 cikti yaslara baktigimizda 9 sayiis cok buyuk bu yuzden r2 az cikti
MAE(tahminReg,testDeneme$Age)
# 6.98 bu da cok cikti

# bu veriler outcome icin toplanmis verilerdir o yuzden bu sonuclarin gelmesi normal
# yasi tahmin etmek burda cok zor olur

summary(modelReg)






