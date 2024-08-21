# birden cok karar agaci modeli olusturup hepsinde denemeler yapar ve cikan sonuclarda
# bir deger hangi agaclarda kac olarak tahmin edilmis ona bakilir
# toplam skorda bir deger en cok ne olarak tahmin edilmisse o olarak kabul edilir



##### model olusturma #####

install.packages("randomForest")
library(randomForest)

modelRF <- randomForest(Outcome ~ . , data = trainDeneme, ntree = 500)
# ntree kac tane karar agaci olsun sayisi
# mtry secim isleminde kac tane degisken girecegimizi giriyoruz
# default olarak degisken sayim uzerinden hedaplaniyor default birakalim


modelRF$err.rate
# OOb hata degerimi gosterir
# 1. agacimdaki hata 0.31 mesela
modelRF$mtry
# 2 gelmis yani ayirma icin 2 degisken kullanilmis


## tahmin yapalim

tahminRF <- predict(modelRF,testDeneme)
tahminRF


confusionMatrix(tahminRF, testDeneme$Outcome,mode="prec_recall")
# dogruluk oranim 0.74 geldi
# 0lar icin dogru atama durumu iyilesmis
# 1 icin olanlarda yine kotu gelnmis
# p degerim kotu gelmis

confusionMatrix(tahminRF, testDeneme$Outcome,mode="prec_recall",positive = "1")
# 1'i tahmin etme f1 skorum 0.56 dusuk geldi


## tune islemi


library(caret)

modelLookup("rf")
# mtry parametresi var


trControlRF <- trainControl(method = "repeatedcv",number = 10,
                            repeats = 3,search = "random")

modelRFTune <- train(Outcome~.,data= trainDeneme, method="rf",
                     tuneLength=20,
                     trControl= trControlRF)

modelRFTune
# tune edilmis modelim??
# 1 verdi mtrye degerim icin


trControlRF2 <- trainControl(method = "repeatedcv",number = 10,
                            repeats = 3,search = "grid")

modelRFTune2 <- train(Outcome~.,data= trainDeneme, method="rf",
                     tuneGrid=expand.grid(mtry = 1:8),
                     trControl= trControlRF2)

modelRFTune2
# 5 verdi en iyi mtry degeri icin


predTune <- predict(modelRFTune$finalModel, testDeneme)
predTune2 <- predict(modelRFTune2$finalModel, testDeneme)

confusionMatrix(predTune, testDeneme$Outcome, mode="prec_recall")
confusionMatrix(predTune, testDeneme$Outcome, mode="prec_recall",positive = "1")


confusionMatrix(predTune2, testDeneme$Outcome, mode="prec_recall")
confusionMatrix(predTune2, testDeneme$Outcome, mode="prec_recall",positive = "1")

# biraz daha duzelme var ama hala kotu modelim var
# ama ayni diyebiliriz yine de 


















