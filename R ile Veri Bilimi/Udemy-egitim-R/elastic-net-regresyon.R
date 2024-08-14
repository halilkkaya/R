 ##### elastic net model olusturma ####
 library(tidyverse)
 library(caret)
 library(glmnet)
 
 modelData <- mtcars%>% 
   mutate(cyl = as.factor(cyl), vs = as.factor(vs),
          am = as.factor(am), gear = as.factor(gear),
          carb = as.factor(carb))

modelDataDummy <- model.matrix(~ . ,data= modelData) 

set.seed(155)
sampleIndex <- sample(1:nrow(modelDataDummy), size = 0.75*nrow(modelDataDummy))


trainData <- modelDataDummy[sampleIndex,]
testData <- modelDataDummy[-sampleIndex,]

trainData_x <- trainData[,-c(1,2)]
trainData_y <- trainData[,2]


testData_x <- testData[,-c(1,2)]
testData_y <- testData[,2]

lambdas <- 10^seq(2,-2 , by = -0.01)

modele <- cv.glmnet(trainData_x,trainData_y, alpha = 0.5, lambda = lambdas,nfolds = 3)
modele$lambda.min
# en iyi lambda degerim 0.28 verdi
best_lambda <- modele$lambda.min


modelEnet <- glmnet(trainData_x,trainData_y, alpha = 0.5, lambda = best_lambda)
modelEnet$beta
# bazi degiskenlerim 0 olmus . konmus 
modelEnet$dev.ratio
# dev raito degerim 0.88 gayet iyi bir sonuc modelim iyi cikmis diyo

predEnet <- predict(modelEnet, testData_x)

R2(predEnet,testData_y)
# 0.74 degerinde R2 geldi gayet iyi tahin yapmisiz
RMSE(predEnet,testData_y)
# hata kareler toplami ortalamasi 3.12
MAE(predEnet,testData_y)
# hata ortalamalari 2.67

# iyi gekldi sonucalr bir de alpha degerimi degisip ona uygun lambdayi bulup baska model 
# yapayim



modelLambda <- cv.glmnet(trainData_x,trainData_y, alpha=0.5,lambda = lambdas,nfolds = 8)
modelLambda$lambda.min

bestlmd <- modelLambda$lambda.min
# lamda 0.74
model1 <- glmnet(trainData_x, trainData_y, alpha = 0.5,lambda = bestlmd)

pred1 <- predict(model1, testData_x)

R2(pred1, testData_y)
#0.78
RMSE(pred1, testData_y)
# 2.89
MAE(pred1, testData_y)
#2.45

# yakin sonuclar verdi
# cok onemli bisi kesfettim her seferinde lambda degerini farkli veriyor bu durum cv.glmnet icindeki
# nfolds ile gelen rastgelelik yuzunden. set.seed ile bunu sabitleyebilirz ama hangisinin best lambda olacagini bilmiyorum
# kucuk veri oldugundan varyansi buyuk geliyo bi 0.2 lambda geliyo bi 1.5 geliyo
# chate sorim bi
# o da veri setimiz kucuk oldugundan lambda secerken nfolds sayisi kucuk ise orda secilen veriler
# belli bi yonde olabilir diyo. ondan olmus olabilir diyo
# test sonuclarimda buyuk etkileri var mi onu kontrol edim bir de buyuk bi lambda ile
# ilk modelimde lambda 0.25
# sonraki modelimde 0.74
# sonuclar ise yakin geldi
# alpha degeri de lasso ve ridge arasinda yer aliyo o yuzden 0.5 dedim
# onu da degisebilirz istersewk
# caret paketinde de en iyi alpha ve lambda degerini bulabilirz
# cv.glmnet alpha icin calismiyo
# ikisinin beraberken bulan bi yapi var caretta

##### cross validation ile alpha ve lambda bulma #####

library(caret)
rtControl <- trainControl(method = "repeatedcv", number = 8,repeats = 5,
             search = "random",
             verboseIter = T
             )
# number nfolds sayisi oluyo
# repeats tekrarlama sayisi
# random olarak kafadan alpha ve lambdayi rastgele bul

fitElasCV <-  train(trainData_x,trainData_y, method = "glmnet",
                    tuneLength = 30,
                    trControl = rtControl  )
# her bi tune icin 30 kombinasyon yapcak
# alpha 0.08 lambda 1.22 verdi
# bunun dev degeri 0.90 gelmis baya iyiiiii

fitElasCV
fitElasCV$bestTune
fitElasCV$finalModel


##### cross validation ile alpha ve lambda tune grid####

gridAlphaLambda <- expand.grid(alpha = seq(0, 1, by = 0.05), 
                               lambda = seq(0,1, by = 0.1))

# her bir alpha degeri icin once 0.00 sonra 0.01 gibi lambda atip her sekilde kombinasyon yapiyo



trControlGrid <- trainControl(method = "repeatedcv", number = 8,repeats = 5,
                          verboseIter = T)




fitElasCVGrid <-  train(trainData_x,trainData_y, method = "glmnet",
                    tuneGrid = gridAlphaLambda,
                    trControl = trControlGrid )
# alpha 0.2 lambda 1 verdi

modelElas <- glmnet(trainData_x,trainData_y,alpha = 0.2,lambda = 1)
# dev degerim 0.88 baya iyi geldi

predictGrid <- predict(modelElas, testData_x)

R2(predictGrid, testData_y)
# 0.75 r2 degerim
RMSE(predictGrid, testData_y)
# 3.05
MAE(predictGrid, testData_y)
# 2.62


modelElas1 <- glmnet(trainData_x,trainData_y,alpha = 0.08,lambda = 1.22)

predictGrid1 <- predict(modelElas1, testData_x)

R2(predictGrid1, testData_y)
# 0.74 r2 degerim
RMSE(predictGrid1, testData_y)
# 3.10
MAE(predictGrid1, testData_y)
# 2.65

# uzun surdugunden gridAlphaLambda kismini seq fonksiyonuna 0.05 ve 0.1 aldik
# bu yuzden daha iyi bi alpha ve lambda degeri bulabilirdik ama hizli gecsin diye bulamadik
# bunu sebep gostererek yukarida rastgele ile buldugumuz yakin geldi diyebiliriz
# EKK ile yani lambda 0 alpha 1 iken yaptigimiz tahminlerin sonucu
# r2 0.32
# rmse 5.08
# mae 3.81
# daha kotu geldi yani en iyi alpha ve lambda degerlerini bulursak ekk kullanmadan 
# bu glmnet ile daha iyi tahminler yapabilriz. hava durumu projen icin onemli bi detayu
