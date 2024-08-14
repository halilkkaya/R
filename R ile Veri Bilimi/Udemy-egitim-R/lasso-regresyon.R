#### model olusturma ve degerlendirme ####

View(mtcars)
# lambda ile buyuk verilerde calismayi ridge regresyon modelinde denedik ve iyi sonuc alamadik
# o yuzden daha kucuk verilerde deneyecegiz

library(tidyverse)
library(glmnet)
library(caret)

# cyl, vs, am, gear, carb factor olacak


modelData <- mtcars %>% mutate(cyl=as.factor(cyl), vs = as.factor(vs), am=as.factor(am),
                  gear= as.factor(gear), carb = as.factor(carb))


class(modelData$cyl)
# factor baska bakmaya gerek yok



modelDataDummy <- model.matrix(~ . , data = modelData)
View(modelDataDummy)
# dummy degiskenleri olusturduk


set.seed(155)
smpIndex <- sample(1:nrow(modelDataDummy), size = 0.75*nrow(modelDataDummy))


trainData <- modelDataDummy[smpIndex,]
testData <- modelDataDummy[-smpIndex,]


trainData_x <- trainData[,-c(1,2)]
# intercep ve tahmin edecegimiz mpg degerlerini cikaralim
trainData_y <- trainData[,2]
# tahmin edecegimiz degeri y olarak aldik
# aynisini test datalarimiz icin de yapalim

testData_x <- testData[,-c(1,2)]
testData_y <- testData[,2]
# burda da ayni islemleri gerceklestirdik


lambdas <- 10^seq(2,-2, by = -0.01)

fitGL <- glmnet(trainData_x, trainData_y, alpha = 1, lambda = lambdas)
fitGL
# alpha 1 ise lasso yapiyo dikkat
plot(fitGL, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:nrow(trainData_x),
       legend = colnames(trainData_x))

# lambda degerlerimin logaritmik donusumlu halinin grafigini cizdik
# katsayilar ve nonzero degerlerim de var

# uygun lambda degerini bulma 

modelcv <- cv.glmnet(trainData_x, trainData_y, alpha = 1, lambda = lambdas)
# uyari verdi 10a bol dedik ya 3den az gozlem sayisi varmis bazi gozlemlerde
# 3 parcaya bolcez

modelcv <- cv.glmnet(trainData_x, trainData_y, alpha = 1, lambda = lambdas,nfolds = 3)
plot(modelcv)
# ortalama hata kareleri en dusuk olan deger en iyi sonucalri verir 
# seklinde yorumla
# ideal araligim degisken sayim 9 ile 6 arasinda vermis logaritmik donusumu yapilmis sekilde
# -2 ile 0 arasinda bi lambda degeri alicam 10^-2 ve 0 ile gibi


best_lambda <- modelcv$lambda.min
# 0.4265795 imis deger
# katsayilarin degeri yuksek yani anlamli bir etki birakabilir demek

 
## model olusturma ve degerlendirme 

modelLasso <- glmnet(trainData_x, trainData_y, alpha = 1,lambda = best_lambda)

modelLasso$beta
# beta degerlerimde bazilarinda . var. yani bunlar modelimizde 0 demektir
# mesela cyl8 var bu . almis bunun modelimizde etkisi dusuk/yok demektir

modelLasso
# df 7 yani nonzero yani 0 olmayan katsayilarim
# dev 0.87 verdi yani r2 gibi olan dev degerimiz cok iyi gelmis 


predLasso <- predict(modelLasso, testData_x)

library(caret)

R2(predLasso, testData_y)
# 0.79 iyi bir sonuc testte de iyi sonuclar verdi
RMSE(predLasso, testData_y)
# hatalar kareler ortalamasi 2.79 gayet iyi
MAE(predLasso, testData_y)
# hatalar toplami ortalamasi 2.41 o da iyi sayilir




modelLm <- glmnet(trainData_x, trainData_y, alpha = 1,lambda = 0)
# lambda sifir olursa eger bildigimiz dogrusal regresyon gibi ekk modeli gibi davranir
modelLm
# dev degerim 0.91 daha iyi sonuc gibi duruyor
modelLm$beta
# sadece carb8 alinmadi

predLm <- predict(modelLm, testData_x)

R2(predLm,testData_y)
# r2 degerim 0.32 baya dusuk geldi
RMSE(predLm, testData_y)
# hatalar kareler ortalmasi 5 geldi cok geldi resmen 2 kati lasso'ya gore
MAE(predLm, testData_y)
# 3.81 bu da fazla geldi

# bir de ben ridge denim bakim ne gelcek


modelRidge <- glmnet(trainData_x, trainData_y, alpha = 0,lambda = best_lambda)
modelRidge$beta
# etkisizler sifira ayakinlasti carb8 yine sifir
predRidge <- predict(modelRidge, testData_x)

R2(predRidge, testData_y)
# 0.69
RMSE(predRidge, testData_y)
# 3.37
MAE(predRidge, testData_y)
# 2.84


# hepsini kontrol edelim
# en kotu sonuclari lm modelimiz verdi
# en iyi sonuclari Lasso modelimiz verdi
# lasso ile ridge modellerimiz yakin sonuclar verdi gibi ancak veri setindeki degerlerimiz
# kucuk sayilardan olustugunda kucuk artislarin da onemi olabiliyor

# lasso ve ridge gozlem sayisinin az oldugu veri setlerinde daha iyi bir performans gosterdigi
# anlasilmistir bu uygulamada

