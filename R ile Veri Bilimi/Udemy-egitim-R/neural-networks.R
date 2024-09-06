install.packages("neuralnet")
library(neuralnet)

## arguman incelemeleri

?neuralnet
# buralari inceliyoruz
# formul her zamanki gibi ayni olcak
# data yine data vericez
# hidden, hidden layer icerisindeki noron sayisi oluyor ancak soyle yaparsak
# c(3,3,3,6) 4 tane hidden layer oluyor ve ilkinde 3 sonuncuda 6 noron olcak
# sekilde hidden layer olustururuz
# threshold ogrenme islemi ne zaman duracak demek. dusuk olmasi iyi ama uzun surer
# stepmax max iterasyon sayisini gosterir
# rep tekrarlama sayisi. ilk iterasyon bitti diyelim bi daha yapcaz mi
# startweights baslangictaki agirlik degerlerimizi soruyor
# null ile basliyo otomatik olarak
# learningrate.limit buna dokunmuomusuz genelde
# sadece learningrate uzerinden islem yapicaz biz
# limit ve factoru ellemicez
# algorithm ve err.fct en onemlileri err.fct lost fonkiyonu olarak geciyo
# bir de act.fct var aktivasyon fonksiyonu bu da
# algorithm dokumanda neler yazilir yaziyor
# linear.output sonuc surekli verilerse kullanilir ve bu kullaniliyosa
# act.fct kullanilmaz cunku bu siniflandirma icin kullaniliyor

#### baslayalim

## diabetes veri setini kullanacagiz


df <- diabetes
df$Outcome <- as.factor(df$Outcome)
class(df$Outcome)
# factor haline getirdik


# scaling islemini yaomamiz gerek
# ya min max normalizasyon ya da standartlastirma yapcaz

library(caret)

# standartlastirma islemi yapiyoruz
modelScale <- preProcess(df,method = c("center","scale"))


dfScaled <- predict(modelScale, df)
View(dfScaled)
# t??m degerlerim standartlastirilmis hale geldi
# modelScale ile bi standartlastirma modeli olusturduk 
# sonra predict ile olusturulan modeldeki sekle gore tahmin islemiyle 
# verilerimizi standartlastirdil
# artik hepsi  ayni olcekte, factorler haric



set.seed(165)
index <- sample(1:nrow(dfScaled),size = nrow(dfScaled)*0.75)

train <- dfScaled[index,]
test <- dfScaled[-index,]



nrow(train)
nrow(test)

table(train$Outcome)
table(test$Outcome)

# test ve train islemi yapip verilerimizi kontrol ettik
# neural networks icin az bi ama deneyecegiz bakalim
# daha sonra baska veri setlerinde deneriz




##### model olusturma/egitme #####

modelNN_1 <- neuralnet(Outcome ~ ., data = train,
                       hidden = 1, threshold = 0.01,act.fct = "logistic",
                       linear.output =F  )


plot(modelNN_1)

modelNN_1
# sonucalri degerlendiriyoruz
# video 316



modelNN_2 <- neuralnet(Outcome ~ ., data = train,
                       hidden = c(2,2), threshold = 0.01,act.fct = "logistic",
                       linear.output =F  )

plot(modelNN_2)


modelNN_3 <- neuralnet(Outcome ~ ., data = train,
                       hidden = c(4,4), threshold = 0.01,act.fct = "logistic",
                       linear.output =F  )
# Uyar?? mesajlar??:
# Algorithm did not converge in 1 of 1 repetition(s) within the stepmax. 
# hatasi aldik bu istenilen sonuca erisemedik demek
# degerlerimizi dusurmemiz lazim

modelNN_3 <- neuralnet(Outcome ~ ., data = train,
                       hidden = c(3,3), threshold = 0.01,act.fct = "logistic",
                       linear.output =F  )
# hata vermedi bana ama hocaya hata verdi sectigimiz degerlerden dolayi sanirim

plot(modelNN_3)

# modellerimizi olusturduk simdi performanslarina bakalim


#### tahmin islemi ####

tahmin1 <- predict(modelNN_1,test)
tahmin1
# sadece olasiliklar gelmis. burdan yuksek olani secmemiz gerek

apply(tahmin1,1,which.max)
# wonuclar 1 ve 2 olarak geldi bu da sutun isimleri oluyor
# bizde de 1. sutun 0 2. sutun 1 demek oluyordu


tahmin1c <- ifelse(apply(tahmin1,1,which.max) == 1,"0","1")


# model2
tahmin2 <- predict(modelNN_2,test)
tahmin2
apply(tahmin2,1,which.max)

tahmin2c <- ifelse(apply(tahmin2,1,which.max) == 1,"0","1")


# model3
tahmin3 <- predict(modelNN_3,test)
tahmin3
apply(tahmin3,1,which.max)

tahmin3c <- ifelse(apply(tahmin3,1,which.max) == 1,"0","1")


library(caret)

confusionMatrix(as.factor(tahmin1c),test$Outcome)
# ilk modelimizde dogruluk oranim 0.77
# p degerim de dusuk iyi
# sonuc ortalama 


confusionMatrix(as.factor(tahmin2c),test$Outcome)
# 2. modelimin dogrulugu 0.75
# p degerim yyine dusuk iyi
# 0'i tahmin etme oranim baya artmis guzell
# 1'i tahmin etme oranim baya dusmus kotuuu
# ilk modelden kotu sonuc elde ettim, genel olarak ortaslama


confusionMatrix(as.factor(tahmin3c),test$Outcome)
# dogruluk oranim 0.78
# 0'i tahmin etme ilk modele gore sadece 1 veri azalmis sorun yok
# 1'i tahmin etmede en iyi performansi vermis
# genel oalrak en iyi duran modelim bu dogruluk da 0.78 diyor


# diger modelime gore yani decision tree modelime gore ayni gibi
# hatta bu daha iyi performans verdi. ama hala daha da iyi hale getirilebilinir bu model

modelNN_4 <- neuralnet(Outcome ~ ., data = train,
                       hidden = c(2,3), threshold = 0.01,act.fct = "logistic",
                       linear.output =F,learningrate = 0.01  )
plot(modelNN_4)
tahmin4 <- predict(modelNN_4,test)
tahmin4c <- ifelse(apply(tahmin4,1,which.max) == 1,"0","1")

confusionMatrix(as.factor(tahmin4c),test$Outcome)
# 1 veriyi daha dogru tahmin etmisiz yani daha da iyilestirilebiliyoruz
# gordugumuz gibi


##### surekli verileri tahmin etme #####

library(neuralnet)
library(caret)
library(mice)


concrete <- Concrete_Data
View(concrete)
# dayanikliligi tahmin eden bir model olusturalim

nrow(concrete)
# scaling islemi yapalim yine, cunku veriler ayni olcekte olmasi gerek

md.pattern(concrete)
# NA degerim yok

scaleModel <- preProcess(concrete, method = c("center","scale"))
# modelizi olusturduk

df <- predict(scaleModel, concrete)
View(df)
# verilerimiz ayni birimlere yaklasti artik


# train test split
set.seed(165)
index <- sample(1:nrow(df), size = nrow(df)*0.75)

trainSet <- df[index,]
testSet <- df[-index,]
nrow(trainSet)
nrow(testSet)


modelNN_1 <- neuralnet(strength ~ ., data = trainSet,linear.output = T,
                       hidden = 4,threshold = 0.01,
                       learningrate = 0.05)
plot(modelNN_1)


modelNN_2 <- neuralnet(strength ~ ., data = trainSet,linear.output = T,
                       hidden = c(3,2),threshold = 0.01,
                       learningrate = 0.05)

plot(modelNN_2)

fittedValues1 <- modelNN_1$net.result[[1]]
# fitted degerlerimiz

library(caret)

R2(fittedValues1,trainSet$strength)
# 0.88 cikti. oldukca iyi bi skor
RMSE(fittedValues1,trainSet$strength)
# 0.33
MAE(fittedValues1,trainSet$strength)
# 0.25

# min ve max degerlerim -2 ile 2.74 arasinda yani bu aradfa 0.25 hata gayet iyi sayilir
# model 2ye bakalim bir de

fittedValues2 <- modelNN_2$net.result[[1]]

R2(fittedValues2,trainSet$strength)
# 0.86 cikti. oldukca iyi bi skor. model 1den ufacik daha az ama
RMSE(fittedValues2,trainSet$strength)
# 0.36
MAE(fittedValues2,trainSet$strength)
# 0.28 genel skoarlar model1de daha i??yi cikti ama bu da iyi sayilir 
# bir de test setimizle yapalim


#### modeller uzeerinde test verisetiyle tahmin


tahmin1 <- predict(modelNN_1,testSet)
tahmin2 <- predict(modelNN_2,testSet)

R2(tahmin1,testSet$strength)
# 0.88
RMSE(tahmin1,testSet$strength)
# 0.33
MAE(tahmin1,testSet$strength)
# 0.26

# ilk modelimin sonuclari gayet iyi


R2(tahmin2,testSet$strength)
# 0.84
RMSE(tahmin2,testSet$strength)
# 0.39
MAE(tahmin2,testSet$strength)
# 0.31

# ilk modelim daha iyi ancak sonuclar yakin sayabiliriz


# scale islemini reverse etmem,z gerekiyor cunku direkt normal yapsak kotu sonuc alcaktik
# simdi tersine cevirip bakalim

# x - mean(x) / sd
scaleModel$std
scaleModel$mean


reverseScale <- function(x, model) {
  reverse <- (x*model[["std"]]["strength"]) + model[["mean"]]["strength"]
  return(reverse)
}

reverseScale(tahmin1[,1],scaleModel)
# bus ekild cikti verilerimiz
R2(reverseScale(tahmin1[,1],scaleModel),reverseScale(testSet$strength,scaleModel))
# gercek degerlerimde karsilastirdigimda 0.88 geldi R2 degerim
# sonuc ayni yani



reverseScale(df$strength,scaleModel)
# kontrol amacli dfdekileri reverse edip ana veri setimden kontrol ettim
# sonuclar ayni cikti yani fonksiyonu dogru calisiyor


#### disaridan gelen verilerin tahmini ####

# 189. satiri disaridan geldigini varsayalim
new_data <- concrete[189,]

# oncelikle scaleModel uzerinde tekrar scale etmemiz gerek bu verileri 
# ana veri seti uzerindeki sd ve mean degerlere gore ettik
# tahmin edilecek degerimiz strength oldugunda onu da almayalim
# cunku tahmin etcez zaten
pred_data <- predict(scaleModel , new_data)[,-9]


tahmin_sonrakiVeri <- predict(modelNN_1 , pred_data)
# 0.01 verdi hocaya da 0.05 verdi allah allah bakalim sonuclar nasil olcak

reverseScale(tahmin_sonrakiVeri, scaleModel)
# 36.04 dedi gercek degerimize bakalim

concrete[189,]$strength
# tahminimiz 36.04 idi
# 40 imis yanlis tahmin ettik 
# hocada da 36.72 cikti hemen hemen ayniymis sorun  yok












