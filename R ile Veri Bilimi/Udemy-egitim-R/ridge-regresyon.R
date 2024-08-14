##### ridge regresyon veri on isleme- standartlastirma ######
library(caret)
install.packages("glmnet")
library(glmnet)
library(tidyverse)

# glmnet paketi onemli bu islem icin

# diger degiskenleri kullanarak borcu(loan_amount) tahmin etme modeli uzerinde calisacagiz

# term, income_category, purpose_cat,grade, interest_payments, lona_condition, emp_length_int
# annual_inc kullanacagiz
names(Bank_Loan_Data)

loan <- Bank_Loan_Data %>% 
  mutate(purpose_cat=as.factor(purpose_cat),income_category=as.factor(income_category),
         grade=as.factor(grade),interest_payments=as.factor(interest_payments),
         loan_condition=as.factor(loan_condition)) %>%
  select(loan_amount,term, income_category,purpose_cat,grade,interest_payments, 
                                  loan_condition, annual_inc, emp_length_int)

View(loan)
class(loan$purpose_cat)
class(loan$income_category)
class(loan$grade)
class(loan$interest_payments)
class(loan$loan_condition)
# bu sekilde faktor yaptik

# standartlastirma islemi yapacagiz. annual_inc ve emp_length_int degerlerim farkli parametreler kullanilarak
# hesaplanmis birbirinde sayisal olarak cok farkli degerlerimi bulunduran 2 degiskenim
# sebebi ise daha tutarli model olusturmak icin



num_cols <- c("annual_inc","emp_length_int")

pre_scaled <- preProcess(loan[,num_cols], method = c("center","scale"))

loanScaled <- predict(pre_scaled, loan)

View(loanScaled)
# standartlastirilmis verilerim burada. - degerlerim de var cunku standartlastirmada oluyor


class(loanScaled$purpose_cat)
# integer geldi. factor olmali. yukarida tidyverse isleminde factor yaptim hepsini orayi incelersin


#### veri on isleme 2- dummy degiskenler


# kategorik verilerimi binary haline getirme islemi

modeldatascaled <- model.matrix(loan_amount ~ . , data = loanScaled )

head(modeldatascaled)
# burdaki degerlendirmeleri dummy degiskenleri dersinde ogrenmistik hatirla
# 3 deger varsa k - 1 = 2. 2 tane o degisken gelcek
# degiskenlere 1 veya 0 verilcek ikisi de 0sa 3. olan degiskenim demekti bu unutma
# model.matrix() fonksiyonumb u islemi yapti allah razi olsun baya iyi fonksiyonmus 

set.seed(145)
sampleIndx <- sample(1:nrow(modeldatascaled), size = 0.8*nrow(modeldatascaled))


trainSet_x <- modeldatascaled[sampleIndx,]
testSet_x <- modeldatascaled[-sampleIndx,]

trainSet_y <- loanScaled$loan_amount[sampleIndx]
testSet_y <- loanScaled$loan_amount[-sampleIndx]

# tarin ve test verilerimizi x ve y olarak ayirmak zorunda kaldik cunku
# model matrix fonksiyonu bizim bagimli degiskenimizi icerisine almiyor
# onemli bi nokta bu bak


### ridge regrsyon model olsuturma ve degerlendirme

library(glmnet)

# ridge regresyon icin alpha 0 olmalidir
modelRidge1 <- glmnet(trainSet_x , trainSet_y, alpha = 0, lambda = 0.05)

summary(modelRidge1)
# bazi degiskenler ve degerler verdi yorumlamak icin dokumantasyona girelim
?glmnet

modelRidge1$a0
# intercept yani beta0 katsayim 20360
modelRidge1$beta
# simdi yorumlamaya baslayalim mesela
# term 60 months ise 6414 borcumda 6414 artisa sebep oluyor demek
# term 36 month yok burda. onu da hesaplarken -6414 yani 6414 azalisa sebep olacak
# income_category_low -7231 vermis borcum 7231 dusermis yani
# eger mdeium ise  -1165 duser demekmis
# high ise bunlarin tersinin toplami olacak


modelRidge1$lambda
# biz 0.05 belirlemistik zaten. beta katsayilarini carptigimiz deger. L1 ve L2 
# duzeltmelerinde kullandigimiz lambda degeridir bu

modelRidge1$dev.ratio
# 0.3418805 verdi R2 gib dusunebiliriz az gelmis baya
# 1-dev/nulldev seklinde hesaplaniyodu
# nulldev modelde sadece sabit degerimiz varkenki dev degerini gosterir
# dev ise parametreler eklendikten sonraki degeri verir
# eger ki aralarindaki fark fazlaysa yani dev az null dev fazla ise cikan sonuc 0a yaklascak
# sonra bunu 1den cikaricaz ve buyuk sayi gelcek.
# r2 gibi dusun ne kadar buyuk gelirse sonuc model o kadar iyi demek


# bu ders sadece ridge regresyon modeli nasil olusur onu gormek icindi lambda fln
# uygun degeri bulmak gerek


##### cross validation ile lambda degeri tayin etme #####

?cv.glmnet

lambdas= 10^seq(3, -2, by = -0.01)
lambdas
# lambda degerleri olusturduk denemek icin
modelridgecv <- cv.glmnet(trainSet_x, trainSet_y, 
                          alpha=0,lambda = lambdas, nfolds = 10)
# veri setimi 10 parcaya bolup degerlendiricek
# calismasi biraz uzun surebiliyor


modelridgecv$cvm
# yaklasik 500 sonucum var. 500 tane model kurulmus yani

plot(modelridgecv)
# kirmizi cizgi hata cizgim az yerlerde bisi olcak lambda demek

modelridgecv$lambda.min
# en iyi sonuc veren lambda sayisi 
# 0.1 geldi
# hocaya 0.01 verdi bana 0.1 herhalde rastgele sectigimiz sayilardan dolayi
# diye tahmin ediyorum
# set.seed degerimi hocayla ayni yaptim bende de 0.01 verdi
# yani dogru tahmin etmisim

modelridgecv$nzero
# 26 katsayim var ve hicbiri sifir degil onda 26 verdi
ncol(trainSet_x)
# 27 cikti bir de intercept var diye o da yoksa 26 yani

modelridgecv

### tahmin ve degerlendirme islemi

# 0.01 lambdayi alcaz

fitGl <- glmnet(trainSet_x, trainSet_y, alpha = 0, lambda = 0.01)

pred <- predict(fitGl, testSet_x)

pred

library(caret)

R2(pred, testSet_y)
# 0.33 dusuk geldi
MAE(pred,testSet_y)
# 5529 ortalama hata
RMSE(pred,testSet_y)
# 6982

# min max doguruluk testi yapak
# fonksiyon yok dormulle yapcaz
dfPred <- data.frame(predictions= pred, actuals=testSet_y)
minMaxAc <- mean(apply(dfPred,1,min )/ apply(dfPred,1,max)) 
minMaxAc
# modelin dogruluk orani 0.70 geldi
# mae ve rmse degerlerim yuksek gelme sebepleri sadece modelimizin kotu oldugundan degil
# borclar uzerinden calistigimizda 100k degerleri var ve bunlarin tahmininde
# 5k hata cikiyosa bu biraz da normal diyebiliriz

minMaxFun <- function(x,cr){
  if (cr==1) {
    mean(apply(x,1,min )/ apply(x,1,max)) 
  }else if (cr==2) {
    mean(apply(x,2,min )/ apply(x,2,max))
  }else {
    stop("cr can only take 1 or 2")
  }
}

minMaxFun(x=dfPred,cr=1)
# ayni sonucu aldik bi fonksiyon olusturduk bunu fonksiyon klasotuna kaydedim kullaniriz
# cr = col or row seklinde 1 row 2 col 

