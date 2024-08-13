##### regresyon modeli olusturma####

View(airquality)

# temp degerine gore ozone degerini tahmin etmeye calisan bir regresyon kuralim

model <- lm(Ozone~Temp, data = airquality)
model

summary(model)
# H0: parametrelerin anlamli bir etkisi yoktur
# Ha: parametrelerin anlamli bir etkisi vardir

# residual(artik) degerleri gercek deger ile tahmin edilen degerler arasindaki fark
# residullar ne kadar kucukse yani 0a ne kadar yakinsa o kadar iyi tahmindir

# coefficients yani katsayilarim da var
# Intercept degiskenlerim yani formuldaki beta1  oluyo
# std eror o degiskenin icerisindeki +- uzakligi ne olabilir dio
# t degeri t tablosundaki istatistik degeri
# ve p degeri de var p degeri 0.05den kucukse intercept anlamli bi etki gosteriyor
# temp uzerinde baktigimizda p degeri 0.05den kucuk olmasi 
# temp degerinin ozone ustunde anlamli bir etkisi var demektir
# estimate degerine bakinca da temp deger 1 arttiginda ozone degeri 2.42 artarmisd diyo
# temp formuldeki beta2 oluyo 

# modelin p degeri 0.05den kucukse benim modelim 0.95 guven duzeyinde anlamli modeldir
# ama p degeri 0.05den kucukse bizim model iyi calisiyor demek degildir dikkat 
# iyi tahminler yapip yapmadigini modeli test ederken karar vercez burdaki p degerinin onemi
# temp degerleriyle ozone degerlerini tahmin edebilirsin demesi. anlamli bi iliski var
# demek istiyor bize 



# multiple ve adjusted r squared da r2 degerleri oluyor aralarindaki farki dosyaya yaacagim



## artiklarin degerlendirilmesi

# artik degerler 0a yakinsa model o kadar dogrudur denir

model$residuals
# tablo gibi verildi
# 1. degere bakalim mesela bu neden? airquality tablodaki 1. ozone degerine bakiyoruz.
# temp degerine gore tahmin modelinde gercek deger - tahmin edilen deger 
# yani 41 - tahmin edilen deger= 25 verilmis 16 tahmin edilmis yani


artik <- as.numeric(model$residuals)
# vektor olarak aldik
plot(artik)

# x= index, y= artik degerim
# regresyon analizinde artik degerlerin dagilisi cok onemlidir. 
# dagilisa gore model gecerli mi degilk mi soylenebilir
# buna da degisen varyanslilik denir. ve biz regresyonumuzda degisen varyanslilik istemeyiz onemmmllliiiii
# huni tarzi bi dagilim buna ornektir ve biz degisen varyanslilik istemeyiz
# dogru bi model icin artik degerlerimiz rastgekle ya da toplu bi dagilim gostemrelidir



plot(model)
# entera bastikca tablolari acicak ilk tablomus artik ve tahmini degerlerin karsilastirilmasi
# baz?? degerlerde indis sayilari yaziyor. bunun anlami onlar cok uzakta dikkat et
# aralarinda cok fark vardi demek

# entera bastik ve sonraki tablo qq tablosu normallik olcen tablo geldi
# regresyon varsayimlarindan birisi de normal dagilim gostermesidir bunu o yuzden kontrol ediyoruz
# x ekseni standart normal dagilimdan gelen quantillar y kismi da standartlastirilmis artik degerlerimizdir
# ortada bi noktali line var o noktalar onun ustundeyse normal dagilimdir
# yine indis yazanlar normalligi bozuyor dikkat


# entera bastik yeni plotumuz scale location grafigimiz geldi
# ilk grafigin artiklarinin standartlastirilmis halidir. standartlastirmanin amaci degerleri
# daha tutarli degerlendirmek icindir


# entera bastik residuals vs leverage (baski) grafigi geldi
# yine indislerimin yazdigi asiri degerlerim oldu
# cook distance kirmizi cizgili yerim oraya yakin olan yerler outlierlarim oluyo
# cizgi cizgi olan yerden bahsediyorum



#####aykiri deger kontrolu#####

# mhalanobis distance kullancaz 

plot(airquality$Ozone, airquality$Temp)
# lineer bi iliski var oldugu goruluyor.

summary(model)

air <- na.omit(airquality[c("Ozone","Temp")])
View(air)

air.center <- colMeans(air)
air.center
# merkez noktalari belirledik

air.cov <- cov(air)
air.cov
# kovaryanslari veriyo ozone-temp arasina bak sadece




distance <- mahalanobis(air, center = air.center,cov = air.cov)
distance
# mahalanobis distancelarindan olusan bi vektorumu aldim


cutof <- qchisq(p = 0.95, df = 2)
cutof
# cutoff noktamiz

# simdi yapmamiz gereken sey bu distance yani mesafelerden hangileri cutofflardan daha buyuk
# onu bulmak 

index <- which(distance > cutof)
# bu degerlerim buyukmus enter ile gectigimiz tablolardaki indis degerlerimdi bunlar
# simdi bu degerleri tablomuzdan cikaralim

air[index,]
# outlier degerlerim lineer iliskimi bozan degerlerdir

air_new <- air[-index,]
air_new
# yeni tablom


model2 <- lm(Ozone~Temp, data=air_new)


model2
# intercept degerim dusmus biraz digerine gore berabe kontrol edelim

model
model2
# intercept dusmus Temp degerim dusmus yani her 1 ozone degerine temp degisim miktari dusmu
# daha dogrusallasmis giib duruyor 

summary(model)
summary(model2)
# residuallerim sifira biraz daha yaklasmis ama hala uzaklar
# min -40idi -37 olmus dusmus 
# max degerimiz 118idi 52ye dusmus baya dusmus
# r kare degerlerim (r squared) 0.59a yukselmis artik modelimiz diger modele gore daha iyi
# tahmin yapabiliyor. bunu nasil yaptik? aykiri degerleri verimizden atarak yaptik. onemli nokta
# p degerimiz 0a yakin cikmis bu da veriler arasinda bi anlam var demektir

plot(model2)
# ilk grafigimiz yukarida da hatirladigimiz uzere residuals vs fitted
# burda yin 3 tane deger verdi bunlar aykiri olabilir dikkat et uzak diye
# biraz daha karmasik dagiliyor istedigimiz de buydu


# entera bastik qq grafigi inceliyoruz nomrallik hakkinda bilgi veren grafik
# diger ornege gopre biraz daha normal dagilim gibi duruyor 
# yine o 3 noktayi verdi


# yine entera bastik standartlastirilmis artik degerlerimi aldim
# yine o 3 noktayi verdi istersek bunlari da cikarabilriiz

# cooks distamce grafigine bakiyortuz burda bundan onceki gibi ustte
# kirmizi cizgi czigi olan cizgi yok dikkat ama yine de 3 taen aykiri bermis
# x kismi etkiden bahsediyor en etkili olan sondakiler


# burda ders bitti ben yeni bir sey denicem o diger 3luyu de atmayi denicem



air.center1 <- colMeans(air_new1)
air.center1
# merkez noktalari belirledik

air.cov1 <- cov(air_new1)
air.cov1
# kovaryanslari veriyo ozone-temp arasina bak sadece




distance1 <- mahalanobis(air_new1,center = air.center1,cov = air.cov1)
distance1

# mahalanobis distancelarindan olusan bi vektorumu aldim


cutof <- qchisq(p = 0.95, df = 2)
cutof


which(distance1>cutof)
# aykiri degerlerim bunlar da 

inds <- which(distance1>cutof)

air_new1[inds,]

air_new1 <- air_new1[-inds,]


model3 <- lm(Ozone~Temp, data = air_new1)

model
model2
model3
# intercept ve temp degerlerindeki dususu goruyroz


summary(model3)
# min degerim -35 max degerim 29 olmus 
# r kare degerlerim 0.59dan 0.62 olmus
# p degerm 0.05den dusuk yani modelde degiskenler birbiriyle iliskili



plot(model3)

# kendimiz yorumlamaya calisalim
# yine bize indis numaralari verdi bunlar da aykiri deger diye
# artik degerlerimin dagilimini inceliyorum huni gibi bi sekilde degil de rastgele bi dagilim var
# yani degisken varyanslilik degil bu modelimiz icin uygun dagilim


# ikinci tablomuz qq grafigi burda da normallik kontrol ediyorduk
# ortadaki cizginin ustundeyse normal dagilan veriler var diyoduk yine aykiri degerlerimiz var
# normallik olmazsa olmazdir

# ucuncu tablomuz artik degerlerin standartlastirilmis hali degerleri degerlendirmek icin daha tutarli gosterir


# son tablomuz baski ve standartlastirilmis artik degerler tablosu cooks distance degerlerimi gosteren bi cizgi yok yine ama 
# aykiri sayilcak degerleri gosteriyor yine






air.center2 <- colMeans(air_new1)
air.center2
# yeni ortalamalar


air.cov2 <- cov(air_new1)
air.cov2
# yeni kovaryans degerleri


cutoff <- qchisq(p=0.95,df = 2)
cutoff

distance2 <- mahalanobis(air_new1,center = air.center2,cov = air.cov2)
distance2

ind <- which(distance2>cutoff)

air_new2 <- air_new1[-ind,]


model4 <- lm(Ozone~Temp, air_new2)

model3
model4
# laaaaa degerlerim artti nalaka acaba


summary(model3)
summary(model4)
# arada cok da bi fark yok valla

plot(model4)
# karisik bi dagilim
# dagilimin normalligini gosteren qq grafigi
# artik degerlerimin normalize edilmis halinin normal degerlerle karsilastirilmis hali
# standartlastirilmis ayrik degerlerim ve baski grafigim

# bu kadar ornek yetervideoalra devam edelim



#### donusumler ####

# ya degerlerimiz normal dagilim gosterecek ya da residual degerler normal dagilim gostercek

par(mfrow=c(2,2))
plot(model2)
# enter basmadan hepsini verdi
# qq incelicez normallige bakmak icin
# 

shapiro.test(model2$residuals)
# normal dagilim gosteriyor p 0.1647


model_log <- lm(Ozone~log(Temp),data = air_new)
summary(model_log)
# residuals degerlerimde pek degisim olmadi sebebi log degisimini temp'e yaptik sadece
# r kare degerlerimiz dusmus
# temp degerlerimiz artmis cunku log kullaniyoruz

dev.off()

plot(model_log)
# cok bi degisim olmamis hala ayni gibi
# elimizde normal dagilmayan verilerimiz olsaydi onu daha cok normallige getirirdi


model_sq <- lm(Ozone~sqrt(Temp),data = air_new)
summary(model_sq)
# sonuclar log ile ayni gibi asagi yukari r kare sonucu artti biraz

plot(model_sq)
# ayni dagilim veriyo gibi duruyor



## ozone icin de yapalim

model_sqrt <- lm(sqrt(Ozone)~sqrt(Temp),data = air_new)

summary(model_sqrt)
# residual degerlerim dustu sebebi karekok almamiz degisime aldanma 
# cunku tum verilerimizin karekoku alindi. yine bi azalma var ama
# r kare degerim artti


plot(model_sqrt)
# qq biraz daha degisim olmus gibi asagi yukari normal duruyor
# standartlastirilmis artik degerler tamamen rastgele dagilmis duruyor
# kirmizi cizgi var ortada o duzgunlesmis bu iyi bi sey daha rastgele oldugunu soyluyor
# karekok donusumu daha iyi sonuc vermis gibi duruyorf


##### model uzerinden tahmin islemi #####

# ustlerde kullandigim aykiri modellerden arindirdigim model2' yi kullancagim
predict(model2, data.frame(Temp = c(74)))
# modelde temp degerim 74 ise ozone degerim ne olur seklinde bi soru sorduk
# temp 74 ise ozone 30.44 verir dedi


model_log <- lm(log(Ozone)~Temp, data = airquality)
summary(model_log)
# log model bagimli degisken uzerinde
predict(model_log, data.frame(Temp=c(74)))
# 3.15 verdi log modelim uzerinden. donusum yaptigimiz icin dusuk verdi
# ters bi donusum yapmamiz gerekiyor
pre <- predict(model_log, data.frame(Temp=c(74)))

exp(pre)
# daha kabul edilebilir bi sonuc verdi 23.50 verdi
# ters log islemidir bu



# log model bagimsiz degisken uzerinde 
log_model <- lm(Ozone~log(Temp), data = airquality)

summary(log_model)

predict(log_model, data.frame(Temp=c(74)))
# 34.37 verdi

# bagimsiz degisken icin ters donusum uygulanmaz
predict(log_model, data.frame(Temp=c(exp(74))))
# cokkkk yuksekverdi sonucu. hatali kullanim

# sunu ogrendik eger bagimli degisken uzerinde bi donusum yapiyorsak
# ters donusum kullanmaliyiz
# ama eger bagimsiz degisken uzerinde donusum yaptiysak ters donusume gerek yok

predict(log_model, data.frame(Temp=c(74,56,45,78)))
 # 4 tane ozone degeri tahminini verdi

# ders burda bitti baska bisi denicem

# hep temp degeriyle ozone tahmin ediyoduk ozon ile temp tahmin edelim




a <- na.omit(airquality[c("Ozone","Temp")])
modell <- lm(Temp~Ozone, a)
summary(modell)
# birbirini etkiliyor cikti p degerim dusuk baya
plot(modell)

aircenter <- colMeans(a)

aircov <- cov(a)

cut <- qchisq(p=0.95,df=2)
mesafe <- mahalanobis(a,center = aircenter,cov = aircov)


i <- which(cut<mesafe)
# aykiri degerler bunlari atalim


an <- a[-i,]
# aykiri degerleri attik


model_yeni <- lm(Temp~Ozone, data=an)
plot(model_yeni)
predict(model_yeni, data.frame(Ozone=c(40)))
# ozone 40 iken temp 77.9915 verir diyo
# guzel bi alistirma oldu hem aykiri degerleri de attik hem de tahmin yaptik



##### regresyon metrikleri####
#### mse nedir nasil hesaplanir
# mean squared error ortalam standart hata
# r kare hesaplarken kullaniyoduk bunu nasil hesaplariz??

model <- lm(Ozone~Temp, airquality, na.action = na.omit)
model

# standart hata hesaplama
# butun standart hatalarin karesini al topla ve
# ne kadar deger varsa ona bol
mse <- sum(model$residuals^2)/length(model$residuals)
mse

# ortalama standart hesaplama oldukca onemlidir 
# nasil hesaplandigini bilmeyi bill


#### AIC ve BIC kriterleri nedir nasil yorumlanir 

# Birden fazla model kurdugumuzda bu modelleri karsilastirmak icin kullaniriz
# r kare de bu ise yariyo ama tek basina yetmiyo bu kriterler de onemli
# model karsilastirmalarinda hangisinin kriter degeri daha dusukse onu kullanmaliyiz
# yani 2 tanesini karsilastirdik birinin AIC degeri 5 digerinin + geldi biz 4 olan modeli secmeliyiz


# AIC = n * log(RSS/n) + 2 * num_params

# AIC = -2 * (log-likelihood) + k * N_par # R bu fonksiyonu kullanir

# AIC 2k + n * (log(2 * pi * RSS/n)+1) # R ile ayni sonucu verir



# BIC = -2 * (log-likelihood) + k * log(n) # R BIC fonksiyonu bu formulunkullaniyor

# BIC = n * log(RSS/n) + k * log(n)

# k = mdoeldeki parametre sayisi Bo dahil(genelde 2'dir Beta0 ve Beta1)
# ayrica AIC formulunde varyans ayri bir parametre olarak sayilmaktadir
# bu nedenle toplam parametre sayisi = k + 1; bir diger parametre varyans olarak alinmalidir



model <- lm(Ozone~Temp, data = airquality, na.action = na.omit)

 
loglik <- logLik(model)
loglik <- as.numeric(loglik)
loglik
# - 530 cikti
# yukarida yazdigimiz formulu kullanalim
# bu formulu: -2 * (log-likelihood) + k * N_par

k= 3
N_par = 3

AIC <- -2 * (loglik) + k * N_par
AIC
# 1070.706 verdi

AIC(model, k =3)
# ayni sonucu verdi 1070.706 


# BIC ile bakalim
# formulu: -2 * (log-likelihood) + k * log(n)

say <- log(length(model$residuals))

-2 * (loglik) + k * say
# 1075.967

BIC(model)
# 1075.967 aynisi verdi

# BIC degerimiz genelde biraz daha yuksek cikabilir

air <- airquality[c("Ozone","Temp")]
air <- na.omit(air)

air.center <- colMeans(air)
air.cov <- cov(air)

mesafe <- mahalanobis(air, center = air.center,cov = air.cov)

cutoff <- qchisq(p=0.95, df = 2)

index <- which(cutoff<mesafe)

air <- air[-index,]
# verilerden aykirilari cikarttik 

model2 <- lm(Ozone~Temp, data = air)

AIC(model2, k = 3)
# 970e dustu aykirilari atinca

BIC(model2)
# 975 verdi. BIC genelde daha yuksek veriyor diyoduk




##### basit dogrusal regresyon verseti egitimi ve test ayrimi #####

### train ve test ayirma 

# bi kismi modeli olustururken (train)
# diger kismi test ederken kullaniriz



model_data <- kc_house_data[c("price","sqft_living")]

# salonun buyuklugu ile ev fiyatini tahmin etme uzerine calisicaz bu veri setinde
# sadece o ikiliyi aldik

# random sample

set.seed(145)
sampleindx <- sample(1:nrow(model_data), size = 0.8*nrow(model_data))

trainSet <- model_data[sampleindx,]
testSet <- model_data[-sampleindx,]

# %80 modeli egitmek icin
# %20 modeli test etmek icin

nrow(trainSet)
nrow(testSet)
# veri sayilarimiza baktik



### train icin aykiri deger kontolu yapacagiz

anyNA(trainSet)
# NA degerim yokmus iyi direkt aykiri degere geciyorum

cor(trainSet)
# iliski 0.70 yani pozitif iliski var. sqft arttikca price artiyo diyebiliriz

hist(trainSet$price)
hist(trainSet$sqft_living)
# cok fazla aykiri deger var aykirilar gidince hafif normallik gosterebilir

model <- lm(price~sqft_living, data = trainSet)
summary(model)
plot(model)
# koni sekli mevcut olmaz degisken varyanslilikti bu
# normallikten kopan cok aykiri degerim var
# cook distance icerisinde degerlerim var aykiri olanlar
# bunlari atalim

dist <- mahalanobis(trainSet,center = colMeans(trainSet), cov = cov(trainSet))

cutoff <-  qchisq(p = 0.95, df=2)
# 2 degisken, 0.95 olasilikli.

which(dist>cutoff)
# bu degerlerim aykiriymis

index <- which(dist>cutoff)
trainSet1 <- trainSet[-index,]
# yaklasik 1k deger gitti

# buna tektat bi kontrol yapalim bakalim
model2 <- lm(price~sqft_living, trainSet1)
summary(model2)
plot(model2)
# daha rastgele bi dagilim mevcut
# qq grafigim normale daha yakin


## hocanin yaptirdigi

library(ggplot2)

fig <-  ggplot(data= trainSet, aes(x= sqft_living ,y=price))+
        geom_point(size= 2)+
        xlab("salon buyuklugu")+ ylab("fiyatlar")
fig

# grafige bakalim pozitif yondeki iliskiyi gorebiliyoruz
# aykiri degerlerim var gibi duruyor
# aykiri ama yine de anlamli bi degere sahip aykiri degerlerimdir
# cikartmak iyi olabilir

# ben mahalanobis ile yapmistim hoca outliers ile yapiyo
library(outliers)

scr <- scores(trainSet,type = "z",prob = 0.95)
scr
# true falselardan olusan bisi veri
# aykiri degerler true oluyor
# data.frame olarak veriyor

# TRUE olan satirlari verecek bana
# vector olarak
anyTrue <- apply(scr, 1, FUN = function(x){
  any(x==TRUE)
})

anyTrue

index <- which(anyTrue)

trainSetRemoved <- trainSet[-index,]
# yaklasik 1.5k veri gitti ben yaptigimda 1k gitmisti





fig1 <- ggplot(data = trainSetRemoved,aes(x=sqft_living,y=price))+
        geom_point(size=2)+
  ylab("Fiyatlar")+xlab("Salon Buyuklugu")
fig1

# aykiri degerler gitmis sekilde daha iyi bir dagilim var

# benim yaptigim ustteki mahalanobis ile onun figurunu inceleyelim

fig2 <- ggplot(data = trainSet1,aes(x=sqft_living,y=price))+
  geom_point(size=2)+
  xlab("Salon")+
  ylab("Fiyat")
fig2
# bu da beni yaptigim bunda daha cok veri duruyor
# oylesine baktim


cor(trainSetRemoved)
# corelasyon azalmis 0.7ydi 0.57 oldu
# bazen onemli bazen onemsiz olabilir bunu ileride tekrtar denicez
# iki model yapariz aykiri degerlerle beraber ve onlar olmadan
# ona gore karsilastiririz ve dogru modele karar veririz

# bir de benim yaptigima bakayim
cor(trainSet1)
# bunda da dusmus ama 0.7den 0.6e dusmus bunda aralarindaki bag daha pozitif



# kayip gozlem kontrolu bir de bu var
library("mice")

md.pattern(trainSet)
# kayip gozlem yok diyor. kayip gozlem yoktu zaten bu da onemli
# bu islemlerin de kullanilacagi yerler olur dikkat et

# oncelik aykiri degerleri at sonra kayip gozlem doldurma islemini yap
# sebebi ise su doldurma islemi icin model olusturuyoruz biz
# model olusurken cok fazla aykiri deger varsa kotu sonuclarla doldururuz
# o yuzden once at sonra doldur islemini unutma


##### model olusturma ve degerlendirme ####

# aykiri degeri cikarttigimiz, cikartmadigimiz ve video disinda benim yaptigim verisetiyle
# model olusturup karsilastiracagiz

# cikartilmadigi
model1 <- lm(price~sqft_living, data = trainSet)
#cikarttigimiz
model2 <- lm(price~sqft_living, data = trainSetRemoved)
# benim cikarttigim mahalonobis ile
model3 <- lm(price~sqft_living, data = trainSet1)



summary(model1)
summary(model2)
summary(model3)

# r kare degerlerimiz cikarttigimiz ve benim yaptigimda daha dusuk cikti
# sebebi ise deger cikarttikca aradaki iliski bagi azalmisti cor testi ile bakmisti
# sebebi o

# residual degerlerim de dusmus ama hala cok
# hocanin yaptiginda daha dusuk seviyede ama verilerim zaten cok buyuk oldugundan
# sayilar buyuk geldi
# bu souc model dogru demek degildir ama

AIC(model1)
BIC(model1)
# 480329

AIC(model2)
BIC(model2)
# 423316

AIC(model3)
BIC(model3)
# 441598


# hangi skor daha dusukse onu model olarak kullaniriz
# ama verilerin degeri dusuk diye de bu sonuc cikmis olabilir
# peki r kare degerlerim neden bu kadar dusuk 
# sadece bi degisken salonun genisligine gore fiyata baktigimiz icin dusuk geldi
# diger degiskenleri de eklersen bu r kare degerim artis gosterebilir



##### test veri seti uzerinden degerlendirmeler #####

# model1 aykiri degerler duruyor
# model2 hocanin yaptigi aykiri degerleri atma islemi
# model3 benim yaptigim aykiri degerleri atma islemi

# bunlar bi onceki dersten gelen degiskenler


model1pred <- predict(model1, testSet)
model2pred <- predict(model2, testSet)
model3pred <- predict(model3, testSet)

# tahmin ettigi degerle gercek degeri karsilastirma islemi yapiyoruz burda

model1preddata <- data.frame("actuals"= testSet$price, "predictions"=model1pred)
model2preddata <- data.frame("actuals"= testSet$price, "predictions"=model2pred)
model3preddata <- data.frame("actuals"= testSet$price, "predictions"=model3pred)
View(model1preddata)
View(model2preddata)
View(model3preddata)


model1hata <- model1preddata$actuals - model1preddata$predictions
model2hata <- model2preddata$actuals - model2preddata$predictions
model3hata <- model3preddata$actuals - model3preddata$predictions
  


mse1 <- sum(model1hata**2)/ nrow(model1preddata) 
mse2 <- sum(model2hata**2)/ nrow(model2preddata)
mse3 <- sum(model3hata**2)/ nrow(model3preddata)


sqrt(mse1)
sqrt(mse2)
sqrt(mse3)

# aykiri degerlerimin oldugu model en iyi tahminleri yapmis
# 2. benim yaptigim
# 3. hocanin yaptigi

# yani ne kadar cok aykiri deger cikarttiysak (ben 1k hoca 1.5k) o kadar
# tahmin yanlisliligi olmus gibi duruyor
# baska metriklere de bakip karar vericez ve yorumlicaz 


##### r2, mse ve mae #####

# karsilastirma metrikleri

install.packages("caret")
library(caret)

R2(model1preddata$predictions,model1preddata$actuals)
R2(model2preddata$predictions,model2preddata$actuals)
R2(model3preddata$predictions,model3preddata$actuals)

# degerler ayni cikti

RMSE(model1preddata$predictions,model1preddata$actuals)
RMSE(model2preddata$predictions,model2preddata$actuals)
RMSE(model3preddata$predictions,model3preddata$actuals)
# onceki derste hesapladigimiz mse degerlerinin aynisini verdi
# model1>model3>model2 seklinde verdi kullanilabilirlik olarak


MAE(model1preddata$predictions,model1preddata$actuals)
MAE(model2preddata$predictions,model2preddata$actuals)
MAE(model3preddata$predictions,model3preddata$actuals)
# burda da model3>model2>model1 verdi
# farkli metriklere gore farkli degerlendirildi

# R2 icin hepsi ayni
# MSE icin model1>model3>model2 # bu metrik karelerini aliyor
# MAE icin model3>model2>model1 # bu metrik eksilerin mutlak degerini alip ortalamasini aliyor
# kullanilabilirlik acisindan iyi tahmin yapiyor demesi acisindan

# mse icin kareler toplami seklinde calisiyorken
# mae icin eksi degerlerin mutlak degerlerini alip ortalamasini alip kullaniyor



# mse icin karesini aliyo ya yani buyuk hata degerlerim daha cok demek
# mesela 5 hata degerim olsa 25 vericek 10 olsa 100 vericek
# bu sekilde hata degerlerim arttikca o sayilar artiyor
# mae icin ise mutlak deger icine aldigimda hatalarimin ortalamai ne gelir oluyo bu da




##### min max accuracy(dogruluk)#####

# burda da dogruluga bakicaz. dogruluk oranimiz nedir diye bakicaz

model1MinMaxAccur <- mean(apply(model1preddata,1,min )/apply(model1preddata,1,max ))
# model icerisinde tum satirlari gezecek ve hangisi daha dusuk ve yuksek tahmin yapmis onlari alacak
# min degerlerle max degerleri bolecek
# sonra bu degerlerin ortalamasini aldik

model1MinMaxAccur
# dogrulugumuz 0.74 cikti model1deki dogrulugum 0.74

model2MinMaxAccur <- mean(apply(model2preddata,1,min)/apply(model2preddata,1,max))
model2MinMaxAccur
# bunun dogruluk 0.752781 cikti


model3MinMaxAccur <- mean(apply(model3preddata,1,min)/apply(model3preddata,1,max))
model3MinMaxAccur
# bunun dogruluk 0.753529 cikti


# olay su simdi ilk satirda gezdi apply ile hangisi kucukse ilk satirdan onu aldi ilk yapilan islem
# sonra ikinci satir icin de bakti en kucugu aldi
# bunu tummm satirlara yapti ve tum satirlardaki min degerleri aldi
# sonra / den sonraki yere baktigimizda yine ilk satirdan baslayip max degerleri aldi
# ikinci satira bakti en buyugu aldi
# bunlari min/max seklinde boldu ve ortalamasini aldi bu da dogruluk oranimi verdi
# burda cikan sonuc buyuk olan secilir


##### mean absolute percentage error(MAPE)####

# mean absolute errorun yuzdelik hali
# mae yani ustte gordugumuz

model1MAPE <- mean(abs(model1preddata$actuals- model1preddata$predictions)/ 
                     model1preddata$actuals)
# gercek deger ile tahmin edileni cikarip gercek degere bolup onun mutlak degerini aliyoruz ve
# en son ortalamasini aliyoruz


model2MAPE <- mean(abs(model2preddata$actuals- model2preddata$predictions)/
                     model2preddata$actuals)


model3MAPE <- mean(abs(model3preddata$actuals- model3preddata$predictions)/
                     model3preddata$actuals)


model1MAPE
model2MAPE
model3MAPE
# ortalama yuzde hatamiz bu
# model1 hata yuzdesi 0.35
# model2 032
# model3 0.33

# 2.sinde hatam daha az cikti
# 1 - dogruluk oldu



##### model secimi ####
# burada videoya gore gidecegim sadece model1 ve model2 karsilastirmalara dahil

# R2 fonksiyonuna gore ayni cikmisti
# RMSE fonksiyonuna gore model 1 daha iyi sonuc verdi
# MAE fonksiyonuna gore model 2 daha iyi sonuc verdi
# MAE ve RMSE arasindaki fark su RMSE'ye gore benim model2deki hata sayilar daha buyuk ama bunlarin
# mutlak degerini alirsam (MAE) model 2 daha az hata vermis gibi duruyor
# Min Max Accur islemine gore model 2 daha iyi sonuc verdi
# MAPE gore model 2 daha iyi sonuc verdi

# yani model2'yi kullanmak daha mantikli duruyor
# birbirlerine cok yakin sonuclar ama
# model 1 de kullanilabilir yani
# aykiri degerlerimi cikartmam modelimde cok da fark yaratmadi yani sonucuna ulasiyoruz






