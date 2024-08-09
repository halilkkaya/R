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
# temp degerine gore tahmin modelinde gercek deger - tahin edilen deger 
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



#### aykiri deger kontrolu

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




distance <- mahalanobis(air,center = air.center,cov = air.cov)
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

air_new1 <- na.omit(air_new[c("Ozone","Temp")])
View(air_new1)

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
# aykiri degerleri?? bunlar da 

inds <- which(distance1>cutof)

air_new1[inds,]

air_new1 <- air_new1[-inds,]


model3 <- lm(Ozone~Temp, data = air_new1)

model
model2
model3
# intercept ve temp degerlerindeki dususu goruyroz


summary(model3)
# min degerim -35 mac degerim 29 olmus 
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












