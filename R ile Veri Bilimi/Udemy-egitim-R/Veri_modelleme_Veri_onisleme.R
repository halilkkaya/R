##### min max normalizasyon islemi ######

# verileri 0-1 arasina sikistiriyoruz

(x - min(x))/ (max(x)- min(x))
# min max normalizasyon formulum
# indisteki ilk sayiyla mininimum sayiyi cikariyor sonra onu maximum sayi ve minimum sayi
# farkina boluyor. formul budur.
# mesela c(2,3,1,5,6,7) x=2, min(1), max(7)

b <- c(2,3,1,5,6,7)
c <- numeric(length = 6)
for (i in 1:length(b)) {
  
  res <- (b[i] - min(b))/(max(b)-min(b))
  c[i] <- res
}
c
minMaxNorm(b)
# bi deneme yaptim ayni sonuclari aldim fonksiyonu asagida olusturmustuk

minMaxNorm <- function(x){
  a <- (x - min(x))/ (max(x)- min(x))
  return(a)
}
minMaxNorm(df$cdur)
# 0-1 arasina normalize islemi yaptik

min(minMaxNorm(df$cdur))
# 0
max(minMaxNorm(df$cdur))
# 1
# 0 ile1 arasina yerlestirdik yani

df$cdur <- minMaxNorm(df$cdur)
df$vdur <- minMaxNorm(df$vdur)
df$wordfreq<- minMaxNorm(df$wordfreq)
# hepsini 0-1 arasina yerlestirdik
# 100 ile carparsak 0-100 arasina koyariz
df

df$cdur <- minMaxNorm(df$cdur)*100
df$vdur <- minMaxNorm(df$vdur)*100
df$wordfreq<- minMaxNorm(df$wordfreq)*100
# 0-100 arasina aldik


##### scaling (olceklendirme- standartlastirma) islemi #####

?scale 
# x numeric matrix 
# center ortalamsi
# scale standart sapmasi ikisi de logical deger true dersek verdigimiz
# verilerdeki o verileri kullancak

scale(df$vdur,center = T,scale = T)
# standartlastirilmis verilerimiz
# 
mean(df$vdur)
sd(df$vdur)
# scale fonk icindeki verilerle ayniymis

# yani verimizin ortalama degerine ve standart sapma degerine gore 
# standartlastirma islemi yaptigini soyleyebiliriz

scale(df$vdur,center = 50,scale = 5)
# centeri 50 scalei 5 olan standartlastirilmis verileri olusturduk

# neden bu sekilde fonk verildi peki
# modelimizi yaptik diyelim modelimizdeki degiskenler
# bu sekilde scale yapilmis degiskenlerdir
# ve ardindan bi veri geldi ve veriden tahmin yapicaz
# veriyi once scaleden gecirmek gerekiyor. bu yuzden verinin
# kendi ortalama standart sapmasini kullanilmasi gerekiyor




par(mfrow=c(1,2))
hist(df$vdur)
hist(scale(df$vdur))

# ufak tefek degisiklikler var ama bu veriyi degistirmek degildir
# sadece standartlastirmadir. veriler degismez


###### aykiri deger tespit yontemleri ######

# boxplot yontemiyle

library(rstatix)


identify_outliers(as.data.frame(AirbnbNewyork$price))
## bayyaa cok veri verdi bazilari ekstrem hatta o da yaziyo

out <- identify_outliers(as.data.frame(AirbnbNewyork$price))

names(out)

min(out$`AirbnbNewyork$price`)
# min deger 335

max(out$`AirbnbNewyork$price`)
# 10000 max deger

min(AirbnbNewyork$price)
max(AirbnbNewyork$price)
# direkt tablodaki min ve max degerlerim

# min degerimiz normal tablomuzda 0 geld
# out verisinde de 335di yani aslinda 335 altindakiler normal veri 
# ustundekiler de aykiri deger mantigini kullaniyoruz

ids <- which(out$is.extreme== TRUE)
# ekstrem degerlerin indislerini aldik
ekstrem <- out[ids,"AirbnbNewyork$price"]
# ekstrem degerlerim

min(ekstrem)
# 495
max(ekstrem)
# 10000

# ekstrem deger araligimdir

library(tidyverse)

df_clean <- AirbnbNewyork %>% filter(price<335)
# aykiri degerleri attik

p <- numeric(length = 100)
for (i in 1:100) {
  deger <- sample(df_clean$price,size=100)
  test <- shapiro.test(deger)
  pvalue <- test$p.value
  p[i] <- pvalue
}
mean(p)
# normallik testi yaptik normal cikmadi
# p degerim 0a cok yakin

hist(df_clean$price)
# harbiden saga carpik yani veriler solda togunluklu bi tablomuz var
# neyse burasi oylesine deneme icin yaptim 

## z, t, chi square skorlarina gore aykiri deger kontrolu

install.packages("outliers")
library(outliers)


setwd("C:/Users/halil ibrahim kaya/Desktop")
load(file = ".Rdata")
# calisma dosyami masaustune kaydetmistim ordan devam icin loading islemi yaptim



?scores

View(airquality)
scores(airquality$Ozone, type = "z", prob = 0.95)
# NA sonuclari geldi. veri setinde cok NA deger vardi onlari aykiri verdi
# na.omit ile NA'lari atip ole bakalim

a <- scores(na.omit(airquality$Ozone), type = "z", prob = 0.95)
# TRUE FALSE seklinde hangi degerlerin aykiri oldugunu verdi

b <- which(a=="TRUE")
# TRUE yazanlari yani aykiri deger olanlarin indislerini ald

na.omit(airquality$Ozone)[b]
# a atamasini yaparken NA degerleri silip aykiri degerleri oyle bulduk
# o yuzden na.omit diyerek o indisleri aramamiz gerekki indis
# kaymasi olmasin
# verilere bakiyoruz ve 97den sonrasi aykiri deger gibi duruyor
# yine de bunu fonksiyonlarla yapalim
min(na.omit(airquality$Ozone)[b])
# 97 diyor

max(na.omit(airquality$Ozone)[b])
# 168 diyor

# yani 97den buyukleri atacagiz

par(mfrow=c(2,1))
hist(airquality$Ozone)
hist(na.omit(airquality$Ozone)[-b])
# grafik olarak da inceleyelim dedik
# grafikleri alt alta baktik 



library(tidyverse)

df_clean <- airquality %>% filter(Ozone<97) 

# temiz verileri aldik


# t ile yapma
View(airquality)
scores(airquality$Ozone, type = "t", prob = 0.95)
# tine na.omit islemi yapalim
a <- scores(na.omit(airquality$Ozone), type = "t", prob = 0.95)
b <- which(a=="TRUE")
na.omit(airquality$Ozone)[b]
# ayni sonuclari verdi yine



par(mfrow=c(2,1))
hist(airquality$Ozone)
hist(na.omit(airquality$Ozone)[-b])
# grafikleri de ayni


# ki square ile yapma
View(airquality)
scores(airquality$Ozone, type = "chisq", prob = 0.95)
# yine na.omit islemi yapalim
a <- scores(na.omit(airquality$Ozone), type = "chisq", prob = 0.95)
b <- which(a==TRUE)
na.omit(airquality$Ozone)[b]
# daha az sonuc en kucuk degeri 108 olarak aldi diger tarata 97ydi
# sebebi chi sq saga carpik veriler verdigi icin biraz daha
# verilerin soldakilerini aykiri saymadi


# iqr ile yapma
View(airquality)
scores(airquality$Ozone, type = "iqr", prob = 0.95)
# yine na.omit islemi yapalim
a <- scores(na.omit(airquality$Ozone), type = "iqr", prob = 0.95)
b <- which(a==TRUE)
na.omit(airquality$Ozone)[b]
# hicbi aykirid eger bulmadi bu


# mad gore yapma medyan yani
View(airquality)
scores(airquality$Ozone, type = "mad", prob = 0.95)
# yine na.omit islemi yapalim
a <- scores(na.omit(airquality$Ozone), type = "mad", prob = 0.95)
b <- which(a==TRUE)
na.omit(airquality$Ozone)[b]
# mad yani medyana gore en kucuk aykiri deger 77 oldu
# daha fazla deger verdi


par(mfrow=c(2,1))
hist(airquality$Ozone)
hist(na.omit(airquality$Ozone)[-b])
# aykiri degerleri cikarinca daha da normale dondu ama yine de saga carpiklik var





###### mhalanobis fistance ile cok degiskenli aykiri deger kontrolu#####

getwd()
setwd("C:/Users/halil ibrahim kaya/Desktop")

load(file = ".RData")
# yine kayit dosyami getirdim

library(ggplot2)
library(car)
# gerekli kutuphaneleri getirdim

View(airquality)

# ozone ve temp kullanip wind'i tahmin etmeye calisacagiz  

fig <- ggplot(airquality, aes(x=Ozone, y= Temp)) +
        geom_point(size=2)+
        xlab("ozone degerleri")+ ylab("temp degerleri")
fig
# na degerleri icin uyari veriyor ama grafik
# yine de verildi
# lineer iliski var gibi duruyor yani 
# biri artinca digeri de artiyo ama aykiri degerler bunu bzouyor gibi duruyo
# ortalamaya bi elips ciziyo ve aykiri degerleri buluyor
# cok degiskenli veriler icin boyle yapiliyor


X <- na.omit(airquality[c("Ozone","Temp")])
View(X)
# NAlari attik

air.center <- colMeans(X)
air.center
# ortalamalarini aldik ozone 42, temp 77

air.cov <- cov(X)
air.cov
# matris verdi temp-ozone kovaryansi 218 cikti
# yani birlikte olan degisim katsayilari

# alipsin yari capini hesaplamamiz lazim bunu da kikare dagilimindan bulacagiz

rad <- sqrt(qchisq(0.95, df = 2))
# df kac degisken varsa o kadar sayi gircez
# 0.95 de olasilik degerim
# ki kare testi oldugu icin karekok alinmis halini almamiz gerek
rad
# 2.44 verdi yani yaricapim 2.44





elipse <- ellipse(center = air.center,shape = air.cov, radius = rad,
        segments = 100, draw = FALSE)

elipse
# ellipse degerlerim var. x ve y koordinatlarimi verdi bunlari ggplot tablosuna 
# ekleyecegiz 

colnames(elipse) <- colnames(X)
# sutun adlarini degistirdik

fig <- fig + geom_polygon(data = elipse, color = "orange",
                   fill = "orange",
                   alpha = 0.3) +
          geom_point(aes(x= air.center[1],y= air.center[2]),
           size = 5, color = "blue")

fig
# elipsi cizdik simdi aciklayayim
#  fig ile halihazirda yukarida olusturdugumuz grafigi cagirdik
# + operatoru ile ustune geom_polygon fonksiyonu ile elips cizdirdik
# geom_polygon icersinde data elips degerlerimi yani kopordinatlarimi
# color rengimii fill icerisinin rengini yani dolguyu diyelim
# alpha da dolgunun seffafligini belirtiyor
# geom_point ile iki degiskenin ortalamalarinin kesitigi noktayi cizdik
# bu da bizim center yani merkez noktamiz oldu.
# aes icerisine x ve y degerlerine ortalamalari ataadik
# noktanin buyuklugunu de size ile belirledik
# color ile noktaya renk verdik
# ellipse icerisinde olanlar normal disinda olanlar ise aykiri degerler
# biz bunu ki kareye gore cizdik yani 0.95 ile yaptik kesinligi daha da arttirisak
# nawsil olur onu incelkeyelim

fig <- ggplot(airquality, aes(x=Ozone, y=Temp)) +
  geom_point(size=2)+
  xlab("ozone degerleri")+ ylab("temp degerleri")
fig
X <- na.omit(airquality[c("Ozone","Temp")])
View(X)


air.center <- colMeans(X)
air.center
# ortalamalarini aldik ozone 42, temp 77

air.cov <- cov(X)
air.cov
rad <- sqrt(qchisq(0.99, df = 2))

elipse <- ellipse(center = air.center,shape = air.cov, radius = rad,
                  segments = 100, draw = FALSE)
elipse
colnames(elipse) <- colnames(X)

fig <- fig + geom_polygon(data = elipse, color = "orange",
                          fill = "orange",
                          alpha = 0.3) +
              geom_point(aes(x= air.center[1],y= air.center[2]),
             size = 5, color = "blue")

fig
# 0.99 ile ikinc, olusturdugum daha da cok alan aldi
# bir de 0.80 yapalim
rad <- sqrt(qchisq(0.80, df = 2))

elipse <- ellipse(center = air.center,shape = air.cov, radius = rad,
                  segments = 100, draw = FALSE)
elipse
colnames(elipse) <- colnames(X)

fig <- fig + geom_polygon(data = elipse, color = "orange",
                          fill = "orange",
                          alpha = 0.3) +
  geom_point(aes(x= air.center[1],y= air.center[2]),
             size = 5, color = "blue")

fig
# daha kucuk bi alani aldi yani daha fazla aykiri deger verdi
# dig degerini hic degismedigimiz icin hep ic ice cizdi
# genel olarak 0.95 kullan ama
# aykiri degerleri bulalim



dist <- mahalanobis(x = X ,center = air.center , cov = air.cov)
dist
# tablo formatinda gozlem indislerine gore merkezden uzakliklarini verdi
# ki kare kullanip cutoff yani kesim noktasi dgeri bulalim


cutoff <- qchisq(p=0.95, df=2)
cutoff

ids <- which(dist>cutoff)
ids
# aykiri degerlerimin indekslerini noyle buldum

names(ids)
# indislerimi veriyor

X[ids,]
# aykiri degerlerimi buldum mahlanobise gore bulduk


##### kayip degerleri degerlendirme yontemleri####

# kayip degerlerin cikarilmasi

# ortalama deger ile doldurma

# yeni bir gozlem ile yer degistirme

# Hot Deck Imputation(rastgele deger atama)

# Cold Deck Imputation (en yakin gozlem ozelliklerine sahip baska bir gozlemin degeri)

# Regression Imputation (regresyon modeli ile tahmine dayali atama islemi)

# Stochastic Regresion Imputation( Regresyon modeli tahmin ve rastegele artik deger ile toplama)

 

install.packages("MICE")
install.packages("Amelia")
install.packages("missForrest")
install.packages("Hmisc")
install.packages("mi")
 
# bu bolum icin gerekli paketleri simdiden yukledim




## kayip degerler nasil ve hangi durumlarda cikartilmali ##

df <- student_placement_data_with_NA
# df seklinde yazalim istedim

View(df)

is.na(df)
# hangileri na veya degil True False ile bize tablo verdi.

indis <- which(is.na(df))
length(indis)
# 39000 tane na degerim varmis

df_new <- na.omit(df)

nrow(df)
# 20k satir
nrow(df_new)
# 2630 satir
# bayyyyaaa gitmis veri 17400 veri gitmis resmen
# na olan satirlari sildigimiz icin sikintili oldu
# ne zaman na.omit kullanalim?
# ayni satir uzerinde cok fazla na varsa o satiri cikartalim
# diger turlu 39 degiskenden 1 tanesi na diye cikarirsak elimizde veri kalmaz
# bu turler icin doldurma kullanacagiz. deneyelim



## ortalama atama yontemi  ##

is.na(df$Acedamic.percentage.in.Operating.Systems)
ids <- which(is.na(df$Acedamic.percentage.in.Operating.Systems))
# na degerlerin indislerini aldik

length(ids)
# 993 deger na imis bu sutunda

ort <- mean(df$Acedamic.percentage.in.Operating.Systems,na.rm = T)
# 77.00952 verdi bunu atayalim bi degere

df$Acedamic.percentage.in.Operating.Systems[ids] <- round(ort) 
# atama islemi yaptik. round ile sadece int kismini aldik


is.na(df$Acedamic.percentage.in.Operating.Systems)
which(is.na(df$Acedamic.percentage.in.Operating.Systems))
# hic yok dedi yani atama islemi basarili oldu.
# na degerlerine o sutunun ortalamasini atadik

# bazi durumlarda kullanisli yontem olurken bazi durumlarda olmaz
# bakalim bu veri seti icin nasil degerlendirecegiz

# 993 tane NA degerimiz vardi
# 20000 tane de gozlemimiz vardi
# 20knin yaninda 993 kucuk de olsa baya buyuk bi sayi. 
# veri setimizde aykiri degerler varsa aykiri degerlerden etkilenen
# bi ortalama olusturmus oluruz.
# bu yontem icin oncelikle aykiri degerleri kaldirmak mantikli olabilir

# cok degiskenli veri setlerinde degiskenlerimizin birbirine olan
# bagimliligini iliskisini goz onunde bulundurarak tahminler yapmak
# daha mantikli olabilir

# ama tek degiskense ortalama atama mantikli olabilmektedir



## rastgele olarak kayip deger doldurma (Hot Deck Imputation)##

set.seed(123)
rnorm(2)

rnorm(2)

set.seed(123)
rnorm(2)
# set.seed(123) yazdikran sonra aldigimiz sayilar neyse yine onlari verdi
# rastegele veri uretirken onlari sabitlemek icin kullanilir

indis <- which(is.na(df$Acedamic.percentage.in.Operating.Systems))

length(which(is.na(df$Acedamic.percentage.in.Operating.Systems)))
# 993 deger NA


# istersek bi tane rastgele sayi olusturup hepsine onu atariz

random <- sample(df$Acedamic.percentage.in.Operating.Systems[-indis],size=1 )
# NA indisleri olmayan degerlerden 1 sayi al

random

x <- df$Acedamic.percentage.in.Operating.Systems

x[indis] <- random

which(is.na(x))
# NA degerlerim gitti atama islemi yaparak bunu direkt verilere yazabiliriz
# df$Acedamic.percentage.in.Operating.Systems <- x
# seklinde atama yaparak eklemis oluruz ama ben yapmayacagim baska seyler de deneyecegiz diye


mean(x)
# rastgele eklenen sayili
mean(df$Acedamic.percentage.in.Operating.Systems,na.rm = T)
# orijinal veriler

# aradaki fark 0.05 cikti baya az cok fazla oynamamis ama ortalamayi asagi cekmis




# kayip gozlem sayimiz kadar random sayi secelim


set.seed(12)
random_many <- sample(df$Acedamic.percentage.in.Operating.Systems[-indis], size = length(indis))

random_many
# set seedle beraber calistir hep ayni sayilari atar
  
y <- df$Acedamic.percentage.in.Operating.Systems

y[indis] <- random_many

mean(y)
mean(df$Acedamic.percentage.in.Operating.Systems,na.rm = T)
# neredeyse hic fark olmamis 0.008 fark var

# kayip deger sayisi kadar rastgele deger cekme daha mantikli bir yoldur tek veri secmeye gore




## Cold Deck En yakin gozlem degeri atama ##


df <- student_placement_data_with_NA


index <- which(is.na(df$percentage.in.Algorithms))
length(index)
# 1008 tane deger na var


index[1]
names(df)
# "certifications"
# "workshops"
# bu iki degiskenlerin sonuclarina ait diger verilerden iliski kurup veri
# verecegiz na degerimize

df[c("certifications","workshops")][index[1],]
# percentage.in.Algorithms sutununda bulunan ilk NA indisimizin
# "certifications","workshops" verilerini aldik
# ilk indisin bu degerleri python ve data science imis


d <- subset(df, 
       select = c("percentage.in.Algorithms","certifications","workshops"),
       subset = (certifications=="python"& workshops =='data science'))

# sertifikasi python ve calisma alani veri bilimi olanlarin percentage.in.Algorithms 
# degerlerini fln getirdik. iclerinde de NA olanlar var dikkat

anyNA(d)
# NA degerim varmis


ort <- mean(d$percentage.in.Algorithms,na.rm = T)
ort
# 76.66805 ortalamasini verdi

df$percentage.in.Algorithms[index[1]] <- ort

# artik kayip degerimiz yerine o alanda calisanlarin ortalamasini girdik
# bu tek bir veri uzerine yaptigimizdi bunu hepsine yapalim

df <- df %>% mutate(percentage.in.Algorithms = ifelse(certifications == 'python' & workshops == 'data science' & is.na(percentage.in.Algorithms), 
                                                      round(ort), percentage.in.Algorithms))
# burada python data science ve na degeri olanlari buldum ve bunlarin hepsine atamayi yaptim

d <- subset(df, 
            select = c("percentage.in.Algorithms","certifications","workshops"),
            subset = (certifications=="python"& workshops =='data science'))
# burada kontrol amacli python ve data science kosullarini saglayanlarini aldim
anyNA(d)
# ve na degeri var mi baktim. gercekten de yaptigimiz atama onlarin yerine ortalamayi atmis


manip <- df %>% select(certifications,workshops,percentage.in.Algorithms) %>%
  filter(certifications=='python'&workshops=='data science')
# once ve sonra ortalamalarini kontrol etmek amacli manip adinda python ve data science
# eslesmelerini aldim

# ortalamalari kontrol edelim

mean(df$percentage.in.Algorithms,na.rm = T)
# once genel ortalamasi 76.96125

mean(manip$percentage.in.Algorithms, na.rm = T)
# once python ve data science olanlarin ortalamasi 76,66805

# simdi verileri aktarip bakalim
mean(df$percentage.in.Algorithms,na.rm = T)
#yeni once genel ortalamasi 76.96128

mean(manip$percentage.in.Algorithms)
# yeni python ve data science olanlarin ortalamasi 76.6875


# verilerimizdeki degisimler bu sekilde olmustur. videolarda tamamina atama yoktu
# tidyverse kullanarak ben yaptim


##### kayip degerleri gorsellestirme ####

