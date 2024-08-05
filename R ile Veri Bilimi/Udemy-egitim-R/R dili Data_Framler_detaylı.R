
##### data frame boyutlari#####
df <- data.frame('A'=c(12,14,15,16,17),
                 'B'=c(12,17,67,54,34),
                 'C'=c('a','b','c','d','e'))
df

length(df$A) #kac gozlemden olusuyorogreniyoruz
nrow(df) # 5 satir demek
ncol(df)# 3 sutun demek

length(df) #sadece sutunu sayar.

dim(df) # 5 ve 3 yaziyo yani 5 satir 3 sutun demek
#once satir yaziyor sonra sutun
d<-dim(df)
d[1] #satir
d[2] #sutun


#####data framelerde sorgu islemleri####
df <- data.frame('A'=rnorm( 100, mean=10,sd=2),
                 'B'=rnorm(100, mean =34, sd=10),
                 'C'=rnorm(100, mean = 45,sd = 15))
View(df)

df$A
df$A < 8
k<-which(df$A<8)

df[k,] #a sutununda 8den kucuk olanlari ve diger sutunlarin onla kesiaen degerler

df[k,c('B','C')] #A'nin kucuk oldugu yerlerdeki b ve c sutunlari geldi

mean(df$B) #ortalamayi getirdi
o <- mean((df$B))
od<- which(df$B < o)

df[od,] #b'nin kendi ortalamasindan kucuk degerleri ve onlarla
#eslesen diger sutunlari getirdik
df[od,c('A','B')] #b'nin kendi ortalamasindan kucuk degerlerle eslesen
#a ve c sutunlari


View(df[od,]) #tablodan aciyor

##### birden fazla kosul subset() ile####  

data() #R icerisindeki datalari gosteriyr
CO2 #R icerisinde bulunan bi data mesela.
View(CO2) #tablo olarak goster de bu


?subset

subset(x, subset, select, drop = FALSE)
#bu sorguda subset kismi hangi alt sorgulari kullanicaz onu yazcaz
#select kismi hangi elemanlari sececcksin demek


#Type Quebec, uptake 30dan buyukleri secelim. CO2 icin
names(CO2)
subset(CO2, subset= (uptake >30 & Type =='Quebec'))
#belirledigimiz kosullarda tum tablo geldi. 
#select kullanarak hangi sutunlarin gelmesini istedigimizi de secebiliriz

df <- subset(CO2, subset = (Type =='Quebec'& uptake >20), select = c('Type','uptake'))
#uptake ve type kismini verdi

View(df)


subset(CO2, subset = (Treatment !='chilled' & conc <= 500), 
       select = c('Type','uptake','conc','Treatment'))
#istredigimzi sirayla verio. select yazdigim sirayla veriyo

subset(CO2, subset = uptake >20, drop = F)



subset(CO2, subset = (Treatment !='chilled' | conc <= 500))



#### subset ile icice kosullar####


data()

BOD
View(BOD)
names(BOD)
subset(BOD, subset = (Time < 5 & Time > 3)|demand == 19 , select = c(Time)) 
# zaman 5 ile 3 arasinda olsun veya demand 19 olsun dedik sadece zamani getir dedik

CO2
names(CO2)
nrow(CO2)
subset(CO2, subset = (Type==c('Quebec','Mississippi')))
#c() ile birden fazla type esittir kullanabildik

df <- subset(CO2, subset = (Type =='Quebec'|Treatment =='nonchilled')&(uptake>30&uptake<35))
df #satir sayilari diger tabloda neyse o olarak gelio
row.names(df) <- seq(from = 1, to = nrow(df),by=1)
df #satir sayilarini 1den kac tane varsa ona kadar yaptik

##### satitlar ve sutunlar bazinda ortalama##### 

data()

View(iris)

d1 <- rowMeans(iris[1:4])
d1


iris[length(iris)+1] <- rowMeans(iris[1:4])
iris

names(iris[length(iris)]) <-  
View(iris)

colnames(iris[6]) <-'Means' #calismadi


iris <- iris[-6] #sildik o yuzden
iris

iris['Means'] <- rowMeans(iris[1:4]) #ve bu sekilde direkt ad vererel ekledim
View(iris)


d2 <-  rowMeans(iris[1:2])
iris['ortalama2'] <-d2
iris

View(iris)

colMeans(iris[1:4], na.rm = T) #sutun ortalamalarina yazanlari koymayalim





##### data frame gruplar bazinda hesaplamalar#####

View(iris)
iris <- iris[-length(iris)] #2 kere calistirdik. son 2 sutunu sildik 
#ortalama eklemistik
iris

View(iris)
# asagida i v ve vc olarak yaptigim islemler 3 farkli data.frame olusturup
#diger tablodaki degerleri oraya aktarmakti. bunu ben oylesine yaptim.
#alistirma icin
s <- which(iris['Species']=='setosa') #species setosa olanlarin indislerini aldim
setosa <- iris[s,] #setosa olanlarin tum verileri geldi
setosa
setosa['ortalama'] <- rowMeans(setosa[1:4])
View(setosa)


v <-  which(iris$Species=='versicolor')
versicolor <- iris[v,]
versicolor['ortalama'] <- rowMeans(versicolor[1:4])
versicolor

vc <- which(iris$Species=='virginica')
virginica <- iris[vc,]
virginica['ortalama'] <- rowMeans(virginica[1:4])
View(virginica)


#sepal.length sutunlarinin ortalamasini aldik her bir species icin
meanSetosa <- mean(iris[s,c('Sepal.Length')])
meanVersicolor <- mean(iris[v,c('Sepal.Length')])
meanVirginical <- mean(iris[vc,c('Sepal.Length')])
#ortalamalar
meanSetosa;meanVersicolor;meanVirginical



aggregate(iris[1:4], by= list(iris$Species), FUN = mean)#ortalama
#yukarida yaptigim islemi direkt yapti
#by kismi liste olmak zorunda

aggregate(iris[1:4], by= list(iris$Species), FUN = sd) #standart sapma


aggregate(iris[1:4], by= list(iris$Species), FUN = sum) #toplam

aggregate(iris[1:4], by= list(iris$Species), FUN = var, na.rm =T) #varyans

#kendi olusturdugumuz fonksiyonlari da ileride ekleyenilirz






##### excelden r'ye data aktarmia######

#onerilen budur. aynisini sagdan aktarma kisminda from text ile de yaparsin
read.csv('')

getwd()
setwd('C:/Users/halil ibrahim kaya/Desktop/R ile Veri Bilimi')

list.files()

df <-  read.csv('CSV_singapore.csv',header = T,sep = ";",dec = ".")

View(df)


df2 <-  read.csv('CSV_singapore.csv',header = T,sep = ",",dec = ".")
#virgulk ile ayirdik dedik ama noktali virgul ile ayirdik. sacma cevap verdi
#kontrol et
View(df2)



getwd()
setwd("C:/Users/halil ibrahim kaya/Desktop")


df2 <-  read.csv('R ile Veri Bilimi/CSV_singapore.csv',header = T,sep = ";",dec = ".")
View(df2)
#konumu dekstop yapip denedik.bu sekilde yazmamaiz gerek.

setwd("C:/Users/halil ibrahim kaya/Documents")
getwd()






##### iki data framei merge ile birlestirme#####

#inner join yapiyormusuz gibi dusun.
#demografik tablosundaki id ile transactions tablosundaki customer_id aynidir.



?merge
merge_df <- merge(Demografik,Transactions,
      by.x = "ID", by.y = "CUSTOMER_ID")
View(merge_df)

names(Transactions)[2] <-  'ID' #customer_id yazan yeri id ismine veirdik.
#baska bi yontem icin iki parametrenin de adi ayni olmali


merge_df2 <-  merge(Demografik,Transactions,
                    by = 'ID')
View(merge_df2)
#iki tabloda da ayni degerleri tasiyan 2 sutunun ismi de id idi
#burda ikisini birlestirme islemini bu sekilde yaptik. genellikle ayni isim kullanilmiyo ama
#o yuzden ilk yontemi unutma  



##### data framelerin ozet bilgileri#####


View(iris)
summary(iris) #ozet bilgileri veriyor. medyan standart sapma, ortalama vsvs.

View(summary(iris))

str(iris) #degiskenleri alip hangi tur onu kontrol ediyo

head(iris) #veri setinin ilk 6 satirini veriyo

iris[10:15,] #10-15 arasi satirlari verir  

?head

head(iris, n = 10) #10 satir verir

tail(iris) #sondan 6 verir

tail(iris, n = 10) #son 10 satir






###### data framlerde degiiken istatistikleri####



mean(singapore$price) #na geldi
any(is.na(singapore$price)) #na varmis
mean(singapore$price, na.rm = T) #168.9887 cebaini verdi. nalari yok saydik

sd(singapore$price, na.rm = T) #340.0893 verdi

#standart sapma ile ortalama arasinda cok fark var. sapma cok mevcut demek.



median(singapore$price,na.rm = T)#124 verdi

#ortalama medyandan buyukse grafik saga carpiktir. yani veriler sol tarafta cogunluktadir

hist(singapore$price) #cok fazla veri var ve hepsi 1000 kisminda duruyo
#anca sorun su ki 10000 civarinda da verilerimiz var
#bu yuzcden tablo sacma olusturuluyor. daha mantikl bi tablo icin
#ucurum farklari atmamiz daha iyi olur. daha once de soylemistik.
#boyle durumlarda ayni zamanda ortalama yerine medyan kullanmak da saglikli olabilir


hist(singapore$price[singapore$price <1000]) #1000den kucukleri ver dedik
#ve saga carpik tabloyu aldik. yani veriler solda cok



var(singapore$price, na.rm = T)#115660.7 verdi
sd(singapore$price,na.rm = T)#340...... aradaki fark cpk fazla la

min(singapore$price, na.rm = T)  #0 verdi
max(singapore$price, na.rm = T) #10000 verdi

table(singapore$room_type)
#ust satir degerlerimizi, altindakiler de kac kere tekrar ettigini verir

which(singapore$room_type=='110')

df <-  singapore[-964,] #110u kaldirdik
df

table(df$room_type) #110u kaldirdik ve baktik kalkmis

class(df$room_type)


which(df$room_type == "")
df <- df[-which(df$room_type == "")]
#ici bosmolanlari kaldirdik

quantile(df$price, na.rm = T) #ceyreklikleri hesapladi
#cok onemli analiz icin. cunku %75. sayi 124 sayisi ama %100 olan 
#sayi 10000. yani son sayilar cok artmis. bu da ilk basta dediklerimizi
#dogrular niteklikte

quantile(singapore$price, na.rm = T)



##### data frameler uzerinde degisken turu donusumleri#####


class(CSV_singapore$name) #karakter ama faktor sayalim

CSV_singapore$name <- as.character(CSV_singapore$name) #karaltere cevirdik.

df <- CSV_singapore #ismi uzun diye atadim direkt
View(df)
df <- data.frame(CSV_singapore)
df
View(df)


df['name']
class(df['name']) #data.frame diyo
cv <- as.character(df['name']) #butun sutunlari slaslari falan her seyi tekte verdi

cv[1] #data framin icindeki onu data frame yapan operatorleri bile getirdi
#hatalidir. vektorlerden olusursa as. kullan. data framelerde olmuyo
#o yuzden kare parantezin icine virgul kullanip istedigimiz alani direkt belirtelim.
df[,c('name')]
class(df[,c('name')]) #karakter diyo. cevirme islemleri rahat olur bunda

as.factor(df[,c('name')])
df[,c('name')] <-  as.factor(df[,c('name')]) #faktor yaptik
class((df[,c('name')]))
View(df)



class(df$id) #karakter dio

#faktorden numerice direkt cevirilemiyor. once karakter sonra numeric yapman gerek


any(is.na(df$id))

class(df$host_id)

class((df$room_type)) #karakter
table(df$room_type) #gereksizler var sayilar var ayiklama yapmamiz lazim

bos <- which(df$room_type ==''| df$room_type =='110'|
        df$room_type =='1200'| df$room_type =='294'|
        df$room_type =='299'|df$room_type =='31'|
        df$room_type =='40'|df$room_type =='50'|df$room_type =='60'|
        df$room_type =='74'|df$room_type =='75'|df$room_type =='750')
#tablodaki bos ve sayilar vardi onlarin indislerini aldik. simdi onlari tablodan ayiklicaz

df$room_type[bos] #gereksizleri aldik
df$room_type[-bos] #gereksizler disindakileri ver dedik

df <-  df[-bos,]
table(df$room_type) #gereksizleri cikartmisizzzzzzzzzzzzz cokiiz








##### apply fonksiyonu ile gruplari hesaplama#####
View(iris)

#margin=1 satirlar bazinda yapar
#margin=2 sutunlar bazinda yapar
apply(iris[1:4], MARGIN = 1, FUN = mean)
#satir bazli ortalamalari verdi.

#saglama yapioz
mean(as.numeric(iris[1,1:4])) #data frame olarak vermisti once karaktere cevirdk
#oyle yaptik. onemli detay
class(iris[1,1:4]) #data frame


apply(iris[1:4], MARGIN = 1, FUN = sd) #standart sapmalari verdi
#saglamaya gerek yok buldu. ya da neyse yapak

sd(as.numeric(iris[1,1:4])) #yaptik dogruymus.

apply(iris[1:4], MARGIN = 1, FUN = sum) #toplamlari aldik

#margini 2 alalim
apply(iris[1:4], MARGIN = 2, FUN = sd ) #her bir sutunun adini veriyo
#ve sutunlara ait standart sapmasini verdi

apply(iris[1:4], MARGIN = 2, FUN = max) #max degerleri verdi

#fun kismina istedigimiz fonksiyonu veriyoruz.


#aggregate() fonksiyonunda by = diyip sutun adi giriyorduk ve sutunlari veriyodu
#burda margin ile hem satilari hem sutunlari alabiliyoz


lapply(iris, FUN = mean)
#burda sonuclari bize liste liste olarak veriyor
#son sutunda da karakterler oldugu icin na verdi meselaaa

#lapply'da margin yok. lapply her bir degisken uzerinde islem yapar. yani sutunlari kullanir
#satirlari kullanmaz


l <- list('a'=c(23,24,55,67,89,90),
          'b'=c(13,45,65,23,45),
          'c'=c(11,22,34,23,12))
l

lapply(l, FUN = mean) #liste liste ortalamalarini aldi.
# a icin verdi , b icin verdi, c icin verdi

lapply(l, FUN = sd)
lapply(l, FUN = max)

#data frame kullaniyorsak apply, liste kullaniyorsak lapply kullanalim.
#


##### attach detach ve with fonksiyonlari#####

df <- singapore #adi uzun

View(df)
names(df)

df$id #idleri verir
df['id'] #idleri verir
df[964,]

attach(df) #degiskenleri direk olarak yazabilitoruz sayesinde
host_name
host_id
class(host_id) #karakter
class(host_name) #karakter


#dezavantaji isimler birbiriyle cakisabilir. iki ayri tabloda iki name 
#degiskeni oldugunu varsayalim. bu degiskenler birbiriyle carpisir.
#tek data.frame ile calisirken attach cok iyidir. birden fazla varsa isimler
#karisir. hangi degisken nerde karistiririz.

detach(df) #attach komutunu kapatiyor.
host_id #hata verdi
#kac kere attach ettiysen o kadar detach etmen gerek

attach(df)
attach(df)
host_id  
detach(df)  
host_id  #iki kere attach ettik diye calisti. 2 kere de detach etmemiz gerekiyor
detach(df)
host_id #calismadi


with(df, print(host_name)) #df icerisindeki elemanlari () icerisinde df yazmadan
#kullanmak icin

with(df, {
        x <- mean(price, na.rm = T) 
        y <- x-5
        print(y)
}) #163.9887 degerini aldik. ayri ayri butun satirlari calistirdik bole


with(df, print(price)) #mesela
#bazi yerlerde kullanisli olabiilyorrrrrrr.










##### odevvv####

#Ekte size iletmis oldug m veri dosyasini lutfen indiriniz. 
#Bu veri dosyas  farkl i tarihlerdeki Kadinlar Dinya Kupasi karsilasma sonuclarini icermektedir. 
#Bu odev kapsaminda bu veri dosyasi uzerinde islem yapacagiz. Lutfen odev sorasinda asagdaki talimatlari takip ediniz.

#Veri dosyasini read.csv()  fonksiyonunu kullanarak R'a aktariniz ve bu veriye df ismini veriniz.

#Veri dosyasinda 1980 Aralik 1 ve 2019 Aralik 31 tarihleri arasinda kalan kayitlarda. 
#Home Team'i (ev sahibi) Hong Kong ve Away Team'i (deplasman takimi) Philippines olan takimlarin
#sadece  date, home_score, away_score, home_team ve away_team degerlerini bir data frame olarak elde ediniz.

#Not:  Tarih verilerini as.Date fonksiyonunu kullanarak R'da tanimli tarih formatina donusturmeyi unutmayiniz.



df <- read.csv('WomenFootballResults.csv' , header = T , sep = ",")

subset(df, subset = (date>as.Date('1980-08-01') & date<as.Date('2019-12-31')& home_team =='Hong Kong' & away_team =='Philippines'),
       select = c('date','home_score','away_score','home_team','away_team'))






