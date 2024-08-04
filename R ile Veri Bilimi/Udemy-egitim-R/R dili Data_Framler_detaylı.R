
##### data frame boyutlar??#####
df <- data.frame('A'=c(12,14,15,16,17),
                 'B'=c(12,17,67,54,34),
                 'C'=c('a','b','c','d','e'))
df

length(df$A) #kac g??zlemden olusuyor ??greniyoruz
nrow(df) # 5 sat??r demek
ncol(df)# 3 s??tun demek

length(df) #sadece s??tunu sayar.

dim(df) # 5 ve 3 yaz??yo yani 5 sat??r 3 s??tun demek
#??nce sat??r yaz??yor sonra s??tun
d<-dim(df)
d[1] #sat??r
d[2] #s??tun


#####data framelerde sorgu islemleri####
df <- data.frame('A'=rnorm( 100, mean=10,sd=2),
                 'B'=rnorm(100, mean =34, sd=10),
                 'C'=rnorm(100, mean = 45,sd = 15))
View(df)

df$A
df$A < 8
k<-which(df$A<8)

df[k,] #a s??tununda 8den k??????k olanlar?? ve di??er s??tunlar??n onla kesi??en de??erler

df[k,c('B','C')] #A'n??n k??????k oldugu yerlerdeki b ve c s??tunlar?? geldi

mean(df$B) #ortalamay?? getirdi
o <- mean((df$B))
od<- which(df$B < o)

df[od,] #b'nin kendi ortalamas??ndan k??????k de??erleri ve onlarla
#el??ele??n di??er s??tunlar?? getirdik
df[od,c('A','B')] #b'nin kendi ortalamas??ndan k??????k de??erlerle e??lese??
#a ve c s??tunlar??


View(df[od,]) #tablodan a????yor

##### birden fazla kosul subset() ile####  

data() #R i??erisindeki datalar?? g??steriyr
CO2 #R i??erisinde bulunan bi data mesela.
View(CO2) #tablo olarak g??ster de bu


?subset

subset(x, subset, select, drop = FALSE)
#bu sorguda subset k??sm?? hangi alt sorgular?? kullan??caz o nu yazcaz
#select k??sm?? hangi elemanlar?? se??e??cksin demek


#Type Quebec, uptake 30dan b??y??kleri se??elim. CO2 i??in
names(CO2)
subset(CO2, subset= (uptake >30 & Type =='Quebec'))
#belirledi??imiz ko??ullarda t??m tablo geldi. 
#select kullanarak hangi s??tunlar??n gelmesini istedigimizi de secebiliriz

df <- subset(CO2, subset = (Type =='Quebec'& uptake >20), select = c('Type','uptake'))
#uptake ve type k??sm??n?? verdi

View(df)


subset(CO2, subset = (Treatment !='chilled' & conc <= 500), 
       select = c('Type','uptake','conc','Treatment'))
#istredi??imzi sirayla verio. select yazdigim sirayla veriyo

subset(CO2, subset = uptake >20, drop = F)



subset(CO2, subset = (Treatment !='chilled' | conc <= 500))



#### subset ile icice kosullar####


data()

BOD
View(BOD)
names(BOD)
subset(BOD, subset = (Time < 5 & Time > 3)|demand == 19 , select = c(Time)) 
# zaman 5 ile 3 aras??nda olsun veya demand 19 olsun dedik sadece zaman?? getir dedik

CO2
names(CO2)
nrow(CO2)
subset(CO2, subset = (Type==c('Quebec','Mississippi')))
#c() ile birden fazla type e??ittir kullanabildik

df <- subset(CO2, subset = (Type =='Quebec'|Treatment =='nonchilled')&(uptake>30&uptake<35))
df #sat??r say??lar?? di??er tabloda neyse o olarak gelio
row.names(df) <- seq(from = 1, to = nrow(df),by=1)
df #sat??r say??lar??n?? 1den ka?? tane varsa ona kadar yapt??k

##### sat??tlar ve s??tunlar baz??nda ortalama##### 

data()

View(iris)

d1 <- rowMeans(iris[1:4])
d1


iris[length(iris)+1] <- rowMeans(iris[1:4])
iris

names(iris[length(iris)]) <-  
View(iris)

colnames(iris[6]) <-'Means' #??al????mad??


iris <- iris[-6] #sildik o y??zden
iris

iris['Means'] <- rowMeans(iris[1:4]) #ve bu ??ekilde direkt ad vererel ekledim
View(iris)


d2 <-  rowMeans(iris[1:2])
iris['ortalama2'] <-d2
iris

View(iris)

colMeans(iris[1:4], na.rm = T) #s??tun ortalamalar??, na yazanlar?? koymayal??m





##### data frame gruplar baz??nda hesaplamalar#####

View(iris)
iris <- iris[-length(iris)] #2 kere ??al??t??rd??k. son 2 s??tunu sildik 
#ortalama eklemistik
iris

View(iris)
# a??a????da i v ve vc olarak yapt??????m i??lemler 3 farkl?? data.frame olu??turup
#di??er tablodaki de??erleri oraya aktarmakt??. bunu ben ??ylesine yapt??m.
#al????t??rma i??in 
s <- which(iris['Species']=='setosa') #species setosa olanlar??n indislerini ald??m
setosa <- iris[s,] #setosa olanlar??n t??m verileri geldi
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


#sepal.length s??tunlar??n??n ortalamas??n?? ald??k her bir species i??in
meanSetosa <- mean(iris[s,c('Sepal.Length')])
meanVersicolor <- mean(iris[v,c('Sepal.Length')])
meanVirginical <- mean(iris[vc,c('Sepal.Length')])
#ortalamalar
meanSetosa;meanVersicolor;meanVirginical



aggregate(iris[1:4], by= list(iris$Species), FUN = mean)#ortalama
#yukar??da yapt??????m i??lemi direkt yapti
#by k??sm?? liste olmak zorunda

aggregate(iris[1:4], by= list(iris$Species), FUN = sd) #standart sapma


aggregate(iris[1:4], by= list(iris$Species), FUN = sum) #toplam

aggregate(iris[1:4], by= list(iris$Species), FUN = var, na.rm =T) #varyans

#kendi olu??turdugumuz fonksiyonlar?? da ileride ekleyenilirz






##### excelden r'ye data aktarm??a######

#??nerilen budur. ayn??s??n?? sa??dan aktarma k??sm??nda from text ile de yapars??n
read.csv('')

getwd()
setwd('C:/Users/halil ibrahim kaya/Desktop/R ile Veri Bilimi')

list.files()

df <-  read.csv('CSV_singapore.csv',header = T,sep = ";",dec = ".")

View(df)


df2 <-  read.csv('CSV_singapore.csv',header = T,sep = ",",dec = ".")
#virg??lk ile ay??rd??k dedik ama noktal?? virg??l ile ay??rd??k. sa??ma cevap verdi
#kontrol et
View(df2)



getwd()
setwd("C:/Users/halil ibrahim kaya/Desktop")


df2 <-  read.csv('R ile Veri Bilimi/CSV_singapore.csv',header = T,sep = ";",dec = ".")
View(df2)
#konumu dekstop yap??p denedik.bu ??ekilde yazmama??z gerek.

setwd("C:/Users/halil ibrahim kaya/Documents")
getwd()






##### iki data framei merge ile birlestirme#####

#inner join yap??yormu??uz gibi d??????n.
#demografik tablosundaki id ile transactions tablosundaki customer_id ayn??d??r.



?merge
merge_df <- merge(Demografik,Transactions,
      by.x = "ID", by.y = "CUSTOMER_ID")
View(merge_df)

names(Transactions)[2] <-  'ID' #customer_id yazan yeri id ismine veirdik.
#baska bi yontem icin iki parametrenin de ad?? ayn?? olmal??


merge_df2 <-  merge(Demografik,Transactions,
                    by = 'ID')
View(merge_df2)
#iki tabloda da ayn?? de??erleri ta????yan 2 s??tunun ismi de id idi
#burda ikisini birle??tirme i??lemini bu ??ekilde yapt??k. genellikle ayn?? isim kullan??lm??yo ama
#o y??zden ilk y??ntemi unutma  



##### data framelerin ozet bilgileri#####


View(iris)
summary(iris) #ozet bilgileri veriyor. medyan standart sapma, ortalama vsvs.

View(summary(iris))

str(iris) #degiskenleri alip hangi tur onu kontrol ediyo

head(iris) #veri setinin ilk 6 satirini veriyo

iris[10:15,] #10-15 aras?? sat??rlar?? verir  

?head

head(iris, n = 10) #10 satir verir

tail(iris) #sondan 6 verir

tail(iris, n = 10) #son 10 satir






###### data framlerde de??i??ken istatistikleri####



mean(singapore$price) #na geldi
any(is.na(singapore$price)) #na varmis
mean(singapore$price, na.rm = T) #168.9887 veba??n?? verdi. nalar?? yok sayd??k

sd(singapore$price, na.rm = T) #340.0893 verdi

#standart sapma ile ortalama aras??nda ??ok fark var. sapma ??ok mevcut demek.



median(singapore$price,na.rm = T)#124 verdi

#ortalama medyandan buyukse grafik saga carpiktir. yani veriler sol tarafta cogunluktad??r

hist(singapore$price) #??ok fazla veri var ve hepsi 1000 k??sm??nda duruyo
#anca sorun su ki 10000 civar??nda da verilerimiz var
#bu yuzcden tablo sacma olusturuluyor. daha mant??kl?? bi tablo icin
#ucurum farklar?? atmam??z daha iyi olur. daha once de soylemistik.
#boyle durumlarda ayn?? zamanda ortalama yerine medyan kullanmak da sagl??kl?? olabilir


hist(singapore$price[singapore$price <1000]) #1000den kucukleri ver dedik
#ve saga carpik tabloyu aldik. yani veriler solda cok



var(singapore$price, na.rm = T)#115660.7 verdi
sd(singapore$price,na.rm = T)#340...... aradaki fark cpk fazla la

min(singapore$price, na.rm = T)  #0 verdi
max(singapore$price, na.rm = T) #10000 verdi

table(singapore$room_type)
#??st satir degerlerimizi, alt??ndakiler de kac kere tekrar ettigini verir

which(singapore$room_type=='110')

df <-  singapore[-964,] #110u kaldirdik
df

table(df$room_type) #110u kaldirdik ve baktik kalkmis

class(df$room_type)


which(df$room_type == "")
df <- df[-which(df$room_type == "")]
#ici bosmolanlar?? kaldirdik

quantile(df$price, na.rm = T) #ceyreklikleri hesapladi
#cok onemli analiz icin. cunku %75. sayi 124 sayisi ama %100 olan 
#sayi 10000. yani son sayilar cok artmis. bu da ilk basta dediklerimizi
#dogrular niteklikte

quantile(singapore$price, na.rm = T)



##### data frameler uzerinde degisken turu donusumleri#####


class(CSV_singapore$name) #karakter ama faktor sayal??m

CSV_singapore$name <- as.character(CSV_singapore$name) #karaltere cevirdik.

df <- CSV_singapore #ismi uzun diye atad??m direkt
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
table(df$room_type) #gereksizler var say??lar var ayiklama yapmamiz lazim

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

#margin=1 sat??rlar baz??nda yapar
#margin=2 sutunlar baz??nda yapar
apply(iris[1:4], MARGIN = 1, FUN = mean)
#sat??r bazl?? ortalamalar?? verdi.

#saglama yapioz
mean(as.numeric(iris[1,1:4])) #data frame olarak vermisti once karaktere cevirdk
#oy??e yaptik. onemli detay
class(iris[1,1:4]) #data frame


apply(iris[1:4], MARGIN = 1, FUN = sd) #standart sapmalar?? verdi
#saglamaya gerek yok buldu. ya da neyse yapak

sd(as.numeric(iris[1,1:4])) #yapt??k dogruymus.

apply(iris[1:4], MARGIN = 1, FUN = sum) #toplamlar?? ald??k

#margini 2 alal??m
apply(iris[1:4], MARGIN = 2, FUN = sd ) #her bir sutunun adini veriyo
#ve sutunlara ait standart sapmasini verdi

apply(iris[1:4], MARGIN = 2, FUN = max) #max degerleri verdi

#fun kismina istedigimiz fonksiyonu veriyoruz.


#aggregate() fonksiyonunda by = diyip sutun ad?? giriyorduk ve sutunlar?? veriyodu
#burda margin ile hem satilari hem sutunlari alabiliyoz


lapply(iris, FUN = mean)
#burda sonu??lar?? bize liste liste olarak veriyor
#son sutunda da karakterler oldugu icin na verdi meselaaa

#lapply'da margin yok. lapply her bir degisken uzerinde islem yapar. yan?? sutunlar?? kullan??r
#satirlari kullanmaz


l <- list('a'=c(23,24,55,67,89,90),
          'b'=c(13,45,65,23,45),
          'c'=c(11,22,34,23,12))
l

lapply(l, FUN = mean) #liste liste ortalamalar??n?? ald??.
# a icin verdi , b icin verdi, c icin verdi

lapply(l, FUN = sd)
lapply(l, FUN = max)

#data frame kullan??yorsak apply, liste kullaniyorsak lapply kullanalim.
#


##### attach detach ve with fonksiyonlar??#####

df <- singapore #ad?? uzun

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


#dezavantaji isimler birbiriyle cakisabilir. iki ayr?? tabloda iki name 
#degiskeni oldugunu varsayal??m. bu degiskenler birbiriyle carpisir.
#tek data.frame ile calisirken attach cok iyidir. birden fazla varsa isimler
#karisir. hangi degisken nerde karistiririz.

detach(df) #attach komutunu kapat??yor.
host_id #hata verdi
#kac kere attach ettiysen o kadar detach etmen gerek

attach(df)
attach(df)
host_id  
detach(df)  
host_id  #iki kere attach ettik diye calisti. 2 kere de detach etmemiz gerekiyor
detach(df)
host_id #calismadi


with(df, print(host_name)) #df icerisindeki elemanlar?? () icerisinde df yazmadan
#kullanmak icin

with(df, {
        x <- mean(price, na.rm = T) 
        y <- x-5
        print(y)
}) #163.9887 degerini ald??k. ayr?? ayr?? butun satirlari calistirdik bole


with(df, print(price)) #mesela
#baz?? yerlerde kullan??isli olabiilyorrrrrrr.










##### odevvv####

#Ekte size iletmi?? oldu??um veri dosyas??n?? l??tfen indiriniz. 
#Bu veri dosyas?? farkl?? tarihlerdeki Kad??nlar D??nya Kupas?? kar????la??ma sonu??lar??n?? i??ermektedir. 
#Bu ??dev kapsam??nda bu veri dosyas?? ??zerinde i??lem yapaca????z. L??tfen ??dev s??ras??nda a??a????daki talimatlar?? takip ediniz.

#Veri dosyas??n?? read.csv()  fonksiyonunu kullanarak R'a aktar??n??z ve bu veriye df ismini veriniz.

#Veri dosyas??nda 1980 Aral??k 1 ve 2019 Aral??k 31 tarihleri aras??nda kalan kay??tlarda. 
#Home Team'i (ev sahibi) Hong Kong ve Away Team'i (deplasman tak??m??) Philippines olan tak??mlar??n
#sadece  date, home_score, away_score, home_team ve away_team de??erlerini bir data frame olarak elde ediniz.

#Not:  Tarih verilerini as.Date fonksiyonunu kullanarak R'da tan??ml?? tarih format??na d??n????t??rmeyi unutmay??n??z.



df <- read.csv('WomenFootballResults.csv' , header = T , sep = ",")

subset(df, subset = (date>as.Date('1980-08-01') & date<as.Date('2019-12-31')& home_team =='Hong Kong' & away_team =='Philippines'),
       select = c('date','home_score','away_score','home_team','away_team'))






