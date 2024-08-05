##### fonksiyonel porgramlama#####
# print cat ve paste fonksiyonlari

print(5+6)
x <- 5
print(x)
y <-  10
print(x,y) #y'yi yazdirmiyor. sadece x veriyor

cat(x,y) #ikisini birden yazdirir

cat(x,' -> ',y) # 5  ->  10 seklinde cikti verir 
 
t <-cat(x,' -> ',y) #5  ->  10 seklinde cikti verir 
t #null verir. cat sadece yazdirmak icindir. atama yapilmaz


cat(x,'esittir:', y,'\n' ,'alt satirdayim') 
#\n ile alt satira gectik. c++ gibi

paste(x,y,'degerleri') #vektor olarak verdi [1] "5 10 degerleri" seklinde

t <- paste(x,y,'degerleri') #atama yapabildik.
t 

class(t) #karakter

paste0(x,y,'DDDD') #bosluk olmadan verdiiii
# paste fonksiyonunda \n calismaz

###### if else #########

if (5==5) {
  print('Dogru')
}

if (5==4) {
  print('Dogru')
} #bir sey yazdirmadi


if (5==4) {
  print('Dogru')
}else {
  print('Yanlis')
} #yanlis cevabini verdi



x <- seq(from = 10, to =25)
x

if(TRUE) {
  print('var')
  
}else{
  print('yok')
} #icerisi true ise if degilse else calisir.


if (mean(x)>50) {
  cat('sayilarin toplami: ',sum(x), ', ortalamasi: ',mean(x))
}else{
  print('bosver')
} #olesine alistirma

y <-  seq(10,150)
y
x <- seq(10,25)
x
if (sum(x)>150 & mean(x)>20) {
  print("selam")
}else{
  print('meraba')
}

y <- rnorm(30, mean = 10, sd = 2)
y
median(y)#9,37
mean(y) #9.45

if (median(y)>mean(y)) {
  print('sola carpik')
}else if(as.integer(median(y))==as.integer(mean(y))){
  print('dengeli')
}else{
  print('saga carpik')
}
#dengeli sonucunu aldik

hist(y) #harbiden dengeli


if (mean(iris$Sepal.Length)>5 & median(iris$Sepal.Width)>4) {
  print('kosullar saglandi')
}else{
  print('kosullar saglanmadi')
} #saglanmamis

if (mean(iris$Sepal.Length)>5 | median(iris$Sepal.Width)>4) {
  print('kosullar saglandi')
}else{
  print('kosullar saglanmadi')
} #veya kullandik sagladi


##### for dongusu ####

y <- seq(1,25)
for (i in 1:10) {
  print(y[i])
} #1den 10a kadar olan indislerin icerisini yazdirdik

x <-  rnorm(12, mean = 18, sd = 3)
x


for (k in 1:10) {
  print(x[k])
} #ilk 10 nesneyi yazd??rd??k
x


for (i in 1:length(x)) {
  print(x[i])
} #1den xin say??s?? kadar eleman yazdir


for (i in length(x):1) {
  print(x[i])
}#tersten yazmaya basladi

y <- c('a','b','c','d')
pas <- ''
for (i in 1:length(y)) {
  
  pas <- paste(pas, '-',y[i])
  print(pas)
} #surekli pasa ekler
print(pas) #son halini kontrol ettik

##### for in icinde if else####


#sayiarin tek cift olduguna gore atayan 

sayi <- seq(10,100)
cift <- ''
tek <- ''
for (i in 1:length(sayi)) {
  if (sayi[i]%%2==0) {
    cift <- paste(cift,'-',sayi[i])
    
  }else if (sayi[i]%%2==1) {
    tek <- paste(tek,'/',sayi[i])
    
  }
}

cift
tek

##### while dongusu####

x <- seq(15,30)
x=10
while (x > 5) {
  print('calisiyor')
  return()
}
# sonuna Sys.sleep(1) yazsak her saniye cevaplari verir
# return yazdigimizda da direkt bitiriyor.
x = 5
l = 1
while (x<10) {
  cat(l, '. dongu gerceklesti', '\n')  
  l = l +1
  Sys.sleep(3)
} #sonsuza kadar calisir
x = 5
l = 1
while (x<10) {
  x = x+1
  cat(l, '. dongu gerceklesti', '\n')  
  l = l +1
  Sys.sleep(3)
}  #x'i surekli arttirarak dongunun son bulmasini sagladik

l=1
x=5
while (x<10) {
cat(l, '. dongu gerceklesti', '\n')  
  if (l==6) {
  x = 11     
  }
  l = l +1
  Sys.sleep(3)
} #burda l 6 olana kadar calisir sonra durur. cunku x=11 dedik if icinde.
#while komutumuz x 10dan kucukken calis demekti





##### icice donguler####

x=1
y=1
for (i in 1:10) {
  
x = x+i  
 for (k in 1:10) {
   y = y+k
 
   }    
}
x # 56
y # 551 
#ilk for dongusu her bi kere calistiginda ikinci for dongusu yazdigimiz sayi
# kadar calisiyor. dikkat etmemiz gereken noktalardan biridir bu.


View(iris)
for (i in 1:ncol(iris)) {  #sayi iceren sutun no ald??k
  
  print(names(iris[i]))
  
  for (k in 1:length(iris$Sepal.Length)) { #sat??r say??s??
   print(iris[k,i])
  }
  
} #sirasiyla hepsini verdi. ama karisik verdi. amacim zaten alt alta verdirmekti.

for (i in 1:nrow(iris)) {
  
  for (k in 1:ncol(iris)) {
    
    text <- paste(names(iris[k]), 'degiskeninin ',i,'. satir degeri => ',
                  iris[i,k],' esittir')
    print(text)
    Sys.sleep(1)
  }  
} #her bir sutunu satir degerleriyle beraber saniyede 1 veri seklinde listeledik

#while icinde for
while (TRUE) {
  
  print('while')
  for (i in 1:5) {
    print(i)
    
    Sys.sleep(1)
  }
  
  Sys.sleep(1)
  if (i==5) {
    break
  } #bunu ekledik sonsuzlugu kesti. yoksa sonsuza kadar gider
  
  
} #sonsuza kadar gidiyor





##### for dongusu icinde vektor olusturma####


x=''

for (i in 1:10) {
  
x[i] = i  
  
}
x #10a kadar olani ekledi

  
#for icinde if
min(iris$Petal.Length) #1
max(iris$Petal.Length) #6.9
mean(iris$Petal.Length) #3.7



for (i in 1:nrow(iris)) {
    if (iris$Petal.Length[i]<3) {
      text <- paste(names(iris[3]),' sutununun ' ,i,'. degeri 3ten kucuktur')
      print(text)
    }else if (iris$Petal.Length[i]>3) {
      text1 <- paste(names(iris[3]),' sutununun ' ,i,'. degeri 3ten buyuktur')
      print(text1)
    }else {
      text2 <- paste(names(iris[3]),' sutununun ' ,i,'. degeri 3tur')
      print(text2)
    }
} #sayilarin kontrolu

x <- character(length(iris$Petal.Length)) #bos sekilde vektor olusturdu

for (i in 1:length(x)) {
  if (iris$Petal.Length[i] >= 3) {
      x[i] <- 'buyuktur'
    
  }else {
    x[i] <- 'kucuktur'
  }
}
x #buyuktur kucuktur seklinde icine eleman atadikk

iris['var2'] <- x
iris #yeni sutun ekleyip oraya attik.



###### fonksiyonlar#####

#en onemli konu#

toplama <- function(x,y){
  
  result = x+y
  return(result)
}

toplama(5,10) #olusturduk ve 15 verdi

a <- seq(1,10)

f <- function(x){
  topla = 0
  for (i in 1:length(x)) {
    topla = topla + x[i]
  }
    return(topla) 
}
f(a) #1den 10a kadar olan sayilariverdi

b <- rnorm(20, mean = 15, sd = 3)
b
sum(b) #293.6969
f(b) #293.6969  #kendi sum fonksiyonumuzu yazdikkk


c <- rnorm(20, mean = 25, sd = 4)

df <-  data.frame(b,c)
df

myfun <- function(x,y){
  
  for (i in 1:length(x)) {
    
    t=x[i]+y[i]
    b=x[i]/y[i]
    z=x[i]*y[i]
    res = (t+b)/z
    print(res)
  }
}
myfun(b,c) #b ve c vektorlerindeki sayilarimizi her bir indisleri
#iceriye girdigimiz ozel islemler yaptik sonucu yazdirdik

##### kullanici girdisi ile fonk deger gonderme#####

f <- function(){
  x = readline(prompt = "Ilk sayiyi girini:")
  y = readline(prompt = "Ikinci sayiyi girini:")
  x <-  as.numeric(x)
  y <-  as.numeric(y)
  d = x+y
  return(d)
  
}

f() #readline icerisi hep character aldigi icin as.numeric ile cevirmek zorundayiz

f1 <- function(){
  x <-  readline(prompt = "ilk sayi: ")
  y <-  readline(prompt = "ikinci sayi: ")
  z <-  readline(prompt = "ucuncu sayi: ")
  x <- as.numeric(x)
  y <- as.numeric(y)
  z <- as.numeric(z)
  
  ort = (x+y+z)/3
  res =  paste('3 sayinin ortalamasi: ',ort,'a esittir')
  
  return(res)
}
f1()
10
15
35
#girdileri boyle de girebilriiz. 20 verdi
##### fonk icinde if else####

f1 <- function(){
  
  x <- readline(prompt = 'bir sayi giriniz')
  x <-  as.numeric(x)
  if (x%%2==0) {
    print('girdigin sayi cifttir')
  }else if (x%%2==1) {
    print('girdigin sayi tektir')
  }
  
  
}

f1()
20
f1()
31
#bu sekilde if else kullandik. bunu kendim yaptim. simdi hocaninkileri yazcam


f2 <-  function() {
  x <- readline(prompt = 'sayi sole: ')
  x <- as.numeric(x)
  if (x%%2==0) {
    return(x**2) #xin karesi
    
  }else if (x%%3==0) {
    return(x**3) #xin kupu
    
  }else{
    return(0) #sifir ver
  }
}
f2()
45
12
11



f3 <-  function(x,y){
    if (x>y) {
      r = x+y
    }else{
      r=0
    }
    if (x<10) {
      r1 = x-y
    }else{
      r1=0
    }
    return(r-r1)
}
f3(9,5) #10
f3(10,5) #15
f3(8,11)#3 
f3(12,13)#0


f4 <-  function(x,y){
  r=0
  r1=0
  if (x>y) {
    r = x+y
  }
  if (x<10) {
    r1 = x-y
  }
  return(r-r1)
}
## diger cozumdeki elseleri kaldirip boyle cozum bulduk
f4(5,4) #calisti 8


##### fonk icinde for####

standart_S <-  function(x, population = TRUE){
  
  uzunluk <-  length(x)
  ortalama <- mean(x)
  fark_vec = numeric(uzunluk) #belirlenen sayi kadar vector olusturur. hepsi sifir olur icinin
  
  for (i in 1:uzunluk) {
    fark_vec[i] <- (x[i]-ortalama)**2
  }
  toplam_fark =sum(fark_vec)
  
  if (population == TRUE) {
    standart_ort = toplam_fark/uzunluk
  }else{
    standart_ort = toplam_fark/(uzunluk-1)
  }
  
  std= sqrt(standart_ort)
  return(std)
  
} #standart sapma fonksiyonu olusturduk.
 
a <- rnorm(30, mean = 15, sd = 12)

sd(a) #11.03327  r dirrekt n-1 olarak veriyo
standart_S(a) #10.84782
standart_S(a,FALSE) #11.03327


##### while dongusunun for gibi kullanilmasi#####

View(iris)


f1 <- function(data){ #irise gore yaptik
  baslangic <- 1 #ilk satir icin
  son <-  nrow(data) #son satir icin
  ortalama <- mean(data[,3], na.rm = TRUE) #o sutunun ortalama degerini aldik. if elsede bunu kullancaz cunku
  
  result <-  list() #eklemek istedigimiz listeyi olusturduk
  result[['k']] <- numeric() #numeric olarak 2 vektor olusturdujk
  result[['b']] <-  numeric()
  
  while (TRUE) {
    if (data[baslangic,3]<ortalama) { #3. 1. satir ortalamadan kucukse yap
      d <- (data[baslangic,1] * data[baslangic,2]) / data[baslangic,4]
      result[['k']] <- append(result[['k']],d)  #virgulden sonraki sonucu listeye ekleme fonk
      
    }else{ #degilse veya esitse yap
      
      d <- data[baslangic,1] * data[baslangic,2] * data[baslangic,4]
      result[['b']] <-  append(result[['b']],d)
    }
    
    baslangic = baslangic +1 # for gibi her dongude bir arttirdik
    if (baslangic == son) { #son satir sayisiyla esit oldugunda bitir dedik.
      break
    }
  }
  return(result)
    
    
} #baslangic ve bitisi belirleyerek bi for dongusu gibi kullandirdik.
# onemli bi kod en azindan bilmek yaraticiligini etkiler


f1(iris)

result <-  f1(iris)
result

class(result) #list

##### fonksiyonlarin icinde tanimlanan nesneler####

f1 <-  function(){
  x=5
  y=10
  z=12
  
  return(x+y+z)
}
f1() #27 verdi

x #bulunmadi cunku fonk icinde tanimli normal tanimli degil.

f2 <-  function(){
  x <<- 5
  y <<- 10
  z <<- 12
  
  return(x+y+z)
}
f2()#27 verdi
x #verdi. disarida da tanimli olmasi icin <<- atama operatorunu kullanmak gerekli
#onemli bi bilgidir bu. ama genellikle kullanmak sikinti
#cok gerek olmadikca kullanmamaya dikkat et


f3 <-  function(x,y){
  
return(x+y)  
  
}
f3(4,5) #9 verdi. normal

f3(x=4,y=5) #9 verdi normal

f3(x=6,t=3)  #hata verdi

f3(x <- 4, y <- 3) #bu da oldu
# <-  ile = arasinda fark yoktur. ama genellikle fonk parametre belirlerken 
#esittir kullan. esittir kullan hata yeme ;)


x=15
f4 <-  function(){
    x= 20
    y=5
  return(x+y)
}

f4() #25
x #15 verdi fonk icinde degisikli etkilemedi


f5 <- function(){
  x <<- 20
  y <<- 24
  return(x+y)
}
f5() #44
x#20 oldu. fonk calistirdiginda deger degisiyo <<- sayesinde






##### odev ####
 #soru 1

#icerisine bir adet vektor argumani (paremetre) alan bir fonksiyon olusturunuz. 
#Bu fonksiyon verilen vektorun tum elamanlarinin mutlak degerini abs() 
#fonskiyonunu kullanarak alimali. Ardindan mutlak degeri alinmi degerlerin 
#karekokunu almali ve sonuc olarak cikan tum elemanlari 10 ile carpip olusan 
#degerlerin toplamini almali. Olusturdugunuz fonksiyonu rastgele normal dagilan 
#bir degisken olusturarak test ediniz. Yazdiginiz tum script'i cevap olarak belirtiniz. 


f1 <-  function(x){
  
  mutlak <-  numeric(length = length(x))
  karekok <- numeric(length = length(x))
  carp <- numeric(length = length(x))
  for (i in 1:length(x)) {
    
    mutlak[i] <- abs(x[i]) 
    karekok[i] <- sqrt(x[i])
    carp[i] <- x[i]*10
    topla <- sum(carp)
    
  }
  return(topla)
}

x <- rnorm(10,mean = 10,sd=1)
f1(x)


#soru 2

f2 <- function(y){
  
  yliste <-  list()
 
   for(i in 1:length(y)){
    
    yvektor <- c()
    
    for(k in 1:length(y[[i]])){
      
      if(y[[i]][k] > 5){
        
        yvektor[k] <- "buyuktur" 
      }else{
        yvektor[k] <- "kucuktur"
      }
    }
    
    yliste[[i]] <- yvektor
  }
  
  return(yliste)
}



a <- c(10,5,23,4,12,3,2,12)
b <- c(45,3,78,1,55,5,7,98)
l <- list(a,b)

f2(l)


fonksiyon2 <- function(liste){
  
  # Bu liste yeni bir listedir for dongusunde yeni elemanlar atanacaktir.
  # Al??d???? elemanlar fonksiyona verilen listenin vektor elemanlarinin 
  # buyuktur kucukktur hale cevrilmis hali olacaktir
  # ONEMLI: bu listenin elemanlari vektor olactir
  yeniListe <- list()
  for(i in 1:length(liste)){
    
    # Bu kisimda yeni bir vektor olusturulmasi gerek. 
    # cunku bu vektor listenin icerisindeki vektorde bulunan
    # degerlere gore buyuktur kucuk degerlerini alacak
    # ve son olarak listeye eklenecektir.  
    # Fonksiyonu verilen listenin diger elemanina gectiginde 
    # tekrar bos olarak olusturulacak ve yeni degerlerini alacaktir.
    yeniVektor <- c() # Her liste dongusunde (i) yeni bos vektor olusturur.
    
    for(k in 1:length(liste[[i]])){
      
      ## Vektordeki elemanlarin kontrolu
      if(liste[[i]][k] > 5){
        
        # Yeni vektore buyuktur atamasi
        yeniVektor[k] <- "buyuktur" 
      }else{
        # Yeni vektore kucuktur atamasi
        yeniVektor[k] <- "Kucuktur"
      }
    }
    
    # Yeni vektorun yeni listenin gecerli indeksine kaydedilmesi
    yeniListe[[i]] <- yeniVektor
  }
  
  # Sonuc olarak yeni listenin donulmesi
  return(yeniListe)
}
fonksiyon2(l)













