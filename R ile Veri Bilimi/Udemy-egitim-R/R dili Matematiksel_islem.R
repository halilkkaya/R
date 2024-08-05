############## matematiksel islemler############

x <- 5
y <- 15


y-x # 10 verir, normal diller gibi -+*/ islemleri yapiliyor
y/x #3
y*x #75
y+x #20

y**2 # 225 karesini aliyor
y**3 #3375 kupunu aliyor

z <- 12**2


sqrt(z) # 12 karekok alir

 
t<- -3*6 #eskiye duyarli
t 

t**2 #karesini aldim arti oldu 324
t/5  #-3.6 verdi. nokta kullaniliyo ondalik iiin


############### vektorlerde matematiksel islemler###########

x <- c(12,54,67,43,35)
x
x+5 #her bi degerine 5 atar
x-2 #her degerinden 2 cikarir
x**2 #hepsinin karesini alir
x*5 #hepsi 5 ile carpar
x/5 #5e boler hepsini
x**3 #hepsinin kupunu alir

y <- x**2 #yye xin karesinin alinmis hali atanir
x #x ayni kalir
y#yeni degerler ydedir

z <- x + 15
z
x
x + z #ayni indisteki sayilari birbiri ile toplar
x*z # ayni indisleri carpar 
x/z#boler

x1 <- c(13,24,35,56)
x2 <- c(12,23)

x1+x2 #sirasiyla 1,2,1,2 seklinde toplama yapmiss. yani aciklamak gerekirse
# 13+12,24+23,35+12,56+23 seklinde toplar. 
# x1-1 ile x2-1, x1-2 ile x2-2, x1-3 ile x2-1, x1-4 ile x2-2 seklinde olur

x1*x2 #ustteki gibi yaptui.

x3 <- c(13,24,35,56)
x4 <- c(12,23,24)
x3+x4 #ikisi arasinda kat farki olmadiggi icin uyari veriyor.
#ama yine de islemi gerceklestiriyor.
#yine ustteki mantikla dusununce sayica cok olana sayica az olanin baslangic 
#degerleri geliyor.
#diger islemler de bu sekilde gerceklesiyor.



####### vektorlerin uzunluklarini elde etme##########
x <- c(1,2,3,4,5,6,7)

length(x) #7 sonucu donuyor

x1 <- c(11,22,33,551,5,15,15,4151,151,165,51,54)

length(x1) #12 elemanliymis

x2 <- c('a','b','d','b',
        'a','b','d','b',
        'a','b','d','b',
        'a','b','d','b',
        'a','b','d','b',
        'a','b','d','b',
        'a','b','d','b',
        'a','b','d','b',
        'a','b','d','b')
length(x2) #36 tane eklemisiz 
x2[length(x2)] #son eleman, secer

len <- length(x2)
x2[len-1] #sondan bir conceki eleman


################ vektcorlerin min-max degerleri############

x <- c(2,35,32,62,51,95,61,5,99,159)
x

#min ve max degerleri verir
min(x)
max(x)

y <- c(-1,-10,0,6,-95,97)
min(y)
max(y)


min(y)+max(y)

min(y)+max(y)/min(y) #islem oncelioi vardir. once nolme sonra toplama
#parantez koyarsan once parantez ici yapar


################# vektorlerde degerin hangi indiste bulumasi############

x <- c(12,34,56,45,78,54)
x

y = 5
y == 5 #true dondurur. y 5e esit mi denir

x == 5 #eger 5 yoksa indis sayisi kadar false dondurur
#6 indisli bi vektor bu 6 tane false dondurdu

x==12 # ilki true gerisi false geldi.

which(x==5) #integer 0 diyo yani iceride 5 yok
which(x==56) #3. deger oldugunu donduruyor

which.max(x) #5. deger max iye donduruyor
which.min((x)) #1. deger min diye donduruyor

t <- c(5,15,35,64,5)
which(t==5) # 1 ve 5te var dedi. iki tane var cunku

bes = which(t==5)
t[bes] #5 5 seklinde bi vektor geliyo
t(bes) #1 ve 5 donduruyo. icerisinde 5 bulunan indis sayilariydi bunlar.

t[t==5] #5e esit olanlari verir yine

class(t==5) #logical turu



############## vektorlerde buyuk/kucuk isaretleri##############

x <- c(23,45,67,89,23,14,25)
x

x==45

x>45 #45den buyuk olanlar icin true kucuk ve esit olanlar icin false donduruyor
x<45#45den kucuk olanlar icin true buyuk ve esit olanlar icin false donduruyor

x>=23 #23ten buyuk ve esitler icin true kucuklere false
x<=45 #kuuucuk ve esit true buyuk false

x!=23 #esit degilse true, esitse false. esit degil mi diye sorduk cunku

x[x>45] #sadece 45den buyukleri verir

x[x>=45] #buyuk ve esitleri verir

x[x!=45] #45 olmayanlari getir

############# buyuk/kucuk elamanlarin indislerini bulma #############

x <- c(10,35,64,9,25,36)
x


which(x==64) 
which(x>35) # 3 ve 6. sira buyuk diyo

on<-which(x>10)
x[on] #ondan buyuk sayilari verir

x[which(x==65)] #bos veriyo

kucuktur <-  which(x<25)

x[kucuktur] #25den kucukleri verir


########### vektorlerde karakterlerde buyuk/kucuk kullanumui###########

x <- c('A','B','C','D','E')
x

'Bb' =='BB' #false dondurur

'a'=='a' #true dondurur

x == 'B' #sirasiyla indislere false true false dalse... yazar

which(x=='A') #ilk indis A diyo

which(x>'A') #alfabetik siraya gore gider
#a=1,b=2,c=3 seklinde dooonelim. bu kod a disindakileri verir
which(x>'D') #sadece E harfini verdii
which(x<'D') #a b c verdi
which(x<='D') #ek olarak dyi de verdi


'Ad' < 'Ar' # farkli olan yere kadar ayni gider. ilk farkli harften hangisi 
#alfebetik sayimolarak ondeyse o kucuktur


'A' < 'a' #kucukler daha buyuk sayiliyormus, simdi ogrendik.
#videolarda buyukler daha buyuk sayiliyor. burda kucukler.
#icin icerisinde bir seyler var ama anlamadim
'Ab' >'AB' #true dondu
'aB' > 'Ab' #true dondu
'Ab' < 'AB' #false dondu



x<- c('A','B','C','D','A','F','Y')

x > 'Y' #false dondu hepsi
x > 'X' #sadece y true dundu

which(x>'D') #6. ve 7. indis Dden buyuk demek
which(x>'d') #yok dedi
which(x<'d') #hepsini verdi
x [x>'X'] #sadece Y yazdu
x[which(x=='A')] #iki tane verdi. 2 tane A var cunku


'Sergen' <'Ahmet' #false
'Sergen'<'Serkan' #true
'SeRgen'<'Serkan' #true dondurdu. 

'A'>'a' #false. CHATGpt beni douruladu


########### ardiiik degerleden olusan vektor olusturma############

1:23 #1den 23e gider

-2:5 #-2den baslar 5e kadar gider. 0 da dahildir ixine

seq(10) #1den 10a kadar yazar
seq(2,15) #2den 15e kadar yazar
seq(-10) #1deb baslar -10a kadar 1,0,-1,-2... seklinde gider
 
x <- seq(-4,5)
x
 seq(from= 0, to=10)
 
 df <- data.frame(a=c(1,2,3,4,5), b=c(34,35,36,25,12))
df

df <-df[-3,] #3. satiri sildik
df

uz<-length(df$a) #satir sayisini aldik
uz

row.names(df) #satir isimlerini verir.
row.names(df) <- seq(from = 1, to=uz) #satir isimlerini degistik
#3. satiri silince 1,2,4,5 kalmisti satir isimleri
#onlari 1,2,3,4 yaptik burda
df

######## belirli bi deger ile artan ardisik vektorler##############

seq(10) #1den 10a kadar

?seq


seq(from=2,to = 20,by =3) #2den basla 20ye kadar 3er 3er art

seq(from=1, to=20, by=2) #1den 20ye kadar 2ser art.

y<- seq(from=0.30,to=10.79,by=0.001) #baya bi var 
length(y) #10491 sayi varmis. 
y[length(y)] #son indisi kac demek

t<- seq(from = 1,to=10,length.out=30) #1 ile basla 10a kadar ama arada 30 sayi olsun demek bu. 
length(t) #30 tane mi diye kontrol ettik

seq(from=1,to=10,by=20)#tek deger veriri
seq(from =1,to=20,by=4, length.out=20) #20 tane eleman olusturamaz. o yuzden hata
seq(from=1,to=20,by=4,length.out=3)#3den daha fazla eleman olusturuyo o yuzden hata
#seq fonskiyonunda by ile length.out kismi ayni anda olamaz. hep hata verir
seq(from =0,to=20,by=4,length.out=6)# normalde 6 deger veriyo 0dan baslayip 20ye kadar
#biz de 6 deger ver dedik ama yine hata. ayng anda kullanglmaz


################### vektor degerlerinin rastgele siralanmasi###########
seq(from=1,to=10,by=2)#belirli artma bu 
x <- seq(from =100,to=300,by=10)
x
sample(x) #x vektorunu karisik olarak verir
sample(x,replace = TRUE) #yine karisik ama deger tekrarlama da var
sample(x)
sample(x)#her seferinde farkli veriyo

y <- sample(x)
y#farkli olan degerler buna ataniyo. kalici halde duruyo ve her seferi,nde degismiyo artsk


############### vektsrlerde rastgele ornek seimi ve set.seed()##########


x <- seq(from=10,to=200, length.out=50)
x
length(x)#kontrol ettik 50 eleman var harbi

sample(x) #rastgele siralama

y <-sample(x,size=1)#rastgele 1 eleman ver, her seferinde farkli verir
y
which(x==y) #rastgele atadigimiz sayinin hangi indiste oldugunu hulduk

which(x==sample(x,size = 1)) #her seferinde farkli sayi atadigi icin
#farkli indisler buluyor

set.seed(165) #surekli degismesin istiyorsak bunla beraber calistirmak gerek.
sample(x,size = 1)#once setseed sonra buna basinca 185 veriyo sadece
#sadece sample basinca farklo degerler veriyo

set.seed(175) #175'in anlami su. her 175 yazdigimizda belirli bi sayi vericek
#burdaki islemce 137.9592 sayisini veriyor mesela. baska ornek yapalim
sample(x,size=1)

set.seed(2)
sample(x,size=1) #bu ornekte de 2'sayisini verdigimizde hep 87.55102 verecek

set.seed(2)
sample(x,size = 1)#yeni kod yazdim mesela 2 verip yine 87 verdi

sample(x,size = 20)# 20 tane veriyo karisik
sample(x,size = 60)#60 eleman yok, hata verir amaaa
sample(x,size = 60,replace = TRUE)#dersek ayni elemanlari yeniden yazdigi icin verir

set.seed(165) 
which(x==sample(x,size = 1)) #setseed(165) olan degerin xdeki indisi kac onu buluyoz 




############# tekrar eden degerlerden olusan vektor ve seriler####

rep(4,14) #14 kere 4 yazar
r <- rep(4,14)
length(r) #kontrol ettik 14 tane var 

x <- c(34,23,45,67)
rep(x,2) #2 kere deserleri sirasiyla yazar

rep(sample(x),2) # verileri karistirip 2 kere tekrarlatir. karistirdigi sekli
#alarak tekrarliyo

rep(x,each=2) #her biri 2 kere tekrar etsin dedik. 34,34,23,23... eisklinde
rep(x,3,each=2) #her bi deisgeri yanyana 2 kere toplam 3 kere tekrar et
#34 34 23 23 45 45 67 67 sekli 3 kere doncek demek bu
?rep

sample(rep(x,2)) #her birini 2 kere ama karistirarak veriyo
sample(x,4,replace = TRUE) #tekrar acik 4 sayi ver dedik bu da onceki konu
#karislastirmak icin koydum

####### vektorlerin buyuktrn kucuge/kucukten b. siaralanmasi #########
7
x<-c(12,36,54,69,32,65,15,02)

sort(x)# varsayilan olarak kuvukten buyuge siralar

sort(x, decreasing = TRUE)#buyukten kuuuue oldu

sort(x,T)#yine buyukten kuuuue suralama

y <- c('Osman','Ahmet','Mehmet','Berk')
y

sort(y) #alfabetik olarak kuuuk olandan basladi A  hmet ile yani degen artarak gibi
sort(y,T) #azalarak oluyo Osman ile basladi

na <- c(12,35,NA,25,265,NA,NA)
sort(na) #NA degerlerini hesaba katmyor.

sort(na, T, na.last = NA) #NA degerlerini almaz
sort(na, T, na.last = T) #NA degerlerini al,r
sort(na, T, na.last = F) #NA degerlerini alir, na.last F oldugu icin en basa atar NAlars
sort(na, F, na.last = T) #NA degerlerini alir NAlar wn sonda na.last T cunku

sort(na,decreasing = T, na.last = T, index.return =T)
#$x [1] 265  35  25  12  NA  NA  | $ix [1] 4 2 3 1 5 6
#seklinde verir cevabi. yani ilk yerde 265 vermis o 4. indiste yer aliyo demek
#35 sayisi 2. indiste NAlar ise 5 ve 6. indiste diyor. vektoru en basta
#olusturduumuz sirayi veriyo yani

siralama <- sort(na,decreasing = T, na.last = T, index.return =T)
siralama #atama yaptik

class(siralama) #list verdi

siralama[['x']] #degerleri veriyor
siralama[['ix']] #indisleri veriyor


siralama[['x']][1] #siralamaya gore ilk sayiyi aldim
siralama[['ix']][1] # siralamaya gore ilk sayinin indisini aldim











############# soru ######
#20 ile 300 arasinda ardisik olarak artan ve uzunlugu 200 olan bir vektor olusturunuz.

#Olusturdugunuz vektor uzerin uzunlugunu kontrol ediniz.

#Olusturulan vektorden 50 adet rastgele ornek aliniz ve bu vektoru yeni bir nesneye kaydediniz.

#Rastgele alinan orneklerden, 100'den buyuk olan elemanlari ve vektorde bulunduklari indeksleri elde ederek bir nesne icerisinde kaydediniz.



x<-seq(from =20, to=300,length.out=200)
length(x)
y<-sample(x,50)
y
y1<- y[y>100]
y1
z<-which(y>100)
z

