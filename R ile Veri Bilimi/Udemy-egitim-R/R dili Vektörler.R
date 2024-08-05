
############### vektorler ############
y = c(1,2,3,4,5) # y icerisine c fonksiyonu ile (1,2,3,4) ekledik
y

z <- c(10,20,30,40) #okun yonu ile atama yapiyoruz.
c(10,20,30,40) -> a

t <- c("A",'D','F',"D")
t

e <- c("A","B",2,3,4,"D") # bu sekilde kullansak da sayilari karakter olarak algiliyor
e

c(34,45,,67) # argiman 3 bos hatasi veriyor.


####VEKTORLERDE ELEMAN SEciMi. BAsLARKEN YUKARIDAKiLERi SIFIRLADIM.####
 x <- c('a','b','c')
x[1] #ilk elemanini verir
x[2] #ikinci eleman secimi
x[3]#ucuncu eleman secimi


y <- c(1,2,3,4,5,6,7,8,9,10)

y[1:3] #birinci ve ucuncu elemanlar ve arasi
y[4:9]# dorduncu ile dokuzuncu elemanlar ve arasi

y[c(5,6,7,8)] #ynin icerisinden 5,6,7,8. elemanlari aliyor


t<-c(11,12,13,29,25,29,30,31,32)

t[c(4,8,9)]# tnin icerisinden 4,8,9. elemanlari aliyor

t[c(4,8,15)] #tnin icerisindeki elemanlari getiriyo ancak 15. eleman yok
#15. eleman yerine NA yazar yani eleman yok demek


t[c(1:3,8)]# 1 ile uc arasini getir ve 8. elemani da getir demek
t[c(2:5,7,8:10)]#2 ile 5. eleman arasi, 7. eleman, 8 ile 10. eleman arasi demek.


#### VEKTiRLERDEN ELEMAN cIKARTMA ####

x<- c(12,13,14,15,16)
#eleman secimi
x[1]
x[5]

#eleman c,kartma islemi
x[-1] #ilk eleman disindakileri veriyor
x[-5] #5. eleman disindakileri veriyor. bunlar kalici degil. sadece sonuc veriyor.

#kalici hale getirmek iiin tekrar atanmali
x <- x[-1]
x #kalici olarak ilk indis cikar

x <- x[-5]
x #5 eleman yok artik. 4 eleman var o uyuzfen burda bir sey silmeyecek.

x <- x[-4]
x #4 elemana dt icin son indis 4 oldu. son sayiyi sildik.


x<- c(12,13,14,15,16)

x <- x[-1:-3]
x # 1 ile 3 indisleri ve arasindakiler siler

x <- x[-c(1:3)]
x #yine usttekiyle ayni. 1 ile 3 indisleri ve arasindakiler siler

y <- x[-1]
y #x'in ilk elemaninin cikarilmis halki y'ye atanir.

x;y #ikisini birde calistir.
# tek satirda birden fazla kod icin oktali virgul gerek.

x <- x[c(-1,-2,-3)]
x #birde cok indisi secerek cikartma

cikartilacak <- c(2,3,1)
x[-cikartilacak]#degiskene atayip basina - koyup yle cikarttik
#kalici cikartma icin yine basina atama operatoru koyabilirsin. 



#### VEKToRLERDE ELEMANLARINI DEgisTiRME #######

x <- c(10,11,21,32,43,56)
x

x[2] <- 33
x #normalde 11 olan 2. indis degerini 33 ile degistik.


x[2:3] <- c(25,18)
x #birden fazla degeri degistirmek icin kullandik


x[c(2,3,5)] <- c(26,48,67)
x #birden cok indise yeni sayi atama kodumuz.

x[c(1,2,3,4,5,6)] <- 1
x #tum elemanlar 1 oldu

x[c(1,2,3)] <- c(1,2)
x # 1 ve 2yi 1. ve 2. elemana atams,. ayni zamanda tekrara dusup 1i 3. elemana da atamis


x[c(1,2)] <-c(25,36,48)
x #sadece ilk iki elemans degismiss

x[1:3] <- 11:13
x # ilk 3 elemana 11 12 13 atiyor


####### VEKTORLERE YENI ELEMAN EKLEME######

x <- c(11,22,33,44)

x[length(x)+1] <- 55
x #length ile son indisi buluyoruz ve 1 ekkliyoruz. yeni indis demek bu

x[6] <- 66
x


x[7:10] <- c((7:10)*11)
x #her birini 11 ile carpip xe ekledik.

x[ c((length(x)+1):(length(x)+3))] <- c(((length(x)+1):(length(x)+3))*11)
x #her bir sayiyi 11 ile carpip kendi indisine yaz dedik. 
#ben buldum cokiiiii 


x[16] <- 15
x #aradaki elemanlar bos kalir NA yazar












