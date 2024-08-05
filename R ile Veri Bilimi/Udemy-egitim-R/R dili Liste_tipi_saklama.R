
###################### LiSTELER##################
#liste olusturma syntaxi
l1 <- list(13,56,47,89) #[1] ile ilk indisi cagiririz 
l1
l2 <- list("A","B")
l2


l3 <- list("A","B",2,"C",6) #burada vektorlerin aksin hem numerik hem de char olabiliyor. ikisini de kendi veri tipinde kaydediyor.
#vektorlerde her ikisi de varsa ikisini de char yapiyordu.
l3


# liste icerisinde vektor
l4 <- list(c(1,2,3,4,5),"a",5)
l4


l5 <- list("a"=c(1,2,3,4,5), "b"= "A") #"a" ile indisi kendimiz belirledik.
l5



a =c(56,67)
#disarida olusturdugumuz degiskenleri bu sekilde listelerde kullanabiliriz
l6 <- list('a'=a,'b'=e)
l6


f <- c(45,56)
h <- c(1,2)
fh <- c(f,h) #elemanlar yanyana gelir, vektor olarak
fh <- list(f,h) #her bi vektor liste olur. 2 liste satiri cikar
fh




#############   LiSTELERDE ELEMAN(DEgER) SEciMi ##############

list(1,2,3,4)
list(c(1,2,3),'a',5)


x<- list(11,12,c('a','b','c'))
x

x[2] #diretk olarak 2. elemani getirir

x[[2]] #2. elemani getirir ve iceri girer

x[3][1] #null dondurur
x[[3]][1] #3. eleman vektordu. o vektorun de ilk elemanini getirir

class(x[3]) #kontrol etme komutu. list dondurur
class(x[[3]])#bu da char olarak dondurur. iceri girdigini anliyoruz


y <- list('a'=c(1,2,3,4),'b'=c(11,22,33,44))
y

names(y) #adlari verir. a ve be verdi burda
y['a'] #liste olarak geldi
y[['a']] #vektor olarak geldi
y$a # a'nin icerisindeki verileri verir. vektorleri yani
y&a[1] # a'nin ilk degerini verir.
y&b #bu kullanima alis ileride bunu kullancaz hep



########### LsiSTELERDE ELEMAN KALDIRMA/SiLME################

x <- list(1,2,3)
x

x <- x[-1] #ilk indisi siliyor ve atiyor
x

y <- list(25,35,c('a','b'),'h')
y[-(1:3)] #1 ile 3 arasi siler

y[[-1]] #hata veriyor.

y[-1] #ilk degeri siler ama atama yapmaz

y[-3] # ucuncu degeri siler ama atama yapmaz

y <- y[-1] #atama yapilmaz hali
y
y[[1]] <- NULL 
y #ilk eleman kaldiriliyor ve 1. elemana 2. eleman gelecek sekilde yer degiailiyor
#NULL ile bir degeri kaldirdiktan sonra yeniden atama yapmaya gerek yok
#ancak liste icerisindeki vektorlere gideceksek yeniden atama yapmamiz gerek.


y[[3]][-1]
 
y[[3]] <- y[[3]][-1] 
y # onemli bi nokta silecegimiz degeri direkt degiskene atarsak
#vektorde silinen degerden sonra kalan degerler atanit ve liste sadece o degerler olur
#hangi vektore gideceksek burada atama islemi sirasinda belirlemek gerekiyor
#cunklu oranin icerisine yapacaggiz atamayi.


x <- list(c(1,2,3) ,'a','b')
x[[1]] <- NULL

x[[1]] <- x[[1]][-2]
x #listedeki  2. elemani ccikariyor

x <- list('a'=c(1,2,3),'b'=c('a','b'))
x$a <- x$a[-1] 
x$b <- x$b[-2]
x #verdiiimiz adlandirmalari dolar isareti ile kullanma yontemi

x$a <- NULL
x #a degerini direkt sildi



############# LiSTEDEKi ELEMANLARIN DEgERLERiNi DEgisTiRME/GuNCELLEME##################

x <- list('a','b','c',12,13,c(11,22,33,44))
x

x[[2]] <- 'dd'
x# en dogru yontem budur. riske girme bunu yap hep

x[1] <- 'aa' 
x # calisir ama her durumda degil bazen hata alirsin.

x[[length(x)]][4] <- 0 
x #son elemana gittim onun da 4. elemanini sifir yaptim




############# LiSTEDEKi ELEMAN EKLEME##################


x <- list(1,2,3,4,'a','b',a=c(55,66,77))
x

x[length(x)+1] <- 15
x # listenin son eleman sayisina 1 ekleyip oraya yeniir degeer atadik

x[[length(x)]][5] <- 99 #x'in son elemaninin 5. indisine deger ekledik
x

x[[length(x)]][length(x$a)+1] <- 110 # x'in son elemaninin icine girip
#onun da son elemanina 1 ekliyoruz ve yeni deger ekleyeblecegimiz yer oluyor.
x

x[[1]][2] <- 12
x

class(x[2])
class(x[3])
class(x[[2]])
class(x[[7]])
class(x[7])

y <- list(11,22,33)

y[4] <- 44 
y #unutma cift kare parantex daha onemli. burda tuttu ama sen yine de cift kullan

y[[5]] <- 55 
y #bunun gibi

y[[6]] <- c(44,55,66)
y #6. indise vektoru atadik

y[7] <- c(77,88,99)
y #7. indise sadece 77.yi atadi. tek kare parantezde hepsini tek tek ayri ayri
#atamaya calisiyor. fark bu. cift karede direkt o indisin icine girerken
#tek kare parantezde listedeki elemani seciyoruz













