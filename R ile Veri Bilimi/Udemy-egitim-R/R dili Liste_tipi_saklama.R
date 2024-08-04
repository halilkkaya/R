
###################### L??STELER##################
#liste olu??turma syntax??
l1 <- list(13,56,47,89) #[1] ile ilk indisi ??a????r??r??z. 
l1
l2 <- list("A","B")
l2


l3 <- list("A","B",2,"C",6) #burada vekt??rlerin aksin hem numerik hem de char olabiliyor. ikisini de kendi veri tipinde kaydediyor.
#vekt??rlerde her ikisi de varsa ikisini de char yap??yordu.
l3


# liste i??erisinde vekt??r
l4 <- list(c(1,2,3,4,5),"a",5)
l4


l5 <- list("a"=c(1,2,3,4,5), "b"= "A") #"a" ile indisi kendimiz belirledik.
l5



a =c(56,67)
#d????ar??da olu??turdu??umuz de??i??kenleri bu ??ekilde listelerde kullanabiliriz
l6 <- list('a'=a,'b'=e)
l6


f <- c(45,56)
h <- c(1,2)
fh <- c(f,h) #elemanlar yanyana gelir, vektor olarak
fh <- list(f,h) #her bi vektor liste olur. 2 liste sat??r?? c??kar
fh




#############   L??STELERDE ELEMAN(DE??ER) SE????M?? ##############

list(1,2,3,4)
list(c(1,2,3),'a',5)


x<- list(11,12,c('a','b','c'))
x

x[2] #diretk olarak 2. eleman?? getirir

x[[2]] #2. eleman?? getirir ve i??eri girer

x[3][1] #null d??nd??r??r
x[[3]][1] #3. eleman vekt??rd??. o vekt??r??n de ilk eleman??n?? getirir

class(x[3]) #kontrol etme komutu. list d??nd??r??r
class(x[[3]])#bu da char olarak d??nd??r??r. i??eri girdi??ini anl??yoruz


y <- list('a'=c(1,2,3,4),'b'=c(11,22,33,44))
y

names(y) #adlar?? verir. a ve be verdi burda
y['a'] #liste olarak geldi
y[['a']] #vekt??r olarak geldi
y$a # a'n??n i??erisindeki verileri verir. vekt??rleri yani
y&a[1] # a'n??n ilk de??erini verir.
y&b #bu kullan??ma al???? ileride bunu kullancaz hep



########### L??STELERDE ELEMAN KALDIRMA/S??LME################

x <- list(1,2,3)
x

x <- x[-1] #ilk indisi siliyor ve at??yor
x

y <- list(25,35,c('a','b'),'h')
y[-(1:3)] #1 ile 3 aras?? siler

y[[-1]] #hata veriyor.

y[-1] #ilk de??eri siler ama atama yapmaz

y[-3] # ??????nc?? de??eri siler ama atama yapmaz

y <- y[-1] #atama yap??lm???? hali
y
y[[1]] <- NULL 
y #ilk eleman kald??r??l??yor ve 1. elemana 2. eleman gelecek ??ekilde yer de??i??iliyor
#NULL ile bir de??eri kald??rd??ktan sonra yeniden atama yapmaya gerek yok
#ancak liste i??erisindeki vekt??rlere gideceksek yeniden atama yapmam??z gerek.


y[[3]][-1]
 
y[[3]] <- y[[3]][-1] 
y # ??nemli bi nokta silece??imiz de??eri direkt de??i??kene atarsak
#vekt??rde silinen de??erden sonra kalan de??erler atan??t ve liste sadece o de??erler olur
#hangi vekt??re gideceksek burada atama i??lemi s??ras??nda belirlemek gerekiyor
#????nkl?? oran??n i??erisine yapaca????z atamay??.


x <- list(c(1,2,3) ,'a','b')
x[[1]] <- NULL

x[[1]] <- x[[1]][-2]
x #listedeki  2. eleman?? ????kar??yor

x <- list('a'=c(1,2,3),'b'=c('a','b'))
x$a <- x$a[-1] 
x$b <- x$b[-2]
x #verdi??imiz adland??rmalar?? dolar i??areti ile kullanma y??ntemi

x$a <- NULL
x #a de??erini direkt sildi



############# L??STEDEK?? ELEMANLARIN DE??ERLER??N?? DE??????T??RME/G??NCELLEME##################

x <- list('a','b','c',12,13,c(11,22,33,44))
x

x[[2]] <- 'dd'
x# en do??ru y??ntem budur. riske girme bunu yap hep

x[1] <- 'aa' 
x # ??al??????r ama her durumda de??il bazen hata al??rs??n.

x[[length(x)]][4] <- 0 
x #son elemana gittim onun da 4. eleman??n?? s??f??r yapt??m




############# L??STEDEK?? ELEMAN EKLEME##################


x <- list(1,2,3,4,'a','b',a=c(55,66,77))
x

x[length(x)+1] <- 15
x # listenin son eleman say??s??na 1 ekleyip oraya yeni??r de??eer atad??k

x[[length(x)]][5] <- 99 #x'in son eleman??n??n 5. indisine de??er ekledik
x

x[[length(x)]][length(x$a)+1] <- 110 # x'in son eleman??n??n i??ine girip
#onun da son eleman??na 1 ekliyoruz ve yeni de??er ekleyeblece??imiz yer oluyor.
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
y #unutma ??ift kare parantex daha ??nemli. burda tuttu ama sen yine de ??ift kullan

y[[5]] <- 55 
y #bunun gibi

y[[6]] <- c(44,55,66)
y #6. indise vekt??r?? atad??k

y[7] <- c(77,88,99)
y #7. indise sadece 77.yi atad??. tek kare parantezde hepsini tek tek ayr?? ayr??
#atamaya ??al??????yor. fark bu. ??ift karede direkt o indisin i??ine girerken
#tek kare parantezde listedeki eleman?? se??iyoruz













