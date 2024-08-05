######## veri turleri ########


#integer (tam sayi)
#Numeric (reel sayi)
#Character(string, Nominal)
#Factor(ordinal)
#Logical(mantiksali true false)


x <- c(12,12,12,34,56)
x
class(x) #numeric

y <- c('a','b','c')
y
class(y) #character

f <- factor(c('a','b','c')) #levellari var bunlarin, karakterlerden olusmali bi de
#factor fonksiyonunun icine vektor yazmak ZORUNDAYIZZZ 
f
class(f) #factor

l <- c(T,F,T,T,F)
class(l) #logical

##### veri turu kontrolleri ####
x <- c(12,23,45,56)
y <- c('a','b','c')
f <- factor(c('d','f','g'))
l <- c(T,F,T,F,T)

is.numeric(x) #true dedi
is.integer(x) #false dedi. sebebi su
#veriyi olustururken r dili o,nce numeric yapoiyo.
is.character(y)#true
is.factor(f) #true
is.logical(l) #true 


is.logical(x)
is.factor(y)
is.character(x)
#hepsi false


class(y) #bu da bi yoonetem
class(x)
class(f)
class(l)

#class gelen sonuca hangi sinifta oldugunu yazar ve gonderir
#is.factor,is.character fln ise true false goonderir. bu youzden bunlar daha ocok iose yarica ileride


##### veri tourou donusumleri####

x <- c(12,13,14,15,16)
class(x) #numeric
as.integer(x) #integer yapar ama atama yapmayiz
x <- as.integer(x)
class(x) #integer

x<- as.numeric(x) #tekrar cevirmek icin
class(x)

y <- c('A','B','C','D')
class(y)

y <- as.factor(y) #faktore cevrdi, leves verdi 
class(y)

y <- as.character(y)
class(y) #eski haline dondu

 

xn <- x
xn

xc <- as.character(xn)
xc #tirnak icerisine alip verdi. karakter oldu bunlar
class(xc) #character


x <- c(0,1,0,0,1,1)
class(x)
xl<-as.logical(x) #true ve falselara cevirdi
class(xl) #logical

x1 <- c(11,23,45,56)
x1l <- as.logical(x1)
x1l #hepsini true verdi. yani 0dan farklilar  full true verir


x2 <- c(11,23,45,0,-5)
x2l <- as.logical(x2)
x2l #sadece sifir icin false verdi gerisi true


y1 <- c('a','b','c')
y1l <- as.logical(y1)
y1l #na verdi hepsini.
#logical mantiksal operator oldugu icin saycilardan karakterlerde calismaz.
#ya true false ya da 0-1 yazicaz yoksa dogru donm olmaz










##### veri donusumundeki ozel durumlar #####

x <- factor(c('a','b','c','d'))
class(x)
x


#factor to numeric
as.numeric(x) #1,2,3,4 verdi. ama degerler a b c d idi? nasil oldu?
#bunun sebebi levellardir. levellar neyse onu verir

x1 <- factor(c('a','b','c','d','a','b'))
as.numeric(x1) #123412 seklinde verdi
x1 #levellar abcd verdi safece. levellar sadece 1 tane olur: o levelin icinde 
#birden fazla veri olabilir. ama level tek

x2 <- factor(c('a','c','f','b','e','d'))
x2
as.numeric(x2) # 1 3 6 2 5 4 verdi. 

x3 <- factor(c('d','a','c','b'), levels = c('d','a','c','b'))
x3
as.numeric(x3) # 1 2 3 4 sonucunu aldik. kendimi d=1 atamasi yaptik cunku

x4 <- factor(c('10','12','14','45'))
x4

as.numeric(x4) #1 2 3 4 olarak geldi. 10 12 14 45 olarak istiyorum.
#once karaktere sonra numerice cevir

x4c <- as.character(x4)
x4n <- as.numeric(x4c)
x4c;x4n #sayi olarak alabildik. goollll
#unutma factoru once karakter sonra numeric yaptik


