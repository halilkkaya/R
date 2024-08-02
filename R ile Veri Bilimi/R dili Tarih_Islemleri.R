# %d g??nler say?? olarak (0-31) 01-31
# %a k??salt??lm???? g??nler mesela Mon
# %A k??salt??lmam???? g??nler mesela Monday
# %m aylar (00-12) 00-12
# %b k??salt??lm???? aylar mesela Jan
# %B k??salt??lmam???? aylar mesela January
# %y 2 haneli t??llar mesela 07
# %Y 4 haneli y??llar mesela 2007

############ tarih i??eren de??erler #########
Sys.Date() #bug??n??n tarihini verir. y??l ay g??n ??eklinde verir

today <- Sys.Date()
today
class(today) #date verdi


g <- format(today,format = "%d/%m/%Y")
g
class(g) #karakter olarak verdi


tarih <- '06/06/2016'
tarih
class(tarih) # karakter verdi.
as.Date(tarih,format = "%d/%m/%Y") #tarihleri yazd??????m??z ??ekilde format koymam??z gerek
#bu karakter olan tarih de??i??kenini date t??r??ne ??evirdi.
yenitarih <- as.Date(tarih,format = "%d/%m/%Y") 
yenitarih
class(yenitarih) #date verdi


as.Date(g, format = "%d/%m/%Y")
today <-as.Date(g,format = "%d/%m/%Y")
today
class(today) #date oldu

tarih2 <- '23-05-2020'
as.Date(tarih2) #date yyyy-aa-gg ??eklinde oldu??u i??in yanl???? ??evirdi
#"0023-05-20" de??erini verdi

as.Date(tarih2, format = '%d/%m/%Y') 
#normalde tarihi tire(-) ile ay??rm????t??k. burda / kulland??k ve hata verdi
#ne ile yazd??ysak format k??sm??nda da o ??ekilde yazam??z gerek. ay??rma ve g??n ay k??s??mlar
#i??in s??yl??yorum. de??i??kende ??nce g??n?? tan??mlay??p da d??n????t??rmede ??nce %m yazarsak
#bu da hatal?? sonu?? verir ya da ??al????maz

#do??rusu ??u olacak:
as.Date(tarih2, format = '%d-%m-%Y') #burda do??ru sonmucu ald??k.




##### tarih ve zaman?? ayn?? anda i??eren de??erler#######
Sys.Date() #sqldeki getdate() ile ayn?? i??lev
Sys.time() #tarih ve zaman?? verir.

t1 <- Sys.time()
t1

class(t1) # "POSIXct" "POSIXt hem tarih hem de zaman demek

t1 <- as.POSIXct(t1) 
t1
class(t1) #"POSIXct" "POSIXt 

unclass(t1) #1720885671 verisi d??n??yo. POSIXct format??n??n ??zelli??i
#tarihleri bellekte bu veri ??eklinde tutmas??

t1 <- as.POSIXlt(t1) 
class(t1) #"POSIXlt" "POSIXt" 
unclass(t1) #bi liste verdi. saniye dakika y??l ay fln verdi
unct1 <- unclass(t1)
unct1 # ay?? bulmak i??in burdan gelen de??ere +1 eklemek gerekiyor
#y??l?? bulmak i??in burdan gelen de??ere 1900 eklemek gerekiyor

names(unct1)
unct1[['min']] #dakikay?? verdi
unct1[['sec']]
unct1[['mday']]

year <- unct1[['year']]+1900
year #y??l?? hesaplad??k

month <- unct1[['mon']] +1
month #ay?? hesaplad??k



t2 <- '12/12/1920 18:30:23'
t2
class(t2) #character

tv2<- as.POSIXlt(t2, format = "%d/%m/%Y %H:%M:%S", tz = 'UTC')
tv2
t2
tv2c <- unclass(tv2)

names(tv2c)
tv2c[['sec']]
tv2c[['min']]
tv2c[['hour']]
tv2c[['year']]+1900
tv2c[['mon']]+1
tv2c[['mday']]


t3 <- '12/12/1840 18:30:23'
t3
class(t3)

tv3 <- as.POSIXlt(t3, format = '%d/%m/%Y %H:%M:%S', tz='UTC')
tv3
tv3c <- unclass(tv3)
tv3c #y??l 1900den k??????k diye eksi verdi dikkat...

names(tv3c)
tv3c[['sec']]
tv3c[['min']]
tv3c[['hour']]
tv3c[['year']]+1900
tv3c[['mon']]+1
tv3c[['mday']]







#### iki tarih aras??ndaki fark hesaplama ######
d <- '12/12/2020'
d2 <- '13/12/2020'

d <- as.Date(d, format = '%d/%m/%Y')
d

d2 <- as.Date(d2, format = '%d/%m/%Y')
class(d2)

d2 - d #Time difference of 1 days sonucu d??n??yor. yani 1 g??n var dio
d - d2 #Time difference of -1 days sonucu d??n??yo. yani -1 gun var.
#yani burda da d d2den k??????k demek d2 dden 1 gun fazla demek

d3 <- Sys.Date()
d3

d3 - d2 #bug??nden c??kartt??k
Sys.Date()-d2 #bug??nden ????kartt??k

as.double(d2-d) #direkt 1 yazd??

as.double(d3-d2) #1308 ??evird
as.double(d3-d2)%%30 #mod ald??k. yani ay??n 18ine denk geliyor gibi bi ??ey.
#baz?? aylar 31 subat 27-28 fln saym??yoz olesine yapt??m ben
as.integer(as.double(d3-d2)/30) #burda iki i??lem yapt??m
#??nce 30a b??ld??m. b??ld??????mde 43.6 ????kt?? yani tam ay?? vermiyor burda 43 laz??m bize
#onu da integer de??ere ??evirirsem o k??sm?? atar. 43. aym???? yani

d4 <- '12/12/2020 15:00:00'
d5 <- '15/06/2022 14:52:30'

d4 <- as.POSIXlt(d4, format = '%d/%m/%Y %H:%M:%S')
d5 <- as.POSIXlt(d5, format = '%d/%m/%Y %H:%M:%S')
d4
d5
class(d5)

d5-d4 #Time difference of 549.9948 days d??nd??rd??

as.double(d5-d4) #549.9948 d??nd??rd?? g??n olarak verdi
as.double(Sys.time()-d5) #759.2073 ??evirdi g??n olarak verdi

d6 <- '12/12/2020 15:00:00'
d7 <- '12/12/2020 15:45:00'

d6 <- as.POSIXlt(d6, format = '%d/%m/%Y %H:%M:%S')
d7 <- as.POSIXlt(d7, format = '%d/%m/%Y %H:%M:%S')
d6
d7

d7-d6 #Time difference of 45 mins dedi
as.double(d7-d6) #45 d??nd??rd?? dakika olarak

d8 <- '12/12/2020 15:00:00'
d9 <- '12/12/2020 16:45:00'

d8 <- as.POSIXlt(d8, format = '%d/%m/%Y %H:%M:%S')
d9 <- as.POSIXlt(d9, format = '%d/%m/%Y %H:%M:%S')
d9-d8 #Time difference of 1.75 hours
as.double(d9-d8) #1.75 verdi. saat ??zerinden








##### tarih ve zaman i??eren vekt??rler#######

as.Date() #tarihe d??nd??r??r
as.POSIXct() #tarih ve zamana d??nd??r??r
as.POSIXlt() #tarih ve zamana d??nd??r??r. names bulunur

d <- '12/12/2012'
d2 <- c('12/12/2012','13/12/2012','14/12/2012')
d2
class(d2) #karakter olarak tan??mlad??

d2 <- as.Date(d2, format = '%d/%m/%Y')
d2
class(d2) #date oldu

d2[2]-d2[1] #1 g??n fark var diyo
as.double(d2[2]-d2[1]) #1 diyo


d3 <- c('12-12-2012','13-12-2012','14-12-2012')
d3 <- rep(d3, times=5) #5 kere tekrar ettirdik
d3 <- rep(d3, each=3) #yanyana olarak 3 kere daha yaz
d3 <- as.Date(d3, format = '%d-%m-%Y')
class(d3)#date diyo
length(d3) #45 tane var
d3

as.double(d3[15]-d3[11]) #1 dedi


d4 <- c('12-12-2012 18:00:00','13-12-2012 19:00:00','14-12-2012 20:00:00')
d4 <- rep(d4, times=3)
d4 <- rep(d4, each =3)
length(d4)

d4 <- as.POSIXlt(d4, format = '%d-%m-%Y %H:%M:%S' ,tz = 'UCT')
class(d4)


d4[5]-d4[7] #11.041667 dedi. 7. indisteki de??er daha b??y??km???? demekki











### ard??????k ilerleyen tarih vekt??r?? olu??turma####

seq(from = 10, to=100, by=5) #5er art
seq(from = 10, to=100, length.out=10) #10 de??er ver demek


#ard??????k tarih
seq(from= as.Date('12/12/2020','%d/%m/%Y'),
    to = as.Date('19/12/2020','%d/%m/%Y'),
    by = 1) #by parametresi girmek zorundas??n. burda 1er g??n artt??

seq(from= as.Date('12/12/2020','%d/%m/%Y'),
    to = as.Date('19/12/2020','%d/%m/%Y'),
    by = 5) #by parametresi girmek zorundas??n. burda 5er g??n artt??

seq(from= as.Date('12/12/2020','%d/%m/%Y'),
    to = as.Date('19/12/2020','%d/%m/%Y'),
    length.out=12) #by parametresi yerine length.out da kullan??labilir.
  #burda ????yle durum var saat olsa veriler daha anla????labilirdi ama ayn?? tarih iki
#kere verilmi?? gibi zannediliyor.



seq(from= as.Date('12/12/2020',format ='%d/%m/%Y'),
    to = as.Date('19/12/2020',format ='%d/%m/%Y'),
    by = 'day') #yine 1 g??n arayla

seq(from= as.Date('12/12/2020',format ='%d/%m/%Y'),
    to = as.Date('19/12/2021',format ='%d/%m/%Y'),
    by = 'month') #aylar artt?? birer birer


d <- seq(from= as.Date('12/12/2020',format ='%d/%m/%Y'),
    to = as.Date('19/12/2021',format ='%d/%m/%Y'),
    by = 'month')

length(d) #13 ay artm???? ????rendik
#by parametresi yaz??lan de??erlerdeki en k????????e g??re artt??r??r default olarak


seq(from= as.Date('12/12/2020',format ='%d/%m/%Y'),
    to = as.Date('19/12/2030',format ='%d/%m/%Y'),
    by = 'year') #y??llar birer birer artt??



seq(from= as.Date('12/12/2020',format ='%d/%m/%Y'),
    to = as.Date('19/12/2030',format ='%d/%m/%Y'),
    by = 'quarter') #3 ayda bi ??eklinde gidiyo.??eyreklik yani

seq(from= as.Date('01/01/2021',format ='%d/%m/%Y'),
    to = as.Date('01/03/2021',format ='%d/%m/%Y'),
    by = 'quarter') # arada 1 hatta 2 ay var ama de??er vermedi. aylar?? ??eyre??e b??lmez

seq(from= as.Date('12/12/2020',format ='%d/%m/%Y'),
    to = as.Date('19/12/2030',format ='%d/%m/%Y'),
    by = 'week') #haftal??k olarak verdi

seq(from= as.Date('12/12/2020',format ='%d/%m/%Y'),
    to = as.Date('19/12/2030',format ='%d/%m/%Y'),
    by = '2 days') #2 g??n dedik

seq(from= as.Date('12/12/2020',format ='%d/%m/%Y'),
    to = as.Date('19/12/2021',format ='%d/%m/%Y'),
    by = '2 months') #2 ayda bi veriyo

seq(from= as.Date('12/12/2020',format ='%d/%m/%Y'),
    to = as.Date('19/12/2030',format ='%d/%m/%Y'),
    by = '2 years') #2 y??lda 1 veriyo



#zaman i??erenler i??in


seq(from= as.POSIXct('01/01/2021 12:30:00', format='%d/%m/%Y %H:%M:%S'),
    to = as.POSIXct('01/01/2021 12:45:00', format='%d/%m/%Y %H:%M:%S'),
    by = 15) #saniye olarak 15 saniyede bi verdi


seq(from= as.POSIXct('01/01/2021 12:30:00', format='%d/%m/%Y %H:%M:%S'),
    to = as.POSIXct('01/01/2021 12:45:00', format='%d/%m/%Y %H:%M:%S'),
    by = 1) #1 saniyede 1 verdi


seq(from= as.POSIXct('01/01/2021 12:30:00', format='%d/%m/%Y %H:%M:%S'),
    to = as.POSIXct('01/01/2021 12:45:00', format='%d/%m/%Y %H:%M:%S'),
    by = 'min') #dakika olarak verdi

seq(from= as.POSIXct('01/01/2021 12:30:00', format='%d/%m/%Y %H:%M:%S'),
    to = as.POSIXct('01/01/2021 14:45:00', format='%d/%m/%Y %H:%M:%S'),
    by = 'hour') #saat olarak verdi


seq(from= as.POSIXct('01/01/2021 12:30:00', format='%d/%m/%Y %H:%M:%S'),
    to = as.POSIXct('01/01/2022 12:45:00', format='%d/%m/%Y %H:%M:%S'),
    by = 'quarter') #3 ayl??k verdi yine dikkattt

seq(from= as.POSIXct('01/01/2021 12:30:00', format='%d/%m/%Y %H:%M:%S'),
    to = as.POSIXct('01/01/2021 15:45:00', format='%d/%m/%Y %H:%M:%S'),
    by = '2 hour') # 2 saatte bi



seq(from= as.POSIXct('01/01/2021 12:30:00', format='%d/%m/%Y %H:%M:%S'),
    to = as.POSIXct('01/01/2021 12:45:00', format='%d/%m/%Y %H:%M:%S'),
    by = '15 min') #15 dakkada bi












##### tarih de??i??kenlerine sorgu yllama. ??nce sonra gibi#####

d <- seq(from = as.Date('2012-01-01'),
         to=as.Date('2020-01-01'),
         by= 'month')
#ay olarak artan vektor
d

d > '2012-05-01' # ilk 5 de??er false gerisi true. as.date demeye gerek kalmad??
#????nk?? yazarken ayn?? formatta yazd??k
d> '2012/05/01'#bu da ??al????t??

d>'2012.05.01' #bu yanl????. standart bi format de??ildir.

d>'01-05-2012' #hepsi true verdi ????nk?? format?? yanl???? yazd??k. foramt?? do??ru yazmak ??ok ??nemli
d[d>'01-05-2019'] #direkt tarihleri veriyo

d[d>as.Date('01/01/2019', format = '%d/%m/%Y')]
#farkl?? formatta sormak i??in yukardaki gibi format atamas?? yapmak zorunday??z

which(d>as.Date('01/01/2019', format = '%d/%m/%Y'))
#hangi indislerde oldu??unu g??steriyor

inx <- which(d>as.Date('01/01/2019', format = '%d/%m/%Y'))
d[inx] #indisleri inx de??i??kenine at??p d[] ile ??a????rd??k

dt <- seq(from = as.POSIXct('2012-01-01 12:00:00'),
          to=as.POSIXct('2012-01-01 12:45:00'),
          by= 'min')
class(dt)





dt > '2012-01-01 12:30:00' #true false ??eklinde verdi

dt[dt>'2012-01-01 12:30:00'] #12.30dan sonras??n?? veriyo

which(dt>'2012-01-01 12:30:00') #indisleri veriyo




##### iki tarih arasd??nda kalanlar?? elde etme ####

x <- c(1,2,3,4,5,6,7)
x > 5 
x < 7

x > 5 & x<7 #aras??n?? veriyo
# & ve i??lemi | veya i??lemi i??in kullan??l??yo

x[x > 5 & x<7]


date <- seq(from =as.Date('2020-05-15'),to = as.Date('2020-09-15'), by ='month')
date
date > '2020-05-15' & date < '2020-08-15' # tipleri ayn?? girmeyi unutma
date[date > '2020-05-15' & date < '2020-08-15' ] #hangi de??erler oldu??unu verir
which(date > '2020-05-15' & date < '2020-08-15' ) #indisleri verir



d2 <- seq(from = as.POSIXct('2020-05-15 12:00:00'),
          to = as.POSIXct('2020-05-16 15:30:30'),
          by = 'min')
d2

d2 > '2020-05-15 12:30:00' & d2<'2020-05-15 12:45:30'

d2[d2 > '2020-05-15 12:30:00' & d2<'2020-05-15 12:45:30'] #aral?????? veriyo
which(d2 > '2020-05-15 12:30:00' & d2<'2020-05-15 12:45:30') #aral??ktaki indisleri veriyo












##### iki tarih vekt??r??n??n fark??n??n elde edilmesi#####

d <- seq(from =as.Date('2020-05-18'),to=as.Date('2020-06-18'),
         by = 'day')
d

f<-as.Date('2020-05-18')-as.Date('2020-04-18') #30 g??n diye verdi
as.double(f) #say??ya cevirdik

d[2]-d[1] #1 g??n verdi


d1 <- seq(from =as.Date('2020-05-18'),to=as.Date('2020-12-18'),
         by = 'month')
d1

d1[7]-d1[1] #g??n olarak verdi yine
as.integer(as.double(d1[7]-d1[1])/30) #aya ??evirdim

d2 <- seq(from =as.Date('2020-01-01'),to=as.Date('2020-06-01'),
          by = 'month')
d2

d3<- seq(from =as.Date('2020-07-01'),to=as.Date('2020-12-01'),
          by = 'month')
d3



d3-d2 #g??n farklar??n?? veriyo

as.integer(as.double(Sys.Date()-d3[length(d3)])/30/12) #3 y??l sonucunu ald??m.
#3.666yd?? normalde integer alarak sadece y??l?? istedim.


##### tarih vekt??rlerinin s??ralamas??####

d <- c('2020-05-01','2020-06-01','2020-04-06','2025-12-24')
d<- as.Date(d)
class(d)

sort(d) #k??????kten b??y????e s??ralad??k
sort(d,decreasing = TRUE) #b??y??kten k????????e s??ralad??k


d1 <- seq(from =as.Date('2021-01-01'), to=as.Date('2022-01-01'),by = 'month') 
d1
d1s<-sample(d1) #kar??lt??ema i??lemi yapt??k
d1s

d1s<-rep(d1s,time=2)
d1s <- rep(d1s, each=3)
class(d1s)
sort(d1s) #k??????kten b??y????e s??ralad??
sort(d1s,decreasing = TRUE)# b????ykten k????????e s??ralad??k


d2 <- seq(from =as.POSIXct('2021-01-01 12:00:00'), 
          to=as.POSIXct('2021-01-02 12:00:00'),
          by = 'hour') 
d2
d2s<-sample(d2)
d2s
sort(d2s)
sort(d2s,T)




##### strptime() fonksitonu ile tarih-zaman degerleri olu??turma####

#strptime() fonksiyonu as.POSIXct() as.POSIXclt() gibi cal??s??yo

x <- '2012-05-05'
x<-strptime(x,format = '%Y-%m-%d') #D??KKAT
#D????KAT burda format girmek zorunday??z.
#format?? date format??nda yazsak bile format girmemizi istiyor
#as.Date as.POSIXct() ve as.POSIXlt() fonksiyomlar??nda ise
#e??er default date ??eklini girdiyse formata gerek yoktu.
#ara??t??rmaya g??re strptime() fonksiy??nu di??erlerinden daha h??zl?? ??al??????yormu?? ama
#o yuzden ezberlesek iyi olur

class(x) #as.POSIXlt() ve as.POSIXt() verdi

####chron k??t??phanesini kullanarak tarih ve zaman de??i??ken olu??turma####
install.packages('chron') #chronu indirdik

?chron
x <- '01/01/2020'
y <- '02/02/2020'

d <- c(x,y)
d
class(d) #char

d<-chron(dates. = d , format = "d/m/y")
class(d) #dates times diyor


xt <- '12:00:00'
yt <- '12:30:00'

d <- c(x,y)
dt <- c(xt,yt)
d
dt

class(d)
class(dt)

d<-chron(dates. = d ,times. = dt, format = c(dates='d/m/y', times='h:m:s'))
class(d) #chron date times
d
d[1]









#### tarih ve zaman??n ayr??lmas??####
d <- seq(from =strptime('2012-01-01 12:00:00',format = '%Y-%m-%d %H:%M:%S'),
         to =strptime('2012-01-30 12:30:00',format = '%Y-%m-%d %H:%M:%S'),
         by = 'day')
d
dt<-format(d, format = '%Y-%m-%d')
dz<-format(d, format = '%H:%M:%S')
class(dz) #karakter oldular
class(dt) #karakter oldular

df <- data.frame('Tarih'=dt, 'Zaman'= dz)
df

class(df$Tarih) #karakyter

as.Date(df$Tarih)

as.POSIXct(df$Zaman, format = '%H:%M:%S') #sorun ??u ki sadece zaman ??eviremiyor. yan??na ??imdiki zaman??n
#atrihini de ekliyor dikkat
df

chron(dates. = df$Tarih, format = 'y-m-d')
times(df$Zaman, format = 'h:m:s') #burda yann??na ??imdiki tarihi eklemeden zaman?? tek ba????na alabildikkkk
#times chron ile gelen bi fonksiyon b??yle bi kullan??m?? da var times. dan ba??ka olarak

df$Tarih <- chron(dates. = df$Tarih, format = 'y-m-d')
df$Zaman <- times(df$Zaman, format = 'h:m:s')
df #art??k tarih ve zaman olarak ayr??ld??. 


df$Zaman[1] - df$Zaman[2] #ay??r??p ikisi aras??nda i??lem yapt??k
df$Tarih[3]- df$Tarih[1] #arada 2 g??n var yaz??o




#### weekdays() ve months() fonksiyonlar??####

x <- '01/01/2020'
x1 <- '02/01/2020'

y <-as.Date(x,format = '%d/%m/%Y')
y1 <-as.Date(x1,format = '%d/%m/%Y')
y
y1
weekdays(y) #??ar??maba g??n?? diyo
weekdays(y1) #per??embe g??n?? diyo

months(y) #january diyo


m <-seq(from =as.Date('2020-01-01'), to = as.Date('2021-01-01'), by = 'month')
m
weekdays(m) #t??m tarihlerin tek tek g??n??n?? verdi
months(m) #t??m tarihlerin tek tek ay??n?? verdi s??ras??yla

ms <- sample(m)
weekdays(ms) #kar????m???? halinin verdi
mss<-sort(ms)
weekdays(mss)#s??ralanm???? halinin verdi

x2 <- '1 Eyl??l 2019'
x3 <- '1 September 2019'
as.Date(x2, format = '%d %B %Y') #hata verdi eyl??l t??rk??e diye
as.Date(x3, format = '%d %B %Y') #2019-09-01 sonucunu verdi

sessionInfo()

Sys.setlocale('LC_TIME','tr_TR.UTF-8') #t??rk??eye cevirip eyl??l?? kabul ettirdik
Sys.setlocale('LC_TIME','en_US.UTF-8')




##### ??dev #####

#L??tfen bu ??devi tamamlamak i??in a??a????da kod olarak g??nderilen tarih vekt??r??n?? kullan??n??z. Bu tarih vekt??r??n?? R'da olu??turarak a??a????daki sorularda belirtilen i??lemleri ger??ekle??tirip kodlar??n??z?? cevap b??l??m??nde belirtiniz.

tarihler <- c('20/09/2020', '19/10/2020' , '19/11/2020' , 
              '11/12/2020' , '12/12/2019' , '17/08/2019', 
              '12/12/2018')


#Bu ??dev ile ilgili sorular
#1. Olu??turulan tarih vekt??r??n?? as.Date() fonksiyonunu kullanarak R'da tan??ml?? bir tarih veri t??r??ne ??eviriniz ve t??r??n?? kontrol ediniz.

#2.  R'da tan??ml?? bir tarihe d??n????en vekt??r??  son gelen tarihten ??nce gelen tarihe do??ru s??ralay??n??z (B??y??kten k????????e).

#Tarihlerdeki haftan??n g??nlerini bulunuz. Sonu?? ingilizce veya t??rk??e olabilir.



tarihler <- c('20/09/2020', '19/10/2020' , '19/11/2020' , 
              '11/12/2020' , '12/12/2019' , '17/08/2019', 
              '12/12/2018')


tarihlerD <- as.Date(tarihler, format = '%d/%m/%Y')
class(tarihlerD)

sort(tarihlerD, decreasing = TRUE)

weekdays(tarihlerD)









