################# data framler #######################

#data frameler excel gibi d??????n sat??r ve s??tunlardan olu??ur
#vektorlerde bu yok, listelerde bu yok

#vekt??rleri olu??turduk.
x <- c(10,20,30,40)
y <- c('a','b','c','d')
z <- c(11,22,33,44)

#hepsini dataframe icine att??k
df <- data.frame(x,y,z)
df

View(df) # r studio i??erisinde olan bi ??zellik. tabloyla g??steriyor. g??zel duruyo

#data frame olu??mas?? i??in vekt??rlerdeki karakter say??lar?? e??it olmal??
#data.frame(x,y,z,t) k??sm?? ??al????maz ????nk?? karalter say??lar?? 4,4,4,5 dir

t <- c(1,2,3,4,5)
e <- c(45,56,67,37,23)
data.frame(x,y,z,t)
data.frame(t,e)
# burada s??tunlar??n ad??n?? de??i??iyoruz
df2 <- data.frame('AVar'=x,'BVar'=y,'CVar'=z)
View(df2)
 
#s??tun isimleri k??t?? geliyor
df3 <- data.frame(c(1,2,3,4,5),
                  c(34,45,67,89,45),
                  c(12,34,45,34,45))
df3

#ad verdik s??tunlara, t??rnak atmasak da olur. ars??nda bo??luk koycaksak t??rnak laz??m.

df4 <- data.frame('a'=c(1,2,3,4,5),
                  'b'=c(34,45,67,89,45),
                  'c'=c(12,34,45,34,45))
df4





################# data frameler i??erisinde sat??r/s??tun se??imi#####################

df <- data.frame(a=c(1,2,3,4,5),
                 b=c('a','b','c','d','e'))
df

df[1,1] #1. sator 1. s??tun demek. ayn?? matrisler gibi
df[1,] #sadece 1. sat??r
df[5,2] #5. sat??r 2. s??tun
df[,2] #sadece 2. s??tun
df$a #a k??sm??n?? getiriyor vektor
df$a[2] #a vekt??r??n??n 2. eleman??n?? getiriyor
df$b[2] #b vekt??r??n??n 2. eleman??n?? getirir


class(df[,2]) #factor/character

df2 <- data.frame(a=c(1,2,3,4,5),
                 b=c('a','b','c','d','e'),
                 c=c(45,23,67,89,24))
df2

df2[,c(2,3)] #2. ve 3. sat??r?? ald??k
class(df2[,c(2,3)]) #data.frame olarak gelir


df2[2] #virg??l yoksa s??tun olarak sayar
df2[1] #ayn??s??
class(df2[2]) #data.frame

df2['a'] # a s??tunu gelir dataframe olarak verir

df2[['a']] # a s??tunu vekt??r olarak bize gelir
df2[c('a','b')]



############### data framelerden sat??r/s??tun ????kartma ###########

dt <- data.frame(a=c(1,2,3,4),b=c('a','b','c','d'),
                 c=c(66,77,88,99))
dt

dt[-1,] #ilk sat??r?? kald??r??r
dt[-2,-1] #ikinci sat??r birinci s??tun kald??r??l??r

dt[-3:-1,]#ilk ???? sat??r gidio
dt[,-2:-1]#ilk iki s??tun gidiyo

dt[-c(1,2),] #iilk iki sat??r gider

dt[[2]][2] <- NA #2. sat??r 2. s??t??n kesi??imine NA aatr

dt
dt ['c'] <- NULL # c vektorunu s??f??rlad??
dt

dt[-c(2:4),] #2 ile 4 aras??ndaki sat??rlar?? siler.


df <- data.frame(a=c(1,2,3),b=c(4,5,6))
df

class(df[-1,]) #data frame
class(df[,-1]) #vektor


df[2] <- NULL
df #2. s??tun gitti. virg??l koymazsan data framelerde s??tunu se??erdi unutma.

df[2,] <- NULL #hata verir ????nk?? 2. sat??r iki ayr?? vectorden geliyo. de??erleri 
#NA yapabilirsin ama null ile silemezsin
df[2,] <- NA
df #2. sat??r?? NA yapt??k. bu ??al????t??.


df[-c('a','b')] #e??er de??i??ken harf ise - de??il null kullan
df[c('a','b')] <- NULL #null kulland??k her ??eyi sildi
df















################# data frame eleman de??eri de??i??tirme###########

df <- data.frame(a=c(1,2,3,4),b=c(4,5,6,7))
df

df[1,2] <- 44 #ikimci sat??r ikinci s??tun 44 yapar
df

df[c(3,4),2] <- c(66,77) # 3 ve 4. satr??daki 2. s??t??nla birle??imindeki 2 say??ya
# 66 ve 77 de??erlerini atar/de??i??tirir
df



################ data frame sat??r sutun ekleme##################

df <- data.frame(a=c(1,2,3,4),
                 b=c('a','b','c','d'),
                 c=c(11,22,33,44))
df

df <- cbind(df,d=c(33,44,55,66)) #sut??n ekledik ve atad??k
df

df[length(df)+1] <- c(77,88,99,55) #son s??tunun say??s??n?? ??eker 1 ekler
#yeni s??tun olarak kaydolmu?? olur atama da yapm???? bulunuyoruz. 
#virg??l kullanmad??????m??z i??in s??tun sayd??
df

df$yeni <- c(12,23,34,54) #yeni adl?? s??tunu ekler
df

length(df) #sadece s??tunu say??yor

df['yeni2'] <- c(5,6,8,4) #s??tun ekler yine
df


df <- rbind(df,c(2,6,5,8,9,5,6))
df

df[7, ] <- c(3,6,5,9,5,3,4)
df

df[6, ]  <- c(3,6,5,9,5,3,4)
df


rbind(df,data.frame('a'=c(1),'b'=c('c'),
      'c'=c(23),'d'=c(13),'V5'=c(23),
      'yeni'=c('f'),'yeni2'=c(13))) 
#s??tun adlar??n?? yazarak sat??r eklemek


rbind(df,data.frame('a'=c(1,2),'b'=c('c','d'),
                    'c'=c(23,24),'d'=c(13,14),'V5'=c(23,25),
                    'yeni'=c('f','d'),'yeni2'=c(13,17))) 
#atama yap??lmam???? hali. 2 sat??r ekledik.


eklenecek <- data.frame('a'=c(1,2),'b'=c('c','d'),
                        'c'=c(23,24),'d'=c(13,14),'V5'=c(23,25),
                        'yeni'=c('f','d'),'yeni2'=c(13,17))
#eklemek i??in yeni olu??rduk.
eklenecek


rbind(df,eklenecek) #d????ar??dan ald??????m??z data framei ekledik.
#factor veri tipine normal d??zeyde bi ??ey ekleyemezsin. normalde na olur.
#ama bu ??ekilde eklersek factorlere de atanabilitor ve na gelmiyo.
#rbind kullan daha sa??l??kl??


eklenecekSutun <- data.frame('K'=c(1,2,3,4,5,6,7,8),'e'=c(1,2,3,4,5,6,7,8))
eklenecekSutun

cbind(df,eklenecekSutun)
#ayn??s??n?? s??tun i??in yapt??k


