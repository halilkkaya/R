################# data framler #######################

#data frameler excel gibi dusun satir ve situnlardan olusur
#vektorlerde bu yok, listelerde bu yok

#vektorleri olusturduk.
x <- c(10,20,30,40)
y <- c('a','b','c','d')
z <- c(11,22,33,44)

#hepsini dataframe icine attik
df <- data.frame(x,y,z)
df

View(df) # r studio icerisinde olan bi ozellik. tabloyla gosteriyor. gouzel duruyo

#data frame olusmasi icin vektorlerdeki karakter sayilari esit olmali
#data.frame(x,y,z,t) kismi calismaz cunku karalter sayilari 4,4,4,5 dir

t <- c(1,2,3,4,5)
e <- c(45,56,67,37,23)
data.frame(x,y,z,t)
data.frame(t,e)
# burada sutunlarin adini degisiyoruz
df2 <- data.frame('AVar'=x,'BVar'=y,'CVar'=z)
View(df2)
 
#sutun isimleri kotu geliyor
df3 <- data.frame(c(1,2,3,4,5),
                  c(34,45,67,89,45),
                  c(12,34,45,34,45))
df3

#ad verdik sutunlara, tirnak atmasak da olur. arsinda bosluk koycaksak tirnak lazim.

df4 <- data.frame('a'=c(1,2,3,4,5),
                  'b'=c(34,45,67,89,45),
                  'c'=c(12,34,45,34,45))
df4





################# data frameler icerisinde satir/situn seiimi#####################

df <- data.frame(a=c(1,2,3,4,5),
                 b=c('a','b','c','d','e'))
df

df[1,1] #1. sator 1. situn demek. ayni matrisler gibi
df[1,] #sadece 1. satir
df[5,2] #5. satir 2. sutun
df[,2] #sadece 2. sutun
df$a #a kismini getiriyor vektor
df$a[2] #a vektorunun 2. elemanini getiriyor
df$b[2] #b vekturunun 2. elemanini getirir


class(df[,2]) #factor/character

df2 <- data.frame(a=c(1,2,3,4,5),
                 b=c('a','b','c','d','e'),
                 c=c(45,23,67,89,24))
df2

df2[,c(2,3)] #2. ve 3. satiri aldik
class(df2[,c(2,3)]) #data.frame olarak gelir


df2[2] #virgil yoksa situn olarak sayar
df2[1] #aynisi
class(df2[2]) #data.frame

df2['a'] # a situnu gelir dataframe olarak verir

df2[['a']] # a sutunu vektor olarak bize gelir
df2[c('a','b')]



############### data framelerden satir/situn ccikartma ###########

dt <- data.frame(a=c(1,2,3,4),b=c('a','b','c','d'),
                 c=c(66,77,88,99))
dt

dt[-1,] #ilk satiri kaldirir
dt[-2,-1] #ikinci satir birinci sutun kaldirilir

dt[-3:-1,]#ilk uc satir gidio
dt[,-2:-1]#ilk iki sutun gidiyo

dt[-c(1,2),] #iilk iki satir gider

dt[[2]][2] <- NA #2. satir 2. sutun kesisimine NA aatr

dt
dt ['c'] <- NULL # c vektorunu sifirladi
dt

dt[-c(2:4),] #2 ile 4 arasindaki satirlari siler.


df <- data.frame(a=c(1,2,3),b=c(4,5,6))
df

class(df[-1,]) #data frame
class(df[,-1]) #vektor


df[2] <- NULL
df #2. sutun gitti. virgul koymazsan data framelerde sutunu secerdi unutma.

df[2,] <- NULL #hata verir cunku 2. satir iki ayri vectorden geliyo. degerleri 
#NA yapabilirsin ama null ile silemezsin
df[2,] <- NA
df #2. satiri NA yaptik. bu calisti.


df[-c('a','b')] #eger degisken harf ise - desil null kullan
df[c('a','b')] <- NULL #null kullandik her seyi sildi
df















################# data frame eleman degeri degistirme###########

df <- data.frame(a=c(1,2,3,4),b=c(4,5,6,7))
df

df[1,2] <- 44 #ikimci satir ikinci situn 44 yapar
df

df[c(3,4),2] <- c(66,77) # 3 ve 4. satridaki 2. sutunla birlesimindeki 2 sayfya
# 66 ve 77 degerlerini atar/degistirir
df



################ data frame satir sutun ekleme##################

df <- data.frame(a=c(1,2,3,4),
                 b=c('a','b','c','d'),
                 c=c(11,22,33,44))
df

df <- cbind(df,d=c(33,44,55,66)) #sutin ekledik ve atadik
df

df[length(df)+1] <- c(77,88,99,55) #son sutunun sayisini ceker 1 ekler
#yeni stun olarak kaydolmus olur atama da yapmis bulunuyoruz. 
#virgul kullanmadigimiz icin sutun saydi
df

df$yeni <- c(12,23,34,54) #yeni adli sutunu ekler
df

length(df) #sadece sutunu sayyor

df['yeni2'] <- c(5,6,8,4) #sutun ekler yine
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
#sutun adlarini yazarak satir eklemek


rbind(df,data.frame('a'=c(1,2),'b'=c('c','d'),
                    'c'=c(23,24),'d'=c(13,14),'V5'=c(23,25),
                    'yeni'=c('f','d'),'yeni2'=c(13,17))) 
#atama yapilmamis hali. 2 satir ekledik.


eklenecek <- data.frame('a'=c(1,2),'b'=c('c','d'),
                        'c'=c(23,24),'d'=c(13,14),'V5'=c(23,25),
                        'yeni'=c('f','d'),'yeni2'=c(13,17))
#eklemek icin yeni oluurduk.
eklenecek


rbind(df,eklenecek) #disaridan aldigimiz data framei ekledik.
#factor veri tipine normal duzeyde bi sey ekleyemezsin. normalde na olur.
#ama bu sekilde eklersek factorlere de atanabilitor ve na gelmiyo.
#rbind kullan daha sa7lkl


eklenecekSutun <- data.frame('K'=c(1,2,3,4,5,6,7,8),'e'=c(1,2,3,4,5,6,7,8))
eklenecekSutun

cbind(df,eklenecekSutun)
#aynsini sutun ixin yaptik


