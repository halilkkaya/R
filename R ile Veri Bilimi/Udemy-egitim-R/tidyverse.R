data %>% select(v1) %>% filter(v1>5) 
#pipeline mantigi kisaca su. art arda istediklerimizi araya %>% koyarak yapabiliyoruz.
#ustte once v1 sutununu sectik sonra v1>5 olanlari filtrele dedik mesela

View(iris)  
iris %>% select(Sepal.Length,Petal.Width) %>% filter(Petal.Width > 2.1 & Sepal.Length<6)
#mesela sepal length ve peral width getirdik ve kosullari yazdik

##### select()####

#sutun secimi icindir


iris %>% select(Sepal.Length, Sepal.Width) #iki sutunu getirdi data frame olarak

#istersek atama yapabiliriz

x <- iris %>% select(Sepal.Length, Sepal.Width) #iki sutunu getirdi data frame olarak
x #atanmis hali

##### slice() fonk satir####
# satir bazinda secimdir
iris[1:5,] #ilk 5 satir

iris %>% slice(1,2) #ilk iki satir
iris %>% slice(3,2) #uc ve iki satir
iris %>% slice(1:15) # ilk 15
iris %>% slice(c(1,2,5,8,45)) # sectiklerimiz

iris %>% select(Sepal.Length,Petal.Length) %>% slice(1:5) #istedigimiz sutunlarin
#ilk 5 satiri

#slice_min ve slice_max icine girdigin desigkenden verdigin sayi kadar min/max sayi getirir
 
iris %>% slice_min(order_by = Sepal.Length, n = 10) #sepal lengthte 10 tane min deger verdi
iris %>% slice_max(order_by = Sepal.Width, n = 10) #sepal width 10 tane max deger verdi
iris %>% slice_max(order_by = Species, n = 10) #alfabetik sirali
#ve hepsi virginica oldugu icin hepsini verdi 10 tane vermedi

#slice_sample() rastgele satir verir

iris %>% slice_sample(n =10) #rastgele 10 satir verdi
iris %>% select(Sepal.Length) %>%slice_sample(n =10) #rastgele 10 satir verdi sepallengthden


##### distinct () nedir####

#seride birden fazla ayni veri varsa hepsini unique olarak verir. yani tekil
 
iris %>% distinct(Species) #icindeki degerleri tek tek verdi. toplam 3 satir
iris %>% distinct(Species, .keep_all = TRUE) #diger degerlerden ilk satirlari da verir


###### arrange() ile siralama #####

iris %>% arrange(Sepal.Length) #sepal.legthe gore kucukten buyuge sirala
iris %>% arrange(Sepal.Width, Sepal.Length) #sepal.width gore kucukten buyuge sirala
# ilk yazan onemli ilki ayniysa ikinciye bakar


iris %>% arrange(desc(Sepal.Width)) #sepal.width gore buyukten kucuge sirala
iris %>% arrange(desc(Sepal.Width), desc(Sepal.Length)) # once sepal.width gore 
#sonra sepallengthe gore buyukten kucuge sirala


iris %>% arrange(desc(Sepal.Width), Sepal.Length) # once sepal.width gore buyukten kucuk
#sonra sepal length kucukten buyuk


###### summarise()  ozet bilgiler######

iris %>% summarise(mean = mean(Sepal.Length),
                   median = median(Sepal.Length),
                   sd = sd(Sepal.Length)) #bi cok fonk ayni anda veriyo

#group by ile beraber daha yararli olcak

#####group_by ile gruplama #####

# en cok kullandigimiz


iris %>% group_by(Species) #gruplar hakkinda bilgi verio
# 3 degisken var dio ve 10 tane veriyo

df <- iris %>% group_by(Species) #gruplar hakkinda bilgi verio
class(df) #[1] "grouped_df" "tbl_df"     "tbl"        "data.frame"
print(df, n =150) #hepsini verdi. default 10 veriyo

extra <- c(rep('A',30),rep('B',30),rep('C',30),rep('D',30),rep('E',30))

iris$extra <-  extra
View(iris)

library(tidyverse)

iris %>% group_by(Species, extra) #grup ad?? hem species hem extra ve extra 7
#tane oldugu icin 7 grup var dedi

###### group by ile summarise kullanimi######
iris %>% group_by(Species) %>% summarise(
                                              
                                      SepalLengthOrtalama = mean(Sepal.Length)
                                        ) #sepal length ortalamalarini gruplara gore verio



iris %>% group_by(Species) %>% summarise(
  
  SepalLengthOrtalama = mean(Sepal.Length),
  sepalwidthOrt = mean(Sepal.Width),
)  #ikisini verdi

iris %>% group_by(Species, extra) %>% summarise(
  
  SepalLengthOrtalama = mean(Sepal.Length),
  sepalwidthOrt = mean(Sepal.Width),
) #setosa a, setosa b
  # versicolor b, versicolor c, versicolor d
  #virginica d, virginica e c??kt??s??n?? verdi
  #onlara gore ortalamasini verdi

minmax <-  function(x){
  return(max(x)-min(x))
}

iris %>% group_by(Species, extra) %>% summarise(
  
  SepalLengthOrtalama = mean(Sepal.Length),
  sepalwidthOrt = mean(Sepal.Width),
  minmax(Sepal.Length) #kendi fonk ekledik meselaaaa
)

##### filter fonk kulanimi #####

#filtrelemek icin operatorler subset gibi

iris %>% select(Sepal.Length) %>% filter(Sepal.Length>7) #7den buyuk sepal length ver

iris %>% filter(Sepal.Length<5) #5den kuck sepal length ver tum sutunlar

iris %>% filter(Sepal.Length<5, Sepal.Width>3) #istedigimiz kodullari verdi.
#virgul koyunca ve (&) isareti gibi oluyo. & de kullanilabilir.

iris %>% filter(Sepal.Length<5 | Sepal.Width>3) #istedigimiz kodullari verdi.
# veya olarak kullandi

iris %>% select(Sepal.Length,Sepal.Width)%>%filter(Sepal.Length<5, Sepal.Width>3)
# sadece sselectteki sutunlar gelir

iris %>% select(Sepal.Length,Sepal.Width,Species)%>%
  filter(Sepal.Length<5, Sepal.Width>3) %>%
  group_by(Species) %>%
  summarise(
    Sepal.Widthmean = mean(Sepal.Width),
    Sepal.Lengthmean = mean(Sepal.Length)
  )
#belirledigim kosulalra gore diger gruplar yok. group by kullaninca sadece setosa geldi

##### mutate() donusum fonk######


iris %>% mutate(Sepal.Length = log(Sepal.Length)) # sepal length kl??sm?? logaritma
#alinmis sekilde gelecektir


iris %>% mutate(Sepal.Lengthlog = log(Sepal.Length)) # sepal length kl??sm?? logaritma
#yeni sutun olarak verdi

#degisken turu degistirme
df <- iris %>% mutate(Species = as.character(Species)) #karaktere dondu
class(iris$Species)#factor 
class(df$Species) #karakter yaptik


iris %>% mutate_if(is.numeric, function(x){ x*10 })
# numeric degerler ise al 10 ile carpar burda mesela


iris %>% mutate_if(is.numeric, log)
#numeric ise logaritma al demek 

x <-  function(x){
  return(x/10)
}

iris %>% mutate_if(is.numeric, x) 
#butun degerleri 10a boldu biz yaptik fonksiyonu





####### odev ##########

FRvideos %>% filter(views > 3000000) %>% group_by(channel_title,category_id)%>%
  summarise(dislikemean = mean(dislikes),
            likemean = mean(likes),
            viewsmean = mean(views))







