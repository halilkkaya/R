##### min max normalizasyon islemi ######

# verileri 0-1 arasina sikistiriyoruz

(x - min(x))/ (max(x)- min(x))
# min max normalizasyon formulum
# indisteki ilk sayiyla mininimum sayiyi cikariyor sonra onu maximum sayi ve minimum sayi
# farkina boluyor. formul budur.
# mesela c(2,3,1,5,6,7) x=2, min(1), max(7)

b <- c(2,3,1,5,6,7)
c <- numeric(length = 6)
for (i in 1:length(b)) {
  
  res <- (b[i] - min(b))/(max(b)-min(b))
  c[i] <- res
}
c
minMaxNorm(b)
# bi deneme yaptim ayni sonuclari aldim fonksiyonu asagida olusturmustuk

minMaxNorm <- function(x){
  a <- (x - min(x))/ (max(x)- min(x))
  return(a)
}
minMaxNorm(df$cdur)
# 0-1 arasina normalize islemi yaptik

min(minMaxNorm(df$cdur))
# 0
max(minMaxNorm(df$cdur))
# 1
# 0 ile1 arasina yerlestirdik yani

df$cdur <- minMaxNorm(df$cdur)
df$vdur <- minMaxNorm(df$vdur)
df$wordfreq<- minMaxNorm(df$wordfreq)
# hepsini 0-1 arasina yerlestirdik
# 100 ile carparsak 0-100 arasina koyariz
df

df$cdur <- minMaxNorm(df$cdur)*100
df$vdur <- minMaxNorm(df$vdur)*100
df$wordfreq<- minMaxNorm(df$wordfreq)*100
# 0-100 arasina aldik


##### scaling (olceklendirme- standartlastirma) islemi #####

?scale 
# x numeric matrix 
# center ortalamsi
# scale standart sapmasi ikisi de logical deger true dersek verdigimiz
# verilerdeki o verileri kullancak

scale(df$vdur,center = T,scale = T)
# standartlastirilmis verilerimiz
# 
mean(df$vdur)
sd(df$vdur)
# scale fonk icindeki verilerle ayniymis

# yani verimizin ortalama degerine ve standart sapma degerine gore 
# standartlastirma islemi yaptigini soyleyebiliriz

scale(df$vdur,center = 50,scale = 5)
# centeri 50 scalei 5 olan standartlastirilmis verileri olusturduk

# neden bu sekilde fonk verildi peki
# modelimizi yaptik diyelim modelimizdeki degiskenler
# bu sekilde scale yapilmis degiskenlerdir
# ve ardindan bi veri geldi ve veriden tahmin yapicaz
# veriyi once scaleden gecirmek gerekiyor. bu yuzden verinin
# kendi ortalama standart sapmasini kullanilmasi gerekiyor




par(mfrow=c(1,2))
hist(df$vdur)
hist(scale(df$vdur))

# ufak tefek degisiklikler var ama bu veriyi degistirmek degildir
# sadece standartlastirmadir. veriler degismez


###### aykiri deger tespit yontemleri ######

# boxplot yontemiyle

library(rstatix)


identify_outliers(as.data.frame(AirbnbNewyork$price))
## bayyaa cok veri verdi bazilari ekstrem hatta o da yaziyo

out <- identify_outliers(as.data.frame(AirbnbNewyork$price))

names(out)

min(out$`AirbnbNewyork$price`)
# min deger 335

max(out$`AirbnbNewyork$price`)
# 10000 max deger

min(AirbnbNewyork$price)
max(AirbnbNewyork$price)
# direkt tablodaki min ve max degerlerim

# min degerimiz normal tablomuzda 0 geld
# out verisinde de 335di yani aslinda 335 altindakiler normal veri 
# ustundekiler de aykiri deger mantigini kullaniyoruz

ids <- which(out$is.extreme== TRUE)
# ekstrem degerlerin indislerini aldik
ekstrem <- out[ids,"AirbnbNewyork$price"]
# ekstrem degerlerim

min(ekstrem)
# 495
max(ekstrem)
# 10000

# ekstrem deger araligimdir

library(tidyverse)

df_clean <- AirbnbNewyork %>% filter(price<335)
# aykiri degerleri attik

p <- numeric(length = 100)
for (i in 1:100) {
  deger <- sample(df_clean$price,size=100)
  test <- shapiro.test(deger)
  pvalue <- test$p.value
  p[i] <- pvalue
}
mean(p)
# normallik testi yaptik normal cikmadi
# p degerim 0a cok yakin

hist(df_clean$price)
# harbiden saga carpik yani veriler solda togunluklu bi tablomuz var
# neyse burasi oylesine deneme icin yaptim 


