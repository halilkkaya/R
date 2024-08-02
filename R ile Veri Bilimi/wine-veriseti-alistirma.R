
##### wine veri seti#####

wine$Alcohol

attach(wine)
shapiro.test(Alcohol[X==1])
# normal dagilim gosteriyor. ilk sarabimiz
shapiro.test(Alcohol[X==2])
# 2. de normal dagilim
shapiro.test(Alcohol[X==3])
# 3.de normal dagilim

wine %>% group_by(X) %>% summarise(shapiro.test(Alcohol)$p.value)
# her bir grubun nomrallik testini veriyor


library(tidyverse)
df <- wine %>% filter(X==1|X==2)
detach(wine)

attach(df)

bartlett.test(x = Alcohol,g = X)
# H0: homojen varyanslilik gosterir
# Ha: homojen varyanslilik gostermez
# p-value 0.22 yani homojen varyanslilik gosterir

class(X)
t.test(df$Alcohol~df$X, mu = 0)
# H0: iki grubun ortalamasi arasindaki fark 0dir
# Ha: iki grubun ortalamasi arasindali fark 0dan farklidir
# p degerim cok duusk yani ortalamasi 0dan farkli dedik.
# peki hangi grup daha alkollu ona bakalim

t.test(df$Alcohol~df$X, mu = 0,alternative='less')
# HO: grup1 ortalamasi grup2 ortalamasindan farki 0dan buyuk veya aynidir
# Ha: grup1 ortalamasi grup2 ortalamasindan farki 0dan kucuktur
# p degerim 1 geldi yani kesimlikle esit veya buyuk dedik


# ilk testte esitligi sorduk ve degil dedi ikinci testte de buyuk veya esittir dedi
# yani grup 1in alkol orani grup ikiden fazladir diyebilirz
# grafikler cizelim


hist(Alcohol[X=="1"])
hist(Alcohol[X=="2"],probability = T,main = "Cielo Sarap Alkol Oranlar?? ve Ygunluklar??",
     xlab = " Cielo Saraplar?? Alkol Oranlar??",
     ylab = "Yogunluk",col = "orange")

lines(density(Alcohol[X=="2"]),col = "black",lwd = 5)

bir <- Alcohol[X=="1"]
iki <- Alcohol[X=="2"]


cor.test(bir,iki,method = "pearson")
# esit uzunlukta degiller


esitiki <- sample(iki,59)


cor.test(bir,esitiki,method = "pearson")
# rastgele orneklem alip ikisini de ayni sayida ornekleme getirdim.
# H0: aralarindaki iliski 0dir yani birbirlerini etkilemiyorlar
# Ha: iliski vardir birbirlerini etkilerler
# mantik olarak bu veriler zaten 2 farkli grubun saraplarina ait ve birbirlerini etkilememeleri gerekir.
# p degerim 0.2 cikti ve H0 kabul edildi. aralarinda iliski yok 


cov(bir,esitiki)
# aralarindaki degisimi bulur kovaryans -0.04 baya dusuk zaten


names(wine)[1] <- "Groups"
names(wine)
# X ismi karisik oluyodu Groups olarak degistim


detach(df)
attach(wine)

wine %>% group_by(Groups) %>% summarise(shapiro.test(Magnesium)$p.value)
# 1. grup normal dagilan  
# 2. grup normal dagilmayan
# 3. grup normal dagilan


wine %>% filter(Groups==1|Groups==3) %>% group_by(Groups) %>% summarise(t.test(Magnesium,mu = 100,alternative = "less")$p.value)
# H0: grup ortalamasi 100 veya daha buyuktur
# Ha: grup ortalamasi 100den kucuktur
# p degerlerim ilk grup icin 1.00 ikinci grup icin 0.33 geldi
# ilk grup H0 kabul edildi 100den buyuk veya esit ortalamasi
# ikinci grup H0 kabul edildi 100den buyuk veya esit ortalamasi


wilcox.test(Magnesium[Groups==2],mu = 100, alternative = "less")
# normal dagilmayan gruptu 2. grup
# H0: ortanca deger 100den buyuk veya esittir
# Ha: ortanca deger 100den kucuktur
# p degerim 0a cok yakin Ha kabul edildi 100den kucuk cikti 



cor.test(Alcohol,Magnesium,method = "pearson")
# alkol ile magnezyum arasindaki iliskiye baktik
# H0: aralarindaki iliski 0dir yani birbirini etkilemezler
# Ha: aralarinda iliski 0dan farklidir. yani birbini etkilerler
# p degerim 0.0002 cikti H0 reddedildi. birbirini etkilerler dedik
# cor degerim 0.27 geldi
# 0/1 arasinda bulundugundan dolayi bu etki pozitif etkidir
# alkol orani arttiginda magnezyum orani da artar diyebiliriz.



hist(Alcohol[Groups==1], probability = T,
     col = "orange",
     xlab = "Alkol Orani",
     ylab = "Yogunlugu",
     main = "Alkol orani ve yogunlugu",border = "blue")
lines(density(Alcohol[Groups==1]),lwd=5)
# histogram grafigi cizdirdik yogunluga gore

boxplot(Alcohol~Groups)
# aykiri degerleri falan inceleyebiliyoruz. ozellikle 2. grupta bulunuyor



barplot(height = table(Groups),names.arg = c("1. grup","2. grup","3. grup"),ylab = "sarap sayisi",
        col =c("pink","red","white"),main = "Gruplara gore alkol sayisi",legend.text = c("grup 1","grup 2","grup 3"),
          axis.lty = 1,)










