
#####pearson kolerasyon testi#####

# iristeki sepal width ile petal widht aras??ndaki iliskiye bakal??m

attach(iris)

shapiro.test(Sepal.Width)
# H0: normal dag??lan veriler
# Ha: Normal dag??lmayan veriler
# p degerim = 0.10. H0 kabul edildi. normal dag??lan dedik


shapiro.test(Petal.Width)
# H0: normal dag??lan veriler
# Ha: Normal dag??lmayan veriler
# p degerim = 0.000000... H0 reddedildi. Ha kabul edildi normal dag??lmayan veriler


hist(Petal.Width)
# normal dag??l??ma yak??n da de??il kabul etmedik bunu

# yeni degiskene bakal??m

shapiro.test(Sepal.Length)
# yaklasik normal dagilim  grafikle kontrol edelim

hist(Sepal.Length)
# hemen hemen normal dagilim pearson kolerasyon yapal??m



cor(Sepal.Width,Sepal.Length, method = "pearson")
# ald??g??m??z deger -0.11 c??kt??
# -1/1 aras??nda degerler al??rd??. 0/-1 aras??ndaysa aralar??nda ters,
# 1/0 aras??ndaysa d??z oran vard??
# yani burda ters iliski var. sepal length degerlerim artt??kca sepal width azal??yor
# ya da sepal width artt??rkca sepal legnth degerlerim azal??yor. ama ters orant??l??
# iliski az seviyede. anlamli bi iliski mi kontrol edelim


cor.test(Sepal.Width,Sepal.Length, method = "pearson")
# H0: korelasyon 0a esittir
# Ha: korelasyon 0a esit degildir
# p degerim 0.15 yani korelasyon yoktur diyor. bu iki degisken aras??nda anlaml??
# bir iliski yoktur dedik



cor(iris[,1:4], method = "pearson")
# korelasyon matrisi. her iki degerin korelasyon degerlerini vermis 


cor.test(Sepal.Width,Sepal.Length, method = "pearson", alternative = "less")
# H0: kolerasyon sifirdan buyuk veya esittir
# Ha: kolerasyon sifirdan kucuktur
# p-value : 0.07 kolerasyon 0a esit veya buyuktur

cor.test(Sepal.Width,Sepal.Length, method = "pearson", alternative = "greater")
# H0: kolerasyon sifirdan kucuk veya esittir
# Ha: kolerasyon sifirdan buyuktur
# p-value 0.92 kolerasyon 0dan kucuk veya esittir. p degerim cok yuksek


 detach(iris)



###### kendall rank kolerasyon testi######


# normal dagilmayan verilerde kullanilir

attach(iris)


hist(Petal.Width)
# carpik dagilim gosteriyor

shapiro.test(Petal.Width)
# normal dagilmayan veriler



hist(Petal.Length)
# carpik gosterim duruyor

shapiro.test(Petal.Length)
# normal dagilmayan veriler

cor(Petal.Width,Petal.Length)
# 0.96 yani 0/1 arasinda ve 1e cok yakin aralarinda duz bi oranti kesinlikle var
# biri arttikca digeri de artar

cor.test(Petal.Width,Petal.Length, method = "kendall")
# H0: aralarindaki iliski 0dir. yani yoktur.
# Ha: aralarindaki iliski 0dan farkl??d??r, yani vardir
# p degerim 0a cok yakin aralarinda iliski oldugunu bulduk


cor.test(Petal.Width,Petal.Length, method = "kendall",alternative = "less")
# H0: aralarindaki iliski 0 veya daha buyuktur
# Ha: aralarindaki iliski 0dan kucuktur
# p-value 1 geldi yani kesinlikle 0 veya daha buyuktur dedik
# ustte 0 m?? diye sormustuk tam 0 dememisti. buyuktur dedik yani
# aralarindaki iliski 0.80 dedi kendall


cor.test(Petal.Width,Petal.Length, method = "pearson",alternative = "less")
# burda da p degerleri fln ayni da aralarindaki iliski 0.96 dedi

# hangisi dogru peki?

# normal dagilmayan veriler oldugundan kendall daha guvenilir olur.


cor(iris[,1:4], method = "kendall")
# aralarindaki iliskileri veriyor

detach(iris)
###### spearmen kolersyon testi #####


# veriler ordinal ise kullanilir, seviyeli ise yani

# bi anket yapt??k. x ve y ??kisker soru ve onlara verilen cevaplarin puanlarini giriyoruz


x <- c(2,3,4,1,1,3,4,5,1,2,2,3)
y <- c(3,4,4,2,1,2,5,3,1,3,3,2)

# iliski kura kura verdik
# 1-5 arasi seviyedir 1 en az 5 en cok seklinde


cor.test(x,y,method = "spearman")

# H0:aralarindaki iliski 0dir
# Ha: aralarindaki iliski 0dan farklidir, vardir yani
# p degerim 0.01 H0 reddedildi Ha kabul edildi
# 0.69 degerinde aralarinda fark varmis
# bir de cor ile bakalim

cor(x,y,method = "spearman")
# 0.69 degerinde aralarinda fark varmis
# 1/0 aras??nda bi deger yani xe verilen yanitin degeri arttikca
# y'ye verilen yanitimn degeri de artiyormus


cor(iris[,1:4], method = "spearman")
# spearman kolerasyon matrisi

attach(iris)

plot(Sepal.Length,Sepal.Width)
# lineer bi iliski yok, karmas??k bi yap??da verdi. yani iliski yok

plot(Petal.Length,Petal.Width)
# lineer bi iliski var. biri artarken digeri de artar seklinde


##### kovaryans hesaplama #####



# varyans degisim katsayisiydi
# kovaryans iki degiskenin birlikte ne kadar degistigini gosterir
# bi siniri yok ne kadar fark varsa o kadar artar
var(Sepal.Length)
# degisim katsayisi 0.68

var(Sepal.Width)
# degisim katsayisi 0.18

cov(Sepal.Length,Sepal.Width)
# -0.04 cikti. ters bi kovaryans. birlikte degisim katsayisi cok az ve ters yonde


cov(Petal.Length,Petal.Width)
# kovaryans degerim 1.29 geldi. 
# yani ikisinin birlikte degisimi 1.29













