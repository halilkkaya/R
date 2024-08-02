##### binom testi #####

?binom.test

dbinom(x = 5,size = 10,prob = 0.7)
# bu olasilik testlerindendi 10 denemeden her biri icin
# %70 basarili olma durumu iceren bi yerde tam 5 tane basarili
# gelme olasiligi 0.1 yani %10

Sys.setlocale("LC_ALL","tr_TR.UTF-8")

soru1 <- SPSS_Gorus_Anketi$`Mesle??im gere??i istatistik yaz??l??mlar??n?? kullanmay?? ????renmem gerekir.`

table(soru1)
# hangi cevaplar kac kere verilmis onu gorduk.

soru1_binom <- soru1[soru1 =='Kat??l??yorum']
soru1_binom <- append(soru1_binom, soru1[soru1=='Kat??lm??yorum'])

table(soru1_binom)
# kat??lan ve kat??lmayan say??lar??n?? ald??k



binom.test(x=24,n=30, p=0.5)

# H0: kat??l??yorumlar 0.5e e??ittir
# Ha: kat??l??yorumlar 0.5e e??it de??ildir


# p degeri 0.001 geldi. yani kat??l??yorumlar orani 0.5e e??it de??il dedi yani
# ba??ar??l?? olma olas??l??????m  0.6143335 0.9228645 ????kt??
# yani kat??l??yorum alma olas??l??????m 0.6-0.9 aras??nda.
# sosyoloji ????rencileri 0.6-0.9 olas??l??kla mesle??i gere??i istatistik yaz??l??m??
# ????renmesi gerektiri??ini d??????n??yor demektir bu. 




binom.test(x=24,n=30, p=0.5, alternative = 'greater')
# H0: kat??l??yorum oran??n??n 0.5 veya daha k??c??k olmas??
# Ha: kat??l??yorum oran??n??n 0.5den buyuk olmas??
# p degerim 0.0007 geldi yani H0 reddedildi, Ha kabul edildi
# Kat??l??yorum olma oran??m 05den buyuktur 



binom.test(x=c(24,6), p=0.5, alternative = 'greater')
# burda da basarili 24 basarisiz 6 seklinde verdik diger islemde
# basarili 34 toplam 30 demistik



##### ki kare iyi uyum testi/ chi quare goodness of fit #######
# 2den fazla nominal degerler icin kullanilir
# binomde 2 tane varsa kullaniliyordu

# ki kare e??er 2 kategori beklenen de??erler en az 5 olmal??

library(readxl)

soru1 <- SPSS_Gorus_Anketi$`Mesle??im gere??i istatistik yaz??l??mlar??n?? kullanmay?? ????renmem gerekir.`
table(soru1)  

sonuc <- chisq.test(table(soru1))
# frekans tablosunu girdik

# H0: beklenen de??erler g??zlemlenen de??erlere e??ittir
# Ha: beklenen de??erler g??zlemlenen de??erlere e??it de??ildir
# p degerini 0.002 verdi 
# H0 reddedildi Ha kabul edildi

sonuc$expected
# beklenen degerlerim 11-11-11-11-11 geldi

sonuc$observed
# kat??l??yorumda yo??unla??ma var.

sonuc2 <- chisq.test(table(soru1), p=c(0.1,0.2,0.5,0.2,0.1))
# toplam?? 1e esit olmal??yd??

sonuc2 <- chisq.test(table(soru1), p=c(0.1,0.2,0.5,0.1,0.1))
sonuc2$observed # s??ras??yla 5-11-24-6-9
sonuc2$expected # s??ras??yla 5.5-11-27.5-5.5-5.5
sonuc2 # p degerim 0.6 geldi yani beklenen degerlerim gozlenen degerlere esit dedi
# kendimiz kontrol ettigimizde de yakin sonuclar oldugunu goruyoruz



#  varsay??malar
sonuc3 <- chisq.test(table(soru1), p=c(0.05,0.2,0.5,0.15,0.1))
# hata mesaji verdi. ki-kare yanl???? sonuc verdi diyor.
# sebebi iste beklenen de??erlerin herhangi biri 5den kucuk olmamal??
# burda 1 tanesi 5den kucuk hale geldi
sonuc3$expected
# 2.75 var. test hatal?? yani


##### kategorik iki degiskenin frekans tablosu uzerinden iliskisinin yorumlanmasi#####


df1 <- data.frame(x=SPSS_Gorus_Anketi$`Mesle??im gere??i istatistik yaz??l??mlar??n?? kullanmay?? ????renmem gerekir.`,
                  y=SPSS_Gorus_Anketi$`Sosyal bilimlerde istatistik hesaplamalar??n??n ve istatistiksel yaz??l??mlar??n gerekli oldu??unu d??????n??yorum.`)

table(df1$x,df1$y)
# bir frekans tablosu yaptik data.frame olarak geldi. 
# sat??r sat??r olan sorular x sutunlarda olan sorular ise y k??sm??na ait sorular.
# bunlar??n kesisim soruclar??n?? verdi

# ki kare ile bu tabloyu degerlendirelim bakal??m. ili??ki var m?? aralar??nda


prop.table(table(df1))
# buran??n toplam??1 veriliyor. oranlar?? verdi. hangisinin kac oranda gelecegi oranlar??

# beklenen deger su sekilde olculur
# hesaplamak istedigimiz degerin satirindaki tum degerleri toplami * sutunundaki tum degerleri toplami
# df[1,1] icin ornek yapalim ilk satir ilk sutunda buluyor deger
# ilk satir degerler toplami 5
# ilk sutun degerler toplam?? 2
# 2*5=20
# tablodaki tum degerlere bolunur
# tum degerler = 55
# 10/55 beklenen deger

tbl <- table(df1)

sonuc <- chisq.test(tbl)
# uyar?? ald??k beklenen de??erlerin %20dinden fazlas?? 5den k??c??kse yanl???? cevap al??rs??n diye uyar?? verdi
# beklenen degerlerimiz 5den kucuk 
# toplam 25 degerimiz var %20si 5 yapar 5den fazla 5 degerinden kuck degerimiz oldugu icin
# uyari mesaji aldik. 

# H0: ??ki de??i??ken birbirinde  ba????ms??zd??r. ili??ki yoktur
# Ha: ??ki de??i??ken birbirine ba????ml??d??r. ili??ki vard??r
sonuc$p.value
# p degerim 0a cok yak??n yani H0 reddedildi Ha kabul edildi.
# sonuc olarak ilk soruya verilen cevapla ikinci soruya verilen cevap birbirini etkiliyor
# diyoruz

# bir de kendimiz inceleyelim

sonuc$observed
# gercek degerler

sonuc$expected
# beklenen degerler


# df 16 geldim ki karede toplam gozlem -1 degil burda hesaplama yontemi su
# ka?? kategori var? 5 tane 5-1=4
# digerinde kac kategori var? 5 tane 5-1=4
# 4*4=16 boyle bulunuyor







###### odds ratio ve fishers exact testi #####




tbl

# fisher exact genellikle 2x2 tablolarda kullan??l??r.
# ki karedeki 5den fazla olma xorunlulugundan dolay?? yap??lamayan verilerde kullan??labilen bbi yontemdir


library(tidyverse)

df2 <- df1 %>% filter(x %in% c('Kat??l??yorum','Kat??lm??yorum') & 
                      y %in% c('Kat??l??yorum','Kat??lm??yorum') )

head(df2)
# sadece kat??l??yrum ve kat??lm??yorumlar?? ald??k
# 2x2 istiyodu cunku

table(df2)

# 2x2 odds ratio
fisher.test(table(df2))
# H0: odds ratio 1e e??ittir
# Ha: odds ratio 1e e??it de??ildir
# p degeri 0.48 h0 kabul edildi
# odds ratio ne demek?
# pozitif durumlar negatif durumlara gore kac kat daha fazla gerceklestirebilir
# pozitifler kat??l??yorum-kat??l??yorum ve kat??lm??yorum-kat??lm??yorum oluyor
# negatifler kat??l??yorum-kat??lm??yorum ve kat??lm??yorum-kat??l??yorum oluyor

# yorumu ilk soruya ve ikinci soruya ayn?? cevab?? verenlerin say??s??
# ilk soruya ve ikici soruya farkl?? cevab?? verenlerden kac kat fazla olabilir
# iliski baz??nda ne deriz. odds ratio ne derse iliski yorumu yapabilirz
# odds ratio 1den buyukse pozitif yonde iliski
# odds ratio 1den kucukse negatif yonlu bi iliski oluyor
# pozitif ve negatif durumlari yukarida anlattim
# bizim problemde pozitif iliski var diyebiliriz

fisher.test(table(df2),alternative = 'less')
# H0: odds ratio 1 veya 1den buyuk
# Ha: odds ratio 1den kucuk
# p degeri 0.93 H0 kabul edildi

fisher.test(table(df2),alternative = 'greater')
# H0: odds ratio 1 veya 1den kucuk
# Ha: odds ratio 1den buyuk
# p degeri 0.48 yan?? %48 1 veya 1den kucuk %52 1den buyuk dedik




# 5x5
fisher.test(table(df1))
# sonuc 0.001 verdi. 
# 2x2 den fazla oldugu icin odds ratio vermez bu cevabi su hiptozlere gore yan??tlar??z
# H0: ??ki de??i??ken birbirinde  ba????ms??zd??r. ili??ki yoktur
# Ha: ??ki de??i??ken birbirine ba????ml??d??r. ili??ki vard??r
# H0 rededdildi Ha kabul edildi. arada iliski var dedik




###### bagimli orneklemler icin McNemar testi ####

# hangi durumlarda kullanilir?
# 2x2 tablolarda kullanilir
# fisher exact ile farki ne?
# orneklemler bagimliyse McNemar kullanilmasi gerekir
# hastam??z var hastalar??n once ve sonraki durumlar?? gibi dusun
# ilac vermeden once ilac verdikten sonra gibi



# once dedigimiz ilac vermedik
# sonra dedigimiz ilac verdik
# ikisinde de durumlar??n??z nas??l evet iyi hay??r kotu anlami tasiyor
mcData <-  data.frame(
  once = c('Evet','Hay??r','Hay??r', 'Evet','Hay??r','Hay??r'),
  sonra = c('Evet','Hay??r','Evet','Hay??r','Evet','Evet')
)

tablo <- table(mcData)


tablo


mcnemar.test(tablo)
# H0: ??nce ve sonra aras??nda herhangi bir de??i??im olmam????t??r
# Ha: ??nce ve sonra aras??nda bir de??i??im olmu??tur
# p degerim 0.6
# H0 kabul edildi. de??i??im yok dedi

evet <- rep('Evet',50)
hay??r <- rep('Hay??r',50)

birles <- append(evet,hay??r)

karistir1 <- sample(birles, replace = T)


karistir2 <- sample(birles, replace = T)

deneme <- data.frame(once=karistir1,sonra=karistir2)

# test icin 100 verilik bi veri olusturdum

deneme

deneme_test <- table(deneme)

deneme_test

mcnemar.test(deneme_test)
# H0: ??nce ve sonra aras??nda herhangi bir de??i??im olmam????t??r
# Ha: ??nce ve sonra aras??nda bir de??i??im olmu??tur
# p degerim 1 yani bu oylesdine olusturdugum verilerde degisim olmamis dedik

# guzel bi uygulama yaptik oylesine, genel tekrar da oldu onceki konulara 
          


