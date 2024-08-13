###### ANOVA TESTI#####
install.packages('rstatix')
library(rstatix)
library(tidyverse)

# ayk??r?? degerleri buluyor bu fonksiyon
outliers <- identify_outliers(heart['chol'])
outliers
# bunlar aykiri degerler. bunlari veriden cikaralim


# outliers degiskeninden gordgumuze gore en dusuk aykiri deger 394.
# biz de 394den buyuk olan degerleri bu sekilde silip att??k
df <- heart %>% filter(chol < 394)
df


max(df$chol)
# max degerim 360 oldu. aykirilari attik

# burada yaslara gore gruplara ay??rd??k verilerimizi
df_new <- df%>% group_by(
  age_groups=cut( age, breaks = seq(min(age)-1, max(age)+1, length.out = 4)))
#min(age)-1 veya max(age)+1 dememizin sebebi o yaslari da dahil etmeye calismamiz

View(df_new)

#yas gruplar??n?? kontrol ettik
levels(df_new$age_groups)

# varsayim testlerimizi yapalim
# yas gruplarimiz var. acaba yas gruplari arasindaki chol farki nasildir.
# yas gruplarinin kolestrolleri ne durumda bakmakdi amacimiz hatirlayalim
# bu gruplarin ortalama farklarina bakcagiz


df_clean <- df_new %>% select(chol,age_groups)
View(df_clean)


leveller <- levels(df_clean$age_groups)



shapiro.test(df_clean$chol[df_clean$age_groups==leveller[1]])
# ilk levelimiz normal dagilim
# p degerimiz 0.05

shapiro.test(df_clean$chol[df_clean$age_groups==leveller[2]])
# ikinci levelimiz de normal dagilim
# p degerimiz 0.65

shapiro.test(df_clean$chol[df_clean$age_groups==leveller[3]])
# ucuncu levelimiz de normal dagilim
# p degerimiz 0.99

# bu uc sekilde yazma benimdi hoca tidyverse kullanarak hizli sekilde test edecekmis
# biz de ogrenelim

df_clean %>% 
  group_by(age_groups) %>% 
  summarise(shapiroRes = shapiro.test(chol)$p.value)

#  age_groups  shapiroRes
#<fct>            <dbl>
#  1 (28,44.7]       0.0590
#2 (44.7,61.3]     0.657 
# 3 (61.3,78]       0.995 
# seklinde sonuc aldik. cokkkkk basitmisss laaaaa
# ne yaptik? once gruplama islemi yaptik. daha sonra iceriye summarise islemi gerceklestirdik.
# oraya istedigimiz fonksiyonu yazik gerceklestiredik. summarise fonk icinde sadece 
# bir deger doner bu yuzden p.value istedik sadece
# her bir gruplama iicn p degerimizi verdi



boxplot(df_clean$chol~df_clean$age_groups)
# bi inceledik ole bakim dedim

# degerler normaldi o yuzden bartlett kullandik
# bagimlidegisken~bagimsiz degisken seklinde yazicaz ha

bartlett.test(df_clean$chol~df_clean$age_groups)
# H0: homojen varyansd??r
# Ha: homojen varyans degildir
# p degerimiz 0.97 yani homojen varyanslidir

library(car)

leveneTest(df_clean$chol~df_clean$age_groups)
# p degeri daha yuksek cikti. normallik konusu cok onemli degil leveme icin o yuzden
# p degeri daha yuksek geliyor

# varsayimlarimizi tammaladik. neler yaptik gozden gecirelim


# 1) verilerimizi yas gruplarina ayirdik.
# 2) sadece age_groups ve chol sutunlarini sectik
# 3) tidyverse ile kolay sekilde normallik olcmeyi ogrendik ve olctuk
# 4) homojenlik olcuk ve homojen cikti. tum kosullar saglandi. artik anova testine haziriz



# istersek r icinde istersek de rstatix paketindeki anovayi kullamabiliriz

# anova testi

anov <- aov(df_clean$chol~df_clean$age_groups)
anov
# between grup seklinde hesaplanan bi uygulamadir.
# deg of freedom grup-1dir ve 2 verdi 3-1den.
# sum of squares yani kareler toplami. 19345 cikmis
# residuals degerler yani artik degerlerimiz de var. artik degerlerin
# de normalligi bulunabilir. eger artiklar da normal dagiliyosa bu 
# gerceklesmis anova testidir der bazi kaynaklar
# Residual standard error yani residuallar??n standartlastirilmis hatalari
# bulur. 


shapiro.test(anov$residuals)
# residuls degerleri de normalmis

summary(anov)
# tum degerleri verdi. df verdi sumsq verdi meansq verdi p degeri verdi pr yazan
# p degerini yorumlayalim hadi
# H0: butun gruplar birbirine esittir
# Ha: en az bir grup digerlerinden farklidir
# p degerim 0.008 geldi. H0 reddedildi Ha kabul edildi. en az biri farklidir
# incelemeyle hangisi farkli bakcaz

TukeyHSD(anov)

df_clean %>% group_by(age_groups) %>% 
  summarise(ortalamalar = mean(chol, na.rm = T))
# ortalamalara bakiyoruz. 
# hepsi birbirinden farkli ama 2 tanesinin farki az.
# hepsi farkli da demis olabilir. 1i farkli da demis olabilir bunu da 
# 2 orneklem t-testinden bakicaz


levels(df_clean$age_groups)

grup1 <- df_clean %>% filter(age_groups=="(28,44.7]")

grup2 <- df_clean %>% filter(age_groups=="(44.7,61.3]")

grup3 <- df_clean %>% filter(age_groups=="(61.3,78]")


tukey_hsd(df_clean_df, chol~age_groups)


t.test(grup1$chol,grup2$chol,mu = 0,var.equal = T)
# esit degil dedi. p degeri 0.01
t.test(grup2$chol,grup3$chol,mu = 0,var.equal = T)
# esit dedi p degeri 0.35
t.test(grup1$chol,grup3$chol,mu = 0)
# esit degil dedi p degeri 0.003



#grup1 != grup2 == grup3
# yani tek grubun esit olmamasi var anova testi icin

# rstatix ile gelen anova testi
class(df_clean)
df_clean_df <- as.data.frame(df_clean)
# data.frame olmak zorundaydi dondurduk

anova_test(df_clean_df,dv = chol, between = age_groups)
# p degerim yine 0.008 geldi.
# dv hangi sutuna gore yapcagimizi
# between de gruplarimizi yazioz. within olsa ona yazcaktik aynisini
# effect size yani ges 0.032 verdi

summary(anov)
# effect size yani ges nedir?
anova_test(df_clean_df,dv = chol, between = age_groups)
# ges 0.032

# anlami bagimsiz degiskenimiz bagimli degiskenimizi ne kadar etkiliyor
# onu veriyor
# yani yas gruplari kolestrolu ne kadar etkilemis onun oran??n?? soyluyor
# yas gruplar?? kolestrolu 0.03 etkiliyor
# https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize#:~:text=The%20general%20rules%20of%20thumb,squared%20than%20to%20eta%2Dsquared.
# bu internet sitesinde 0.01 az 0.06 orta 0.14 cok etkiliyor diyor.
# diger testler icin de ne anlama geldigi yaziyor. dursun site

# bizim ges 0.03 geldi yani az ile orta arasinda bi etki bitakmis bizde


####### bagimli orneklemler- tekrarli olcumler ANOVA####

install.packages("ggplot2")
install.packages('ggpubr')
library(ggpubr) # veri gorsellestirme icin kullancakmisiz
library(tidyverse)
library(rstatix)
# gerekli kutuphaneler


# within kullancaz. bagimli degisken kullancaz


final <- DesignTest %>% gather(key ='Groups', value = 'Scores',
                      ADesign,BDesign,CDesign) %>% 
            convert_as_factor(Groups)
View(final)
# bu kod ile her bir verinin yanina adesign bdesing cdesing yazdik, gruplara ayirdik

?ggboxplot
ggboxplot(final,x='Groups',y='Scores', color = 'orange')


anova_test(final,dv = "Scores", within = 'Groups')
# hata verdi wid degeri yani within id degeri girmek zorundasin dedi
# within id nedir? 1. satirdaki adesing ile 101. satirdaki bdesing kisisi ayni mesela
# bunlari birlestirmek icin kullanilir. yoksa hangisi kim bilmiyor


ids <- c(rep(seq(1,100),3))
final$id <- ids
final
# 1den 100e kadar a b c seklinde sirasiyla gidiyordu. 3tane grubumuz var rep ile
# 3 kere tekrar ettirilmeli 1den 100e kadar id yazdirdik ve bunu yeni sutun
# olarak ekledik. burdaki idleri kullancaz simdi 

anova_test(final,dv = "Scores", within = 'Groups',wid = 'id')
# ilk satir anova testinin sonucalrini veriyor. 
# H0: adesing = b desing = c desing
# Ha: en az biri farkli
# p degerim sifira cok yakin yani en az biri farkli cikti
# ges degerim 0.91 yani etkileme yani gruplar scoru nasil etkiliyo
# cok cok cok yuksek verdi yani dizaynlar aldiklari puani etkiliyor demektir bu


# ikinci ve ucuncu satirda farkli sonuc var
# kureselligi ??l????yo kuresellik testleri de denebiliyor
# f skoru uzerinden yazpilir
# ayri tetsler hepsnin ayri p degerleri fln var duzeltme yapip oyle 
# sonucluyolar. f skoru duzeltmesitle yapiyo. 

##### Post Hoc testleri ikili karsilastirmalar####

outliers <- identify_outliers(heart['chol'])
outliers

df <- heart %>% filter(chol<394)
View(df)

df_new <- df %>% select(age,chol)


df_clean <-  df_new%>% group_by(
  age_groups=cut( age, 
                  breaks = seq(min(age)-1, max(age)+1, length.out = 4)))

view(df_clean)
df_clean <- as.data.frame(df_clean)


anova_1 <- aov(df_clean$chol~df_clean$age_groups) 
anova_2 <- anova_test(df_clean,  dv= 'chol',  between = 'age_groups',)
summary(anova_1)
anova_2
# simdi en az biri esit degil diye bulduk p degeri 0.008 cunku
# simdi ahngi gruplar esit degil onu bulma testi yapicaz ve farkinm anlamina bakcaz
# r icerisindeki bulunan fonk ile deneyelim oncelikle
 
TukeyHSD(anova_1)
# gruplarin karsilastirmalarini verdi, g8uven araliklarini, p degerini verdi
# gencler ile orta yas arasindaki farkin p degeri 0.02 yani farkli demek
# yaslilar ile gencler arasindaki farkin p degeri 0.008 yani farkli demek
# orta yas ile yaslilar arasindaki farkin p degeri 0.62 yani farkli degil demek
# genc != orta yas = yasli hipotezimiz yani Ha kabul edilmistir ve saglamasi da budur



# rstatix icindekini deneyelim
tukey_hsd(anova_2)
# hata verdi. formul girmek lazim 
tukey_hsd(df_clean, chol~age_groups)
# sonuclari verdi yine usttekiyle ayni sonuclari verdi. p degerlerine kadar ayni












##### anova iki yonlu varyans analizi #####

library(tidyverse)
library(rstatix)

# varsayim testlerini yapalim normallik homojenlik fln
df <- heart %>% filter(chol<394)

# gruplari cp ve fbs olarak ayiirdim. 
# cp 3 fbs 1
# cp 3 fbs 0 gibi olcak yani
# ve hepsine tek bi islemde shapiro normallik testi yapcam
df %>% group_by(cp,fbs) %>% summarise(shap= shapiro.test(chol)$p.value)
# tum deegrler icin p degerim 0.05den yuksek geldi yani hepsi normal dagilim gosteriyor

bartlett.test(df$chol~interaction(df$cp,df$fbs))
# interaction ben??m * sandigim gorevi yap??yor. iki grubu aliyor
# p degerim 0.52 yani homojen varyansli diyoruz



anova1 <- aov(df$chol~as.factor(df$cp)*as.factor(df$fbs))
# interaction kullanilmiyo burda geleneksel yontem * kullanioz
summary(anova1)
# hepsini sirasiyla chol ile karsilastirmasini koymus yani sunu demek istedim
# ilk satir chol ile cp  
# ikinci satir chol ile fbs
# ucuncu satir chol ile cp ve fbs 
# sonuclarini veriyor
# hepsinin p degeri 0.05den yuksek. 
# anlamli bir farklilik yok demek bu. hepsini esit sayabiliriz


anova_test(df, dv = 'chol',between = c(cp,fbs))
# yine yakin p degerleri verdi.



# chol uzerinde yapmayacagiz o yuzden aykiri degerleri atmistik ya
# onlari geri alalim

aykiri <- identify_outliers(as.data.frame(heart$trestbps))
aykiri


df1 <- heart %>% filter(trestbps<172)

shapiro.test(df1$trestbps)
# normal degil p degeri 0.003



anova2 <- aov(df1$trestbps~as.factor(df1$cp)*as.factor(df1$fbs))
summary(anova2)



anova_test(df1, dv = 'trestbps',between = c(cp,fbs))

# burda trestbps degerini inceledik
# ilki olan cp ile trestbps arasinda anlamli bi farklilik cikti 
# yani p degeri dusuk cikti ges yuksek cikti birbirini etkileyebilir anlaminda
# ikincisi olan fbs ile trestnps anlamli farklilik cikmadi
# yani p degeri 0.05denm yuksek cikti esit sayabiliriz ortalamalarini
# ucuncusu ve istedigimiz kisim
# cp ve fbs ile trestbps karsilastirmasinda p degeri 0.5 cikti 
# yani aralarinda anlamli bir farklilik yok hepsi yakin deger dedi


# yani gruplar birlesince olusan 8 grubun trestbps icin bi onemi yokmus onu anladik


##### iki yonlu varyans analizinde post hoc testleri #####

# tukeyhsd testi 

 
aykiri <- identify_outliers(as.data.frame(heart$trestbps))

df1 <- heart %>% filter(trestbps<172)


int_groups <- apply(df1, MARGIN = 1, FUN = function(x){
  r <- paste0(x['cp'],' - ',x['fbs'])
  return(r)
})
int_groups
#her bir satirin hangi cp ve hangi fbse ait olduklar?? vector olarak yazd??k
 
df1$int_group <- int_groups
df1


ggboxplot(df1, x= 'int_group',y='trestbps', 
          title  = 'Boxplot Grafigi',
          color = 'orange')


df1 %>% group_by(cp,fbs) %>% 
  summarise(shap = shapiro.test(trestbps)$p.value)
# sadece cp 1 fbs 0 olan normal degil digerleri normal


bartlett.test(df1$trestbps~interaction(df1$cp,df1$fbs))
# p degerimiz 0.33 yani homojen varyanslilik saglaniyor


anova_1 <- aov(df1$trestbps~as.factor(df1$cp)*as.factor(df1$fbs))
summary(anova_1)
# cp tek basina trestbpse etki ediyormus. p degeri 0.01 cikti cunku
# fbs tek basina trestbpse etki etmiyormus. p degeri 0.09 cikti
# cp ve fps beraber trestbpse etki etmiyormus. yani ustunde bi etkisi yokmus yani
# ortalamalari esitmis diyebiliriz her bir grup icin. p degeri 0.49 cikti

TukeyHSD(anova_1)
# her bir grubu ayri ayri karsilastiriyor ve aralarindaki farki yaziyor. p degeri veriyor
# oncelikle her bi cp degerinin trestbps uzerindeki etkilerine bakiyor
# sonra fbs degerlerinin tresnbps uzerindeki etkisine bakiyor
# en son ise ikisinin kombinasyonlu halinin etkisine bakiyor
# p degerine gore hipotezlerimiz suydu
# H0: ortalamalar ayni, etki yok
# Ha: ortalamalar farkli, etki var.
# etkiden kasit nedir? yani benim cp degerim degistiginde trestbps icin farkli ortalama veriyorsa
# bu etki etmistir ve bunun p degeri 0.05den kucuk gelir


tukey_hsd(anova_1)
# bu da ayni degerleri verdi. rstatix paketindeki fonk bu

anova_test(df1, dv='trestbps',between = c('cp','fbs'))


##### non-parametric kruskal wallis testi #####

library(tidyverse)

# oldpeak kullancaz cp kullancaz 

heart %>% group_by(cp) %>% 
  summarise(shap = shapiro.test(oldpeak)$p.value)
# cp degeri 3 olan veriler disindakiler normallik gostermiyor


bartlett.test(heart$oldpeak~heart$cp)
# H0: homojen varyanslilik gosterir
# Ha: homojen varyanslilik gostermez
# p degerimiz 0a cokkkk yakin H0 reddedildi Ha kabul edildi.
# homojen varyanslilik gostermiyor


# iki varsayim da reddedildi 
# kruskal kullancaz ortalama degil medyan ile bakicaz ya da ceyrekliklerle


a <- kruskal.test(heart$oldpeak~heart$cp)
# H0: gruplardaki medyanlar ayni, grup etkisi yok
# Ha: gruplardaki medyanlar farkli, grup etkisi var
# p degerim 0a yak??n geldi H0 reddedildi Ha kabul edildi.
# medyanlarindan en az biri farklidir


heart %>% group_by(cp) %>% 
  summarise(medyan = median(oldpeak, na.rm = T))
# 0 ile 3 arasinda medyan farki yok
# 0 ile 1, 1 ile 2 arasinda farklar var
# tabi bun??ari dunn ile incelicez ama normallik yoksa unutma
# ortalama degil medyan baz alinir lokasnyonlara bakilir


###### kruskal sonrasi dunns testi ile ikili karsilastirma ######

install.packages('dunn.test')
library(dunn.test)
dunn.test(x = heart$oldpeak, g=heart$cp)
# sonuclar tablo olarak geliyor. p degeri 0 geldi yani biri
# en az digerinde  farkli demek. etki olarak etki veriyor demek.
# daha detayli icin icine girip incele
# en altta not olarak 0.05den degil de 0.025den kucuklerde H0??
# reddet dedik ama biz yine de 0.05 alabiliriz




dunn.test(x = heart$oldpeak, g=heart$cp,
         method='bonferroni')
# asagi yukari ayni p degerlerini aldik ama bazi degerlerimizde
# artma mevcut. p degerlerimiz cok krisitik duzeydelerse method kullanmak
# cok onemli. hangi methodu kullanman gerektigini bilmen gerek arastir
# denemek onemli. p degerleri kritik seviyedeyse normal seviyeye cikana kadar
# tek tek yazarsin
# bzen onemli zamanlarda hepsini tek tek yazip yorumlamakj da gerekebikie
?dunn.test
# burda yaziyo methodlar
# gelen sonuclar medyanlar arasindaki farktir he unutma

dunn.test(x = heart$oldpeak, g=heart$cp,
          method='sidak')
# benferroni ile hemen hemen ayni sekilde bulmus baya yakin degerler
# ufak tefek var farklilik sadece










##### MANOVA TESTI####

# veri hazirlihi, aykiri deger kontrolu yapcaz oncelikle.

install.packages("mvnormtest")
library(mvnormtest)


# 2 bagimli degisken sectik. chol ve thalach(max kalp hizi gosterir) cp ile karsilastircaz

# normallik testi yapicaz

heart %>% group_by(cp) %>% 
  summarise(normalDeger = mshapiro_test(cbind(chol,thalach))$p.value)
# cp degeri 2 olan degerler icin normal dagilmayan dedi
# diger cp degerleri icin normal dagilan dedi
# chol ve thalach degerlerine gore



identify_outliers(as.data.frame(heart$chol))
 
df <- heart %>% filter(chol<394)
# tek tek yapabilriiz

identify_outliers(as.data.frame(heart$thalach))

df <- heart %>% filter(thalach!=71 & chol<394)
# ama burda ikisinin aykiri degerlerini & and kosuluyla direkt 
# attim. temiz veriyi elde ettik


df %>% group_by(cp) %>%
  summarise(normdeger=mshapiro_test(cbind(chol,thalach))$p.value)
# aykiri degerleri attik ve p degerlerinin bazilari yukseldi ancak yine de 
# cp degeri 2 olan normal olmadi


## varyans homojenligini test edelim

install.packages('heplots')
library(heplots)

bartlettTests(y = df[c("chol","thalach")], group = df$cp)
# p degeri chol icin 0.30
# p degeri thalach icin 0.37 
# yani homojen varyans dagilimdir ikisi de


leveneTests(y = df[c("chol","thalach")], group = df$cp)
# p degeri chol icin 0.24
# p degeri thalach icin 0.09
# iki test icin de varyans homojen diyebiliriz


library(car)

leveneTest(df$chol, df$cp)
leveneTest(df$thalach, df$cp)
# ayni sonuclari verdi. yani ustteki hepsini tekte yapiyo tek tek ugrasmayak diye


## box's kovaryans matrislerin homojenligi kontrolu

library(rstatix)
?box_m

# kovaryans iki tane degiskenin birlikte nasil degistigini olcer
# varyans tek bir degiskenin degisim kaytsayisini verir.
# cov() kullanilir

cov(df$chol, df$thalach)
# kovaryans degerim -35 geldi. aralarinda ters ortanti var ve degisim
# katsayisi 35 demektir

cov(df$chol[df$cp==0], df$thalach[df$cp==0])
cov(df$chol[df$cp==1], df$thalach[df$cp==1])
cov(df$chol[df$cp==2], df$thalach[df$cp==2])
cov(df$chol[df$cp==3], df$thalach[df$cp==3])
# olesine deneme yaptim




box_m(df[c('chol','thalach')], df$cp)
# H0: kovaryans matrisleri homojendir
# Ha: kovaryans matrisleri homojen degildir
# p degerim 0.50 yani kovaryans matris homojenligi vardir

# varsay??mlar?? tamamlad??k artik manova testi yapabilirizzzz

 
mnv <- manova(cbind(chol,thalach)~cp, data=df)
summary(mnv)
# H0: gruplar 2 bagimli degiskene gore degiskilik gostermez, yani esittirler
# Ha: gruplar 2 bagimli degiskene gore degisiklik gosterir, yani esit degillerdir
# cp degerlerine gore manova degerlerimi aldim.
# p degerim cokkkk dusuk cikti yani gruplar birbirinden farkli demek
# H0 reddedildi Ha kabul edildi.


library(car)

model <- lm(cbind(chol,thalach)~cp, data=df)
Manova(model, test.statistic='Pillai')
# ayni sonuclari verdi            


##### post hoc testleri manova sonrasi #####
grup_df <- df %>% gather(key='variable',value = 'values',thalach,chol) %>%  
  group_by(variable)
# yaptigimiz islem su. hani anovada wihtid vardi ya wid diye
# orada hangi musterinin degeri hangisi diyoduk falan. burda da grubu x2 yapt??k
# chol ve thalach yazisini satirlara cektik.
# variable = thalach yazdik values de onun ilk satirdan son satira dogru hangi veri
# hangi satirdaysa ekledik
# variable chol icin de aynisini yaptik. hepsi tek sutunda olmak zorunda oldugunda dolayi
# verimiz x2 oldu ama asagida values~cp yaparak direkt verilerimizi cekebildik
  
  
## welch anova test

grup_df %>% convert_as_factor(cp) %>% 
        welch_anova_test(values~cp)
# chol icin gruplar birbirine benzer yani p degeri 0.4. veriye etki etmiyormus
# thalach icin gruplar birbirine benzer degil p degeri sifira yakin. veriye etki ediyormus



## tukeyhsd testi

grup_df %>% convert_as_factor(cp) %>% 
             tukey_hsd(values~cp)
# gruplar arasindaki ortalama farklarini gosteriyor.
# hepsini tek tek yaziyor.
# chol p degerleri hepsi 0.05den buyuk cikmis yani chol degerleri benzer cikmis cplerle karsilastirilinca
# thalach degerlerinde bazilari 0.05den dusuk bazilari yuksek
# cpsi 0 olan thalach degerleri 0.05den dusuk cikti yani cp=0 degeri thalach icin
# verileri degistiren bir deger diyebiliriz
# 1-2-3 icin p degeri 0.05den buyuk cikti 

## games howell testi

grup_df %>% convert_as_factor(cp) %>% 
  games_howell_test(values~cp)
# homojen varyanslilik yoksa kullanilan
# chol icin daha net p degerleri verdi. homojenlik aramadigi icin. p degeri yukseldi yani
# thalach degerlerinde de artan degerler var ama genel sonuclar ayni









