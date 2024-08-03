##### histogram grafigi####

View(iris)

hist(iris$Sepal.Length) #saga carpik yani degerler solda cogunluklu tabloymus


hist(iris$Sepal.Length,
     main = 'histogram grafigi') 
#ustunde yazan adi degistik

hist(iris$Sepal.Length,
     main = 'histogram grafigi',
     xlab= 'degisken degerler',
     ylab = 'frekans degerleri'

)
#x ve y kismindaki isimler degisti

hist(iris$Sepal.Length,
     main = 'histogram grafigi',
     xlab= 'degisken degerler',
     ylab = 'frekans degerleri',
     breaks = 3
     
)
# 3 tane kirilim yapti yani 4 sutun verdi
hist(iris$Sepal.Length,
     main = 'histogram grafigi',
     xlab= 'degisken degerler',
     ylab = 'frekans degerleri',
     breaks = 30    
)
#deger arttikca ayrinti artar. sayilari tek tek vermeye basliyo arttirdikca

hist(iris$Sepal.Length,
     main = 'histogram grafigi',
     xlab= 'degisken degerler',
     ylab = 'frekans degerleri',
     breaks = 30,
     xlim = c(0,10)
)
# artik x kismini 0 ile 10 arasinda verdik ve 4-8 arasinda oldugunu daha iyi gozlemledik

hist(iris$Sepal.Length,
     main = 'histogram grafigi',
     xlab= 'degisken degerler',
     ylab = 'frekans degerleri',
     breaks = 100,
     xlim = c(0,10),
     ylim = c(0,30)
)
# yine daha da ufaldi grafik. cunku veriler 10 civarindaydi y ksiminda

#elimizdeki verileri daha iyi sekilde gostermek icin bu sekilde
#goruntuleme ozellikleri onemli


##### hist grafigi renklendirme #####

hist(iris$Sepal.Length,
     main = 'histogram grafigi',
     xlab= 'degisken degerler',
     ylab = 'frekans degerleri',
     col = 'blue'
)
#arka plan mavi oldu
hist(iris$Sepal.Length,
     main = 'histogram grafigi',
     xlab= 'degisken degerler',
     ylab = 'frekans degerleri',
     col = 'orange'
)
#turuncu oldu sutunlar

hist(iris$Sepal.Length,
     main = 'histogram grafigi',
     xlab= 'degisken degerler',
     ylab = 'frekans degerleri',
     col = '#2ecc71'
)
#kod ile de yapilio

hist(iris$Sepal.Length,
     main = 'histogram grafigi',
     xlab= 'degisken degerler',
     ylab = 'frekans degerleri',
     breaks = 15,
     col = c('#2ecc71','purple','orange')
)
#sirasiyla calisti. 3 sutuna sirasiyla 3 renk verdi

hist(iris$Sepal.Length,
     main = 'histogram grafigi',
     xlab= 'degisken degerler',
     ylab = 'frekans degerleri',
     breaks = 30,
     col = c('#2ecc71','black','orange')
)


############ hist grafigi uzerine yogunluk egrisi cizimi##########


hist(iris$Sepal.Length,
     main = 'histogram grafigi yoguynluk egrisi',
     xlab = 'degerler',
     ylab = 'yogunluklar probs',
     prob = T,
     col = 'orange')
#probs ile degerler mesela 10 ile 40 arasindaydi yogunluga gore nerde coksa
#oraya kadar cikan grafig yaptk


density(iris$Sepal.Length) # sepallenghtin grafige gore x ve y degerlerini
#verir. x ozet istatistik bilgileri verirken y yogunluga gore olan bilgileri verir.

lines(density(iris$Sepal.Length))
#once hist calistirip sonra lines calistir. grafik ustune bi cizgi cekecek.
#iste o cizgi yogunluk cizgisidir


lines(density(iris$Sepal.Length,adjust = 2))
#adjust ekledik ve bi cizgi daha cizdi. bu adjust verilerdeki dalgalanmayi
#istediginiz sekilde yonetmeyi amaclar. 2 yaptik daha duz oldu. 3-4-5 falan yapsak
#daha da duzlesir. default degeri 1dir

lines(density(iris$Sepal.Length,adjust = 5))
#daha da kivrimi kaldirdi



lines(density(iris$Sepal.Length, adjust = 1),
      col='gray', lwd = 3) 
#lwd kalinlik artiriyor, col renk degisiyor

lines(density(iris$Sepal.Length, adjust = 1),
      col='blue', lwd = 2, lty = 'dotted' )
#cizgili bi gorhunm verdi

lines(density(iris$Sepal.Length, adjust = 3),
      col='gray', lwd = 3 )
#gri duz cizgi verdi. cizgiler bole yapiliyo
#linesi her calistirdiginda yeni cizgi ekler
#kaydetmek icin export bas save image de

######### sacilim diyagramlari #######

#scatter plat olarak gecer


View(airquality)
#yeni veri seti bu guzel bu da

plot(airquality$Ozone)
#sacilim diyagrami yani nokta nokta gosteriyor


plot(airquality$Ozone, bty = 'L')
#default olarak kare gradfikti L seklinde yaptik

plot(airquality$Ozone, pch =2)
#2 dedik ucgen oldu, 3 dedik arti oldu

plot(airquality$Ozone, pch ='*')
#sekli biz belirledik. hangi isareti harfi sayiyi koyarsak
#npktalar ona donusur


plot(airquality$Ozone, pch =4)
#4 carpiymis

plot(airquality$Ozone, pch =19)
#ici dolu siyah nokta verdi. en cok kullandigi buymus
#isimize yarar bilek

plot(airquality$Ozone, pch =19, type = 'h')
#noktalar bar seklini alir. her sayi icin cizgi cikar

plot(airquality$Ozone, pch =19, type = 'l')
#her birbirine yakin noktayi birlestirip cizerek verir. uzaksa
#sadece cizgi kalir

plot(airquality$Ozone, pch =19, type = 'b')
#bi ustteki kismin noktalari da eklipo birlesme yerleri icin

plot(airquality$Ozone, pch =19, type = 'c')
#noktalari sildi sadece birlesim yeri birakti. 


plot(airquality$Ozone, pch =19, type = 'o')
#cok fazla veri noktasi varsa ust uste binen noktalar varsa
#cok fazla olan noktalari ayirip verir. onemli


plot(airquality$Ozone, pch =19, type = 's')
#adimsal gorsellestirme. azalis ya da artislarda merdiven gibi duruyo
#ani inis cikislari anlamak icin onemli



#ozone sutunu ile temp sutunu arasini gorsellestircez
#yani birbirleriyle nasil hareket ederler biri artar digeri ne olur
#bunlari ogrenmek icin

plot(airquality$Ozone,airquality$Temp, pch=19,type='p')
#x ve y eksenine girdigimiz degiskenleri eklio
#mesela ozoneda 20yken tempde 60 olmus gibi gosterior
#onemli bir yapidir

plot(airquality$Ozone,airquality$Temp, pch=19,type='h')
# h ile de ii duruomus la


plot(airquality$Ozone,airquality$Temp, pch=19,type='p',
     main = 'sacilim diyagrami', xlab = 'ozone', ylab = 'temp degerleri',
     bty = 'L')
#basliklari degistik ve l grafik olarak verdi ne degerlerini otomatik almio
#x ve y veriosak ikisinin de ayni uzunlukta olmasi gerekiyor yoksa hata verir


##### sacilim diyagrami noktalari renklendirme#####
plot(airquality$Ozone, airquality$Temp, pch = 19, bty ='L',type = 'p',
     main = 'sacilim grafigi renkler', xlab = 'ozone',ylab = 'temp',
     col = 'orange')


plot(airquality$Ozone, airquality$Temp, pch = 19, bty ='L',type = 'p',
     main = 'sacilim grafigi renkler', xlab = 'ozone',ylab = 'temp',
     col = 'blue')
#renkleri degistik



class(airquality$Month) #integer
#aylara gore renklendirme islemi yapacagiz o yuzden aylari aldik
#renklendirme icin factor lazim. her seviyeye renk atamak icin

levels(as.factor(airquality$Month))
#faktore cevirip levelleri verdi

#gruplara gore renklendirme
plot(airquality$Ozone, airquality$Temp, pch = 19, bty ='L',type = 'p',
     main = 'sacilim grafigi renkler', xlab = 'ozone',ylab = 'temp',
     col = c('blue','orange','pink','gray','black'))
#suan index sirasina gore verdi

plot(airquality$Ozone, airquality$Temp, pch = 19, bty ='L',type = 'p',
     main = 'sacilim grafigi renkler', xlab = 'ozone',ylab = 'temp',
     col = c('blue','orange','pink','gray','black')[as.factor(airquality$Month)]
     
     )
#simdi aylara gore verdi. sirasiyla;
#5. say blue, 6. aylar turuncu diye gidiyor. once integerdaan faktore cevirdik ve levellar olustu
#sonra level sayisi kadar renk girdik ve her levele bi renk atadi

renkler <-  c('blue','orange','pink','gray','black')[as.factor(airquality$Month)]


plot(airquality$Ozone, airquality$Temp, pch = 19, bty ='L',type = 'p',
     main = 'sacilim grafigi renkler', xlab = 'ozone',ylab = 'temp',
     col = renkler
     
)
#seklinde de once atamayla levelleri belirleyip sonra renkleri atadik ve bunu degiskene
#atayip color kismina yazdik

legend(x = 'topright',legend = levels(as.factor(airquality$Month)),
       col = c('blue','orange','pink','gray','black'),
       pch = 19
       
       )
#hangi renklerin neyi temsil ettigini gostermis oluyoruz. ayni siralarda yazmak cok onemli
#rekleri falan ayni sirayla yazmak zorundasin yoksa yanlis bilgi verir. once levelleri belirledik
#yine factore cevirdik cunku level kavvrami factorde var. sonra da sirasiyla ona renk verdik
#cok onemli sik sik kullanacagin bi yapi

par(mar=c(6,6,6,7), xpd= TRUE)
#bottom,left,top,right seklinde bosluk ekliyor grafige
#xpd disari tasma olsun demek

plot(airquality$Ozone, airquality$Temp, pch = 19, bty ='L',type = 'p',
     main = 'sacilim grafigi renkler', xlab = 'ozone',ylab = 'temp',
     col = renkler
     
)
#seklinde de once atamayla levelleri belirleyip sonra renkleri atadik ve bunu degiskene
#atayip color kismina yazdik

legend(x = 'topright',legend = levels(as.factor(airquality$Month)),
       col = c('blue','orange','pink','gray','black'),
       pch = 19, inset = c(-0.2,0)
       
)
#inset ile legendi disari dogru kaydirdik -0.2 vererek
# ikinci parametre de asagi dogru kaydirdi.
#sag taraftaki deger arttikca asahi sol taraftaki degeri arttirdikca sola kayar. 
#eksi koyduk saga kaydi mesela
#0 ile 1 degerler arasinda deger ver









##### sacilim grafiklerinde noktalar??n belirli bir degiskene gore boyutlanmasi######
 
par(mar =c(5,5,5,7),xpd=T)

plot(x = airquality$Ozone, y= airquality$Temp,
     main = 'Ozon ve Sicaklik iliskisi',
     xlab = 'Ozon degerleri',
     ylab = 'sicaklik degerleri',
     pch = 19,
     col = c('blue','orange','pink','gray','black')[as.factor(airquality$Month)],
     bty = 'L',
     cex = 0.4 #buyucukce kalin kuculdukce kucuk oluyo noktalar
     )


legend(x='topright', legend = levels(as.factor(airquality$Month)),
       col = c('blue','orange','pink','gray','black'),
       pch = 19,
       inset = c(0.2,0)
       )





par(mar =c(5,5,5,7),xpd=T)

plot(x = airquality$Ozone, y= airquality$Temp,
     main = 'Ozon ve Sicaklik iliskisi',
     xlab = 'Ozon degerleri',
     ylab = 'sicaklik degerleri',
     pch = 19,
     col = c('blue','orange','pink','gray','black')[as.factor(airquality$Month)],
     bty = 'L',
     cex = airquality$Wind/10 #kalinliklar ruzgara gore olsun dedik.
     #10a bolduk cunku degerleri cok buyuktu
)


legend(x='topright', legend = levels(as.factor(airquality$Month)),
       title = 'aylar',
       col = c('blue','orange','pink','gray','black'),
       pch = 19,
       inset = c(0.2,0)
       
       
       )
legend(x = 'topright', legend = c('dusuk','orta','yuksek'),
       title = 'ruzgar seviyesi',
       cex = 0.8, # yazilari kucultur
       pt.cex = c(min(airquality$Wind/10),mean(airquality$Wind/10),
                  max(airquality$Wind/10)),
       pch = 19,
       inset = c(0.2,0.3)
       )


##### sacilim diyagraminda linner iliski dogrusu ve lowess egrisi########

airquality <-  na.omit(airquality)  #bi degeri na olan satirin tamamini cikarir
par(mar= c(5,5,5,7.2),xpd=FALSE)

plot(x = airquality$Ozone, y= airquality$Temp,
     main = 'Ozon ve Sicaklik Degerleri',
     xlab = 'Ozon Degerleri',
     ylab = 'Sicaklik Degerleri',
     pch = 19,
     bty = 'L',
     col = c('blue','green','yellow','red','black')[as.factor(airquality$Month)],
     cex = airquality$Wind/10
     
     )

abline(lm(airquality$Temp ~ airquality$Ozone ) ,
       lwd = 2, lty ='dotted', col='black')
#iliski cizgisidir ozone arttikca sicaklik artar ogrendik
#lm icerisinde ilk deger y ikinci deger xdir
#bagimsiz degisken x bagimli degisken ydir
#xpd degeri T oldugunda sacma bi cizgi geliyor
#o yuzden en ustte false yapip legendlere geldigimizde T yaptik.
#onemli detaydir.
lines(lowess(airquality$Ozone,airquality$Temp), lwd=2,col='blue')
#egritilmis cizgidir. lines fonk ile cizilir.
#x ve y yeri normaldir
#lowess ile iliskiyi belirleyip oyle cizdil. lowess cok onemli
par(xpd=T)

legend(x = 'topright',legend = levels(as.factor(airquality$Month)),
       col = c('blue','green','yellow','red','black'),
       pch = 19,
       inset = c(0,0),
       title = 'aylar'
       
       )

legend(x = 'topright', legend = c('dusuk','orta','yuksek'),
       pt.cex = c(min(airquality$Wind/10),mean(airquality$Wind)/10,max(airquality$Wind)/10),
       cex = 0.8,
       title = 'Ruzgar',
       pch = 19,
       inset = c(0,0.5)
       
       )
?lowess
lowess(airquality$Ozone,airquality$Temp)

#cikan sonuclarda x degerleri grafikteki x
#y degerleri grafikteki y degerleridir



##### bar grafikleri ve sayisal deg??sken gruplama#####

# histogram elindeki verileri grafik yaparken bar grafigi
# atiyorum 5 kategori var o 5 kategopriyi grafik haline getirir 

View(mtcars)

table(mtcars$cyl) 
#frekans??n?? aldik. yani hangisinden kac tane var onu bulduk

height = table(mtcars$cyl) 
#sayilari aldik. barplot() fonk ilk degeri yukseklik yani kac adet olacagi

barplot(height = height, names.arg = c('4 silindir','6 silindir','8 silindir'),
        col = 'orange',
        border = 'blue'
        )
#names sirasiyla x eksenindeki degisken adlarini degistirir
# kenarlar vardi border diyerek o kenarlari da degistik rengini. sutunlarin kenarlari yani

barplot(height = height, names.arg = c('4 silindir','6 silindir','8 silindir'),
        col = 'orange',
        border = 'blue',
        horiz = TRUE,
        cex.axis = 0.8,
        cex.names = 0.8
)
#horiz ile yatay yaptik
#cex.axis sayilari cex.names ise yazilarin boyutunu ayarlar


barplot(height = height, names.arg = c('4 silindir','6 silindir','8 silindir'),
        col = 'orange',
        border = 'blue',
        horiz = FALSE,
        cex.axis = 0.8,
        cex.names = 0.8,
        axis.lty = 1
        
)
#axis.lty asagiya cizgi gelir 1,2,3,4... sayilari seklini degistirir


grup_f <-  function(x){
    
  cey <-  quantile(x) #ceyreklikleri atadik
  result <- character(length(x)) #bos karakterli vektor olusturduk
  
  grup_1 <- which(x <= cey[2]) #grup 1e 2. ceyreklikteki verilerden kucuk ve esit olanlari atadik
  grup_2 <- which(x <= cey[3] & x>cey[2]) #grup 2ye 3. cceyrekliktekilerden kucuk 
  #ve esit ayni zamanda 2. ceyreklikdekilerden buyukleri atadik
  grup_3 <- which(x <= cey[4] & x>cey[3])
  grup_4 <- which(x <= cey[5] & x>cey[4])
  
  result[grup_1] <- 'Grup 1' #result icinde grup 1 kismini ekledik ve true degerlere grup 1 yazdirdik
  result[grup_2] <- 'Grup 2'
  result[grup_3] <- 'Grup 3'
  result[grup_4] <- 'Grup 4'
  
  
  return(result)
}
#burdaki fonksiyon icerisinde girdiigimiz tablodaki degerlerin ceyrekliklerine
#gore gruplara ayiriyor. 

sort(mtcars$mpg)
quantile(mtcars$mpg)
result <- grup_f(mtcars$mpg)

barplot(height = table(result), 
        col = 'orange',
        border = 'blue',
        horiz = FALSE,
        cex.axis = 0.8,
        cex.names = 0.8,
        axis.lty = 1
)


result <- grup_f(mtcars$hp)

barplot(height = table(result), 
        col = 'orange',
        border = 'blue',
        horiz = FALSE,
        cex.axis = 0.8,
        cex.names = 0.8,
        axis.lty = 1
)

# bu iki grafikte de ayirdigimiz ceyrekliklere gore siralama yaptik.
#olusturdugumuz fonksiyon onemli. oyle seyler olusturarak fonksiyon
#kullanmaya alis



##### yigin bar grafikleri####

#ust uste binmis barlar da diyebiliriz

view(mtcars)

table(mtcars$gear,mtcars$cyl)
# ikili frekans degerlerini aldik. matris gibi verdi



t <- table(mtcars$gear,mtcars$cyl)

barplot(t)
# x ekseninde cyl kismi var
# y eksininde de vites var.
# cyl kismina gore vites sayisini veriyor burda.
# renklerle ayiriyor. 4 silindir kismini ele aldigimizda
# en koyusu ilk degeri yani 3 vites daha acigi 4 vites en acigi 5 vites olan
#araba sayilarini veriyor

par(mar=c(5.1,5.1,5.1,8), xpd = T)
barplot(t,
        names.arg = c('4 silindirli','6 silindirli','8 silindirli'),
        axis.lty = 1,
        main = 'Bar Grafigi',
        xlab = 'Silindir degerleri',
        ylab = 'frekans degerleri',
        col = c('orange','pink','gray'),
        legend = c('3 vitesli','4 vitesli','5 vitesli'),
        args.legend = list(bty ='o',
                           xjust= 0.5,
                           yjust=-0.5,
                           horiz = F,
                           cex = 0.8)
        
        )


legend(x= 'topright', levels(as.factor(mtcars$gear)),
       title = 'vites sayisi',
       pch = 19,
       col = c('orange','pink','gray'),
       inset = c(-0,-0.2),
       cex = 1,
       pt.cex = 2
       )
 
#iki sekilde de legend eklenebiliyor.

#burda da xe vites sayisi y'ye silindir sayisi verdik
t <- table(mtcars$cyl,mtcars$gear)

par(mar=c(5.1,5.1,5.1,8), xpd = T)
barplot(t,
        names.arg = c('3 vitesli','4 vitesli','5 vitesli'),
        axis.lty = 1,
        main = 'Bar Grafigi',
        xlab = 'Silindir degerleri',
        ylab = 'frekans degerleri',
        col = c('orange','pink','gray'),
        legend = c('4 silindirli','6 silindirli','8 silindirli'),
        args.legend = list(bty ='o',
                           xjust= 0.5,
                           yjust=-0.5,
                           horiz = F,
                           cex = 0.8)
        
)






###### pasta grafikleri######
#bar ile ayni ozllediklere sahip gibi benziyor
#tek degisken uzerinde yapar
#yuzdekil hesaplar


View(mtcars)
table(mtcars$gear)
#tekrarlanma sayilarini goster

prop.table(table(mtcars$gear))
#      3       4       5 
#0.46875 0.37500 0.15625 
 
#ciktisini veriyor bunlar?? 100le carparsak yuzdelik olur??ar

yuz <- prop.table(table(mtcars$gear))*100
#bu sekilde yuzdeliklerini elde ettik. kac vites olduguna gore araba sayisi

names <- c('3 vitesli ','4 vitesli','5 vitesli')

sprintf('%s = %3.1f%s',names,as.numeric(yuz),'%')
# %s namesdeki isimleri soyler.
# %3.1f sayinin ondalikli oldugunu soyler. as.numeric(yuz) girdigimiz yer
# %s en sondaki hangi isareti alacagini soyler
#guzel ve onemli


y <- sprintf('%s = %s%3.1f',names,'%',as.numeric(yuz))
#yerlerini degistik ikisini de dene
#daha iyi anlarsin 



pie(yuz, main = 'viteslerin pasta grafigi', 
    clockwise = F,
    labels = y,
    col = c('orange','pink','cyan'),
    init.angle = 180,
    border = 'black',
    lty = 2,
    radius = 1
    
    
    )

#labels isimleri degistirdi. yukarida degistirip atayip koyduk
#init.angle ile dereceyle dondurduk. 180 dereceyle tamamen dondurebiliriz
#clockwise donus yonunu belirler. t saat yonu tersine F saat yonune. istersen yazma bile
#lty kenarlar??n nas?? cizildigini belirlio
#radius grafigin boyutunu belirler default 0.8dir




##### kutu grafigi boxplot#####

#genellikle ceyreklikler min max medyan ile farkl??l??k gosterir
#onlari anlamak icin kullanilir genellikle
# bir degisken grubunun icindeki degerlerin nasil konumlandigi
#nasil degistigi vs gosterir

boxplot(mtcars[,c('drat','wt')])
# drat'i incelersek. en alta inen cizgi min deger, en sute cikan max deger
# kutu icerisindeki en alt cizgi 1. ceyreklik en ustu 3. ceyreklik
# ortadaki kalin cizgiyse medyandir


boxplot(mtcars[,c('drat','wt')],
        main = 'Box Plot Grafigim',
        xlab ='degiskenler',
        ylab ='degerler',
        names = c('Drat Degiskeni','WT Degiskeni'),
        col = c('blue','orange'),
        border = 'black',
        pch = 19,
        cex.axis = 0.7,       
        range = 0.5,
        outline = T
        
        )
#range min max degerlerini kontrol eder. biz azalttik. aykiri degerleri kontrol eder
#outline aykiri degerleri kapatir eger FALSE ise

boxplot( mtcars$mpg~mtcars$gear,
        main = 'Box Plot Grafigim',
        xlab ='vites degerleri',
        ylab ='mpg degerleri',
        col = c('orange'),
        border = 'black',
        pch = 19,
        cex.axis = 0.7,  
        outline = T
)

#3 tane verdi yani 3 grup vites icin verdi



boxplot( mtcars$mpg~mtcars$gear*mtcars$cyl,
         main = 'Box Plot Grafigim',
         xlab ='vites degerleri',
         ylab ='mpg degerleri',
         names = c('3v 4s','4v 4s', '5v 4s',
                   '3v 6s','4v 6s', '5v 6s',
                   '3v 8s','4v 8s', '5v 8s'),
         col = c('orange'),
         border = 'black',
         pch = 19,
         cex.axis = 0.7,  
         outline = T
)
# 2 bagimsiz 1 bagimli degisken uzerinden yaptik
#3 vites 4 silindirli, 4 viesli 4 silindirli vsvs seklinde yaziyor












##### heatmap grafikleri####

#sicaklik grafikleri. buyk kucuk degerlerden olusmasi fln
#heatmap icin matris olmak zorunda matris olmazsa islem yapmaz

View(mtcars)

mtcars_1 <-  as.matrix(mtcars)

heatmap(mtcars_1, scale = 'column')
#scale islemince sutunlar baz??nda islemler yapiyo ve grafigi daha duzgun hale getiriyor

heatmap(mtcars_1, scale = 'column', Colv = NA, Rowv = NA)
#colv ve rowv baglantilari kaldiriyor
library(RColorBrewer)
#sutun renklerini degismek icin paket
colorRampPalette(brewer.pal(9,'Blues'))(25)
#acik maviden koyu maviye dogru 9 renk tonu verir 25 tane renk


heatmap(mtcars_1, scale = 'column', Colv = NA, Rowv = NA,
        col = colorRampPalette(brewer.pal(9,'Blues'))(25)
        
        
        )

legend(x= 'bottomright', legend = c('Min','Ort','Max'),
       fill = colorRampPalette(brewer.pal(9,'Blues'))(3))

 


##### ODev#####

x <- top_women_chess_players_aug_2020
x <- na.omit(x)

factor(x$Title)

plot(x$Standard_Rating, x$Rapid_rating,
     bty = 'L',
     xlab = 'Standart Rating Degerleri',
     ylab = 'Rapid Rating Degerleri',
     cex = 0.3,
     pch = 19,
    col = c('blue','green','red','black','pink','orange','grey','purple','yellow')[factor(x$Title)]
     )

abline(lm(x$Rapid_rating~x$Standard_Rating),
       lyt = 'dotted',
       lwd = 2,
       col ='black')



legend(x = "bottomright" , legend = levels(factor(x$Title)),
       col = c('blue','green','red','black','pink','orange','grey','purple','yellow'),
       pch = 19,
             
)       
       