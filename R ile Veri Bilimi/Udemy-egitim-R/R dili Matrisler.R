
############### MATRiSLERRRRRRRRRRRRRRRRRRRR ###############
#data frameler matrislerin kopyasi gibi diyebiliriz ama tamamen ayni degil.

x = c(1,2,3,4)

#ilk yer hangi vektorse o giriliyor, dierleri satr ve kolon saylardr.
# 4 tane verimiz bulundugu ixin 2x2 yaptik
#once sutunlardan baslayip 1 2 3 4 diye siraliyor yani alt alta
matrix(x,nrow = 2, ncol = 2)

#bu kodda satirlara gire siralatiyor yani yanyana sonra alta iniyor
matrix(x,nrow = 2,ncol = 2,byrow = TRUE)




y = c(1,2,3,4,5,6,7,8)
matrix(y,nrow = 2,ncol = 4)  
matrix (y,nrow = 4,ncol = 2)


matrix(x,nrow = 4,ncol = 4)#veriden daha fazla sutun satir sayisi varsa veri tekrar eder
matrix(x,nrow = 6,ncol = 6)#ayni sekil tekrar ediyor. alt alta yaziliyor burda
matrix(x,nrow = 6,ncol = 6,byrow = T)#bu da yanyana yazma kodudur.
matrix(x,nrow = 7,ncol = 7)#hata verir. 2 katli sayi olmasi gerek. 4 sayi var cunku




################### MATRiSLERDE ELEMAN VE ALT MATRiS SEixMi################


m1 <-  matrix(c(12,22,34,45,45,56,57,68),nrow = 2,ncol = 4, byrow = TRUE)
m1


# [1,] ile [,1] birlesimi [1,1] olur yani once satir sonra sutun numarasina
#gore arama yapmamiz gerek. 
m1[1,4] #1. satir 4. situn
m1[2,4] #2. satir 4. sutun


#matris parcalarini secmek
m1[c(1,2),c(1,2)] #ilk iki satir, ilk iki sutunu alir

m1[1:2,1:2] #ilk iki satir, ilk iki situnu alir

m1[c(1),c(1,3,4)]# ilk satir ve 1,3,4. sutu  gelio

################ matrislerde satir/sutun kaldirma, deger degistirme################


m <- matrix(c(1,2,3,4,5,6),nrow = 2,ncol = 3)
m

m <- m[-1,] #ilk satiri sildi
m


m <- m[,-1] #ilk sutunu sildi
m

m <- m[,-2]
m

m <- as.matrix(m) #matrixe cevirme islemi. silince vektore dondo tek akld icin
class(m)
m


m1 <- matrix(c(11,22,33,44,55,66,77,88), 4,2, TRUE) 
m1 #nrow ve ncol byrow yazmadan olusturma yaptik. ama dikkat et ilki satir ikinci sutundur

m1[-3,] #atama yapmadik. sadece gostermek icin 
m1[-c(2,3),] #2,3 satcrlari sildik
m1[-c(1,2),-c(1)] #1,2 satirlari 1. sutunu sildik


m1[-(1:2),-2] #1,2 satirlari 2. sutunu sildik


m1[2,2] <- NA
m1 # matrislerden veri silmek mumkun degil onun yerine NA atayabiliriz sadece
#cunku veri silmek matris yapisini bozar

m1[c(1,2),1] <- NA
m1 #1. satir 1. sutun kesisimi ile 2.satir 1. situn kesiiimi na oldu

m1[c(1,2),2] <- c(23,45) # 1 vw 2. satirin 2. sutununa deger atadik

m1[2,2] <- 123
m1 # 2. satir 2. situn degerimi atadik.

m2 <- m1[-4,]
m2 #mq matrisinin 4. satirninn cikarilmis hali m2ye atand





################ matrislerde satir/sutun ekleme###################

m <- matrix(c(1,2,3,4),2,2,T)
m

m[,3] <- c(1,2) #bu sekilde atama yapilmiyor matrislerde. bu hata!!

cbind(m,c(5,6)) #matrislere yeni kolon ekleme gorevi 

rbind(m,c(7,8)) #matrislere satir ekleme

cbind(m,c(1,2,3)) #2 satirdan olustugu icin 1 ve 2 verilir 3 verilemez hata verir 3 icin
rbind(m,c(1,2,3)) #2 sutun var diye sadece 2 sayi ekleyebilir. 3 hata verir

# bu olaylar icin atama yapmazsan eskisi gibi kalir atama yapmak zorundasin.

m <- cbind(m,c(1,2))
m
m <- rbind(m,c(4,5,6)) #dikkat yukaridaki atamadan sonra situn sayisi artti
#bu da daha fazla satir girmen lazim demek
m 


