
############### MATR??SLERRRRRRRRRRRRRRRRRRRR ###############
#data frameler matrislerin kopyas?? gibi diyebiliriz ama tamamen ayn?? de??il.

x = c(1,2,3,4)

#ilk yer hangi vekt??rse o giriliyor, di??erleri sat??r ve kolon say??lar??d??r.
# 4 tane verimiz bulundu??u i??in 2x2 yapt??k
#??nce s??tunlardan ba??lay??p 1 2 3 4 diye s??ral??yor yani alt alta
matrix(x,nrow = 2, ncol = 2)

#bu kodda sat??rlara g??re s??ralat??yor yani yanyana sonra alta iniyor
matrix(x,nrow = 2,ncol = 2,byrow = TRUE)




y = c(1,2,3,4,5,6,7,8)
matrix(y,nrow = 2,ncol = 4)  
matrix (y,nrow = 4,ncol = 2)


matrix(x,nrow = 4,ncol = 4)#veriden daha fazla sutun sat??r say??s?? varsa veri tekrar eder
matrix(x,nrow = 6,ncol = 6)#ayn?? ??ekil tekrar ediyor. alt alta yaz??l??yor burda
matrix(x,nrow = 6,ncol = 6,byrow = T)#bu da yanyana yazma kodudur.
matrix(x,nrow = 7,ncol = 7)#hata verir. 2 katl?? say?? olmas?? gerek. 4 say?? var cunku




################### MATR??SLERDE ELEMAN VE ALT MATR??S SE????M??################


m1 <-  matrix(c(12,22,34,45,45,56,57,68),nrow = 2,ncol = 4, byrow = TRUE)
m1


# [1,] ile [,1] birle??imi [1,1] olur yani ??nce sat??r sonra s??tun numaras??na
#g??re arama yapmam??z gerek. 
m1[1,4] #1. sat??r 4. s??tun
m1[2,4] #2. sat??r 4. s??tun


#matris par??alar??n?? se??mek
m1[c(1,2),c(1,2)] #ilk iki sat??r, ilk iki s??tunu al??r

m1[1:2,1:2] #ilk iki sat??r, ilk iki s??tunu al??r

m1[c(1),c(1,3,4)]# ilk sat??r ve 1,3,4. s??tu  gelio

################ matrislerde sat??r/s??tun kald??rma, de??er de??i??tirme################


m <- matrix(c(1,2,3,4,5,6),nrow = 2,ncol = 3)
m

m <- m[-1,] #ilk sat??r?? sildi
m


m <- m[,-1] #ilk s??tunu sildi
m

m <- m[,-2]
m

m <- as.matrix(m) #matrixe cevirme i??lemi. silince vekt??re d??nd?? tek akld?????? i??in
class(m)
m


m1 <- matrix(c(11,22,33,44,55,66,77,88), 4,2, TRUE) 
m1 #nrow ve ncol byrow yazmadan olu??turma yapt??k. ama dikkat et ilki sat??r ikinci s??tundur

m1[-3,] #atama yapmad??k. sadece g??stermek i??in 
m1[-c(2,3),] #2,3 sat??rlar?? sildik
m1[-c(1,2),-c(1)] #1,2 sat??rlar?? 1. s??tunu sildik


m1[-(1:2),-2] #1,2 sat??rlar?? 2. s??tunu sildik


m1[2,2] <- NA
m1 # matrislerden veri silmek m??mk??n de??il onun yerine NA atayabiliriz sadece
#????nk?? veri silmek matris yap??s??n?? bozar

m1[c(1,2),1] <- NA
m1 #1. sat??r 1. s??tun kesi??imi ile 2.sat??r 1. s??tun kesi??imi na oldu

m1[c(1,2),2] <- c(23,45) # 1 vw 2. sat??r??n 2. s??tununa de??er atad??k

m1[2,2] <- 123
m1 # 2. sat??r 2. s??tun de??erimi atad??k.

m2 <- m1[-4,]
m2 #mq matrisinin 4. sat??rn??nn ????kar??lm???? hali m2ye atand





################ matrislerde sat??r/s??tun ekleme###################

m <- matrix(c(1,2,3,4),2,2,T)
m

m[,3] <- c(1,2) #bu ??ekilde atama yap??lm??yor matrislerde. bu hata!!

cbind(m,c(5,6)) #matrislere yeni kolon ekleme g??revi 

rbind(m,c(7,8)) #matrislere sat??r ekleme

cbind(m,c(1,2,3)) #2 sat??rdan olu??tu??u i??in 1 ve 2 verilir 3 verilemez hata verir 3 i??in
rbind(m,c(1,2,3)) #2 s??tun var diye sadece 2 say?? ekleyebilir. 3 hata verir

# bu olaylar i??in atama yapmazsan eskisi gibi kal??r atama yapmak zorundas??n.

m <- cbind(m,c(1,2))
m
m <- rbind(m,c(4,5,6)) #dikkat yukar??daki atamadan sonra s??tun say??s?? artt??
#bu da daha fazla sat??r girmen laz??m demek
m 


