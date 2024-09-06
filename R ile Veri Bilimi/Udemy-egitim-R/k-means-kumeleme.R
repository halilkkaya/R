install.packages("cluster")
install.packages("factoextra")
library(cluster)
library(factoextra)
library(caret)

#### veri onisleme ####

wholesale # veri setimiz bolum kodalri klasorunde var
View(wholesale)
# k means kisminda factorlere numeric olmalidir

# scale islmini gerceklestirelim
scaleModel <- preProcess(wholesale, method = c("center","scale"))
df <- predict(scaleModel, wholesale)
# scaling islemini gerceklestirdik
View(df)

library(mice)
md.pattern(df)
# kayip gozlemim de bulunmuyor.
# on isleme kismi bitti


#### model olusturma ####

clusterModel <- kmeans(df,centers = 4, iter.max = 15, nstart = 25)
# centers kume sayisi oluyor
# nstar baslangicta gelecek nokta sayim kumeleri belirleyen noktam

clusterModel

fittedCluster <- fitted(clusterModel)
View(fittedCluster)

clusterModel$cluster
# burda hangi kumeye ait olduklarini goruyoruz


#### kumeleri inceleme ####

library(tidyverse)

reversedData <- df %>% select(one_of(scaleModel$mean %>% names)) %>%
  map2_df(scaleModel$std, function(sd, var){var*sd}) %>%
    map2_df(scaleModel$mean, function(mu, var){var+mu})

# kontrol ettim verilerimiz ayni. tidyverse ile yaptik diger fonskiyon de ise yarardi

reversedData$cluster <- clusterModel$cluster
# kumelerimi veri setine ekledim

reversedData %>% group_by(cluster) %>% summarise_all(mean)
# butun clusterlarim icin mean degerlerim geldi

boxplot(Fresh ~ cluster , data = reversedData)
boxplot(Milk ~ cluster , data = reversedData)


fviz_cluster(clusterModel, df )
# inceleyip kume sayimiz hakkinda yorum yaptik





#### optimum kume sayisini bulma ####
# elbow yontemi kullanacagiz


clusterModel$withinss
# butun kumelerim icin degerler var
clusterModel$tot.withinss
# hepsinin toplami


wss <- sapply(2:10, 
       FUN = function(x){kmeans(df,centers = x, nstart = 10,
                                iter.max = 15)$tot.withinss})
wss
# 9 tane sonuc verdi.


plot(2:10, wss, type = "b")
# cikan sonuclari degerlendirelim
# akisa gore hangi noktadan sonra wss degerlerimdeki azalma oranim azaliyor
# 6. nokta iyi gibi duruyor bi daha calistirip bi daha bakalim
# hepsinde 6. nokta iyi gibi duruyor
# bu uzun yontem ama factoextra ile baska bisi yapioz


fviz_nbclust(df,kmeans,method = "wss")
# bu plotta da 6 iyi gibi duruyor ama 2 de olabilir 
# ikisi icin de ayri ayri model olusturup anlamlilik bakmamiz gerekebilir
# ama 6 bi ayri iyi gibi duruyor



# silhouette yontemi ile de bakalim
silhouette(clusterModel$cluster,dist = dist(df))
# silhouette skorumun en yuksek oldugu model en iyi modelimdir



silScore <- function(x){
  model <- kmeans(df, centers = x, nstart = 10,iter.max = 15)
  silsc <- silhouette(model$cluster , dist(df))[,3]
  score <- mean(silsc)
  return(score)
}


scores <- sapply(2:10, FUN = silScore)
scores


plot(2:10, scores, type = "b")
# max en iyisi ve burda max 2 cikmis
# elbow yonteminde ya 2 ya da 6 dedik burda da 2 dedi
# yani biz 6 demistik ama buna da bakinca 2 dedik 2yi kullanalim


fviz_nbclust(df, kmeans, method = "silhouette")
# burda da 3 dedi 2 de cok yuksek ama cok yakin 3 de 2 de kullanilabilir
# silhoutteye gore 3 daha iyi duruyor ama
# sebebi ise iter sayimiz nstart sayimiz fviz icinde afrkli olabilir ondandir

?fviz_nbclust
# burda default degerlerimiz bizim islemlerimizden farkli oldugunda dolayi
# farkli sonuclar verdi


##### optimum k ile model olusturma #####


# 2 ve 3'u deneyip icinden sececgiz


clusterModelK2 <- kmeans(df, centers = 2, nstart = 50,iter.max = 20)

clusterModelK3 <- kmeans(df, centers = 3, nstart = 50,iter.max = 20)


fviz_cluster(clusterModelK2,df)
# daha mantikli bir nolme gibi duruyor en ilk yaptigimdan

fviz_cluster(clusterModelK3,df)
# bu da guzel duruyor ha


reversedData$clusterK2 <- clusterModelK2$cluster
reversedData$clusterK3 <- clusterModelK3$cluster
a
b
c
reversedData %>% group_by(clusterK2) %>% summarise_all(mean)
# video 332 anlatiyor
# kumelerimin arasindaki mesafe fazla anlamli bi kumeleme duruyor

reversedData %>% group_by(clusterK3) %>% summarise_all(mean)
# burda da degerlerim mantikli sekilde ayrilmis bu da iyi model duruyor











