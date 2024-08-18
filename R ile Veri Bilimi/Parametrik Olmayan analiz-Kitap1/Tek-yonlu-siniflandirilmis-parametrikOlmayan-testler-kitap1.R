##### kruskal wallis testi #####
library(DescTools)
# adnersondarling testi icin

BudamaYontem <- data.frame(KavunAgr = c(790,810,800,815,805,825,820,815,810,820,815,805,770,785,765,780,800,770,760,770,750,770,760,765),
                           grup= c(rep("A",6),rep("B",6),rep("C",6),rep("D",6)))
View(BudamaYontem)
AndersonDarlingTest(BudamaYontem$KavunAgr[BudamaYontem$grup=="A"])
AndersonDarlingTest(BudamaYontem$KavunAgr[BudamaYontem$grup=="B"])
AndersonDarlingTest(BudamaYontem$KavunAgr[BudamaYontem$grup=="C"])
AndersonDarlingTest(BudamaYontem$KavunAgr[BudamaYontem$grup=="D"])
shapiro.test(BudamaYontem$KavunAgr)
# normallik gostermiyor

library(ggpubr)

ggboxplot(data = BudamaYontem, x="grup", y="KavunAgr",
          color = "grup",
          palette = c("red","blue","darkblue","black"),
          order = c("A","B","C","D"),
          ylab = "Agirlik",xlab = "Budama Yontemi")

# gorsellestirerek bi yorumda bulundugumuzda verilerin ortanca degerlerinin birbirlerinde ayri oldugunu gormekteyiz


ggline(BudamaYontem, x="grup",y="KavunAgr",
       add = c("mean_se","jitter"),
       order = c("A","B","C","D"),
       ylab = "Agirlik",xlab = "Budama Yontemleri")

# burda da orneklem ortalamalarinin ayriligini goruyoruz


kruskal.test(KavunAgr~grup, data = BudamaYontem)
# H0: ma=mb=mc=md. orneklem gruplarinin ortanca degerleri esit degildir
# Ha: budama yontemi gruplarinin en az bir tanesi farklidir
# P degerim 0.0003 H0 red. Ha kabul
# yorumlamak gerekirse budama yontemlerinden en az bir tanesi digerlerine gore daha cok ise yaramis da olabilir
# daha az ise yaramis da olabilir. 


install.packages("SuppDists")
library(SuppDists)



h <- kruskal.test(KavunAgr~grup, data = BudamaYontem)$statistic
n <- tabulate(as.factor(BudamaYontem$grup))
pKruskalWallis(q = h,c = length(n),N = sum(n),U = sum(1/n),lower.tail = F)
# degerimiz 0.05den kucuk noldugundan dolayi bu sekilde de H0 hipotezimi reddedigimi bulabilirim 
# buradaki H istatistigim daha guzel bilgiler verebilirmis zeynel hocamin dedigine gore


pairwise.wilcox.test(BudamaYontem$KavunAgr,BudamaYontem$grup,p.adjust.method = "none",exact = F)
# burda hangi gruplarin birbiriyle ayni hangilerinin farkli oldugunu goruyoruz
# su sekilde aciklayalim
# A-B arasi 0.29 P degerine sahip yani ayni ortanca degere sahipler diyebiliriz.
# A-C ve A-D 0.05den kucuk p degerlerine sahip olduklari icin esit degil diyoruz. ortancalarinda farklilik var demek
# B-C ve B-D 0.05den kucuk p degerlerine sahip olduklari icin esit degil diyoruz. 
# C-D 0.05den kucuk oldugundan esit degil diyoruz

# burda ortancalar derken aslinda genel grup benzerligini sorguladigimizda verilerin normal dagilmadigini 
# bildigimizden ortanca diyoruz. ortanca degerlerin yakinligindan veya esitlginden verilerimizin
# esit olup olmadigini buluyoruz
# burda ortanca diye yazsam da cikarim olarak sunu dusunmemiz gerekir
# "budama yontemlerinin meyve agirliklarina etkisi aynidir veya degildir" seklinde dusunmek gerekir.
# bu sekilde verileri 
# yukaridaki metotta bir duzeltme yapmadan verdik sonuclari. 


pairwise.wilcox.test(BudamaYontem$KavunAgr, BudamaYontem$grup, p.adjust.method = "bonferroni",exact=F)
pairwise.wilcox.test(BudamaYontem$KavunAgr, BudamaYontem$grup, p.adjust.method = "holm",exact=F)
pairwise.wilcox.test(BudamaYontem$KavunAgr, BudamaYontem$grup, p.adjust.method = "BH",exact=F)
# ucu de farkli parametrelere sahip farkli sonuclar verir. bunlarin anlamlari vardir.
# bonferroni daha kati bi testtir ve p degerlerini daha yukarida verir. bu da anlam ararken 
# daha gucluk cektirir bize. cok fazla veri setimiz arasindan gercekten farklilik olanlari istiyorsak bu kullanilabilir
# holm ve bh biraz daha az katidir ancak yine de nonedan daha yuksek p degerleri verirler.
# tek veri seti uzxerinde calisiyorsak none daha iyi sonuclar verebilir bizim icin. yani degisken sayisinin
# az veya cok kalmasiyla alakali bi durum


library(DescTools)
DunnTest(BudamaYontem$KavunAgr,BudamaYontem$grup)
# bu da gruplara ait verileri ikiser ikiser vermekte
# daha fazla test kitabin 221-223 arasinda vermekte arada bakarsin


library(FSA)
dunnTest(BudamaYontem$KavunAgr,as.factor(BudamaYontem$grup))
# bu da bir karsilastiema ornegidir.


install.packages("pgirmess")
library(pgirmess)

kruskalmc(resp = BudamaYontem$KavunAgr,categ = BudamaYontem$grup,probs = 0.05,cont = NULL)
# false karsilastirma sonucnuun onemsiz
# true karsilastirma sonucunun onemli bi fark oldugunu vurgular
# yine gurplari ikiser ikiser inceler ve verir
# sirasiyla gozlenen fark, kritik fark degerlerini de verir.




misir <- data.frame(verim= c(83,91,94,89,89,96,91,92,90,91,90,81,83,84,83,88,91,89,84,101,100,91,93,96,95,94,78,82,81,77,79,81,80,81),
                        yont = c(rep("Y1",9),rep("Y2",10),rep("Y3",7),rep("Y4",8)))
misir
nrow(misir)

install.packages("agricolae")
library(agricolae)

sonuc <- with(misir, Median.test(y=misir$verim, trt=misir$yont,group = T,console = F))
sonuc
# genel aciklayici sonucalr verir

sonuc1 <- with(misir, Median.test(verim, yont, group = F, console = F))
sonuc1$comparison
# gruplar arasi karsilastirmayi verir


install.packages("coin")
library(coin)
misir$yont <- as.factor(misir$yont)
median_test(verim~yont, misir)




