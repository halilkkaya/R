trainTestSplit <- function(data, dvName, seed){
  tbl <- table(data[,dvName])
  # tabloyu sectik
  classes <- names(tbl)
  # adlarini aldik
  minClass <- min(tbl)
  # en kucuk sayiya sahip classi sectik
  lengthClass <- length(tbl)
  # toplam class sayisini aldik
  
  train <- data.frame()
  test <- data.frame()
  # test ve train verisetlerimizi sonrasinda icini doldurmak icin bos sekilde olusturduk
  
  for (i in 1:lengthClass) {
    selectedClass <- data[,dvName] == classes[i]
    # siniflari tek tek seciyoruz
    sampleIndex <- sample(1:nrow(data[selectedClass,]), size = minClass*0.8)
    # siniflara gore en az sayiya sahip olan sinifin uzerinden belli rastgele indexler aliyoruz
    
    train <- rbind(train, data[selectedClass, ][sampleIndex, ])
    # for ile gelen siniflara ait verilerimi trainin icerisine atiyor
    test <- rbind(test, data[selectedClass, ][-sampleIndex, ])
  }
  return(list(train,test))
}
