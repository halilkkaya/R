# biyonominal dagilis olasilik hesaplama fonksiyonu

signtest.prob <- function(n,s,alternative="two.sided"){
  if (alternative=="less") {
    p.value <- pbinom(q=s,size = n,prob = 0.5)
  }else if (alternative=="greater") {
    p.value <- 1-pbinom(q=s,size = n,prob = 0.5,lower.tail = F)
  }else if (alternative=="two.sided") {
    p1 <- pbinom(q=s,size = n,prob = 0.5)
    p2 <- 1-pbinom(q=s,size = n,prob = 0.5,lower.tail = F)
    p.value <- 2*min(p1,p2)
  }
  list(hipotez=alternative , n=n,s=s,p.value=p.value)
}
