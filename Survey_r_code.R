##################### CZĘŚĆ 1 ####################
library(vcdExtra)
library(dplyr)
library(xtable)
#zad1
dane <- read.csv2("personel.csv", col.names = c("D", "S", "A1", "A2", "W1", "W2", "P", "Wiek", "Wyk"))
View(dane)
print(dane)

table(dane['A1'])
table(dane['W1'])

#dla A1
library(xtable)
dane_a1 <- dane %>% group_by(A1) %>% count()
print(xtable(dane_a1 %>% t(),caption="Tabela liczności zmiennej A1"), include.colnames = FALSE, table.placement = "H")

dane_k <- dane %>% filter(P=="K") %>% group_by(A1) %>% count()
print(xtable(dane_k %>% t(),caption="Tabela liczności zmiennej A1 dla kobiet"), table.placement = "H", include.colnames = FALSE)

dane_m <- dane %>% filter(P=="M") %>% group_by(A1) %>% count()
print(xtable(dane_m %>% t(),caption="Tabela liczności zmiennej A1 dla mężczyzn"), table.placement = "H", include.colnames = FALSE)



dane_zaopatrzenie <- dane %>% filter(D=="Z") %>% group_by(A1) %>% count()
print(xtable(dane_zaopatrzenie %>% t(),caption="Tablica liczności zmiennej A1 dla działu zaopatrzenia"), table.placement = "H", include.colnames = FALSE)

dane_produkcja <- dane %>% filter(D=="P") %>% group_by(A1) %>% count()
print(xtable(dane_produkcja %>% t(),caption="Tablica liczności zmiennej A1 dla działu produkcji"), table.placement = "H", include.colnames = FALSE)

dane_sprzedaz <- dane %>% filter(D=="S") %>% group_by(A1) %>% count()
print(xtable(dane_sprzedaz %>% t(),caption="Tablica liczności zmiennej A1 dla działu sprzedaży"), table.placement = "H", include.colnames = FALSE)

dane_obsluga <- dane %>% filter(D=="O") %>% group_by(A1) %>% count()
print(xtable(dane_obsluga %>% t(),caption="Tablica liczności zmiennej A1 dla działu obsługi"), table.placement = "H", include.colnames = FALSE)


dane_zaw <- dane %>% filter(Wyk=="1") %>% group_by(A1) %>% count()
print(xtable(dane_zaw %>% t(),caption="Tablica liczności zmiennej A1 dla wykształcenia zwowodwego"), table.placement = "H", include.colnames = FALSE)

dane_sr <- dane %>% filter(Wyk=="2") %>% group_by(A1) %>% count()
print(xtable(dane_sr %>% t(),caption="Tablica liczności zmiennej A1 dla wykształcenia średniego"), table.placement = "H", include.colnames = FALSE)

dane_wyz <- dane %>% filter(Wyk=="3") %>% group_by(A1) %>% count()
print(xtable(dane_wyz %>% t(),caption="Tablica liczności zmiennej A1 dla wykształcenia wyższego"), table.placement = "H", include.colnames = FALSE)


#dla zmiennej W1
dane_w1 <- dane %>% group_by(W1) %>% count()
print(xtable(dane_w1 %>% t(),caption="Tabela liczności zmiennej W1"), table.placement = "H", include.colnames = FALSE)

dane_k <- dane %>% filter(P=="K") %>% group_by(W1) %>% count()
print(xtable(dane_k %>% t(),caption="Tabela liczności zmiennej W1 dla kobiet"), table.placement = "H", include.colnames = FALSE)

dane_m <- dane %>% filter(P=="M") %>% group_by(W1) %>% count()
print(xtable(dane_m %>% t(),caption="Tabela liczności zmiennej W1 dla mężczyzn"), table.placement = "H", include.colnames = FALSE)



dane_zaopatrzenie <- dane %>% filter(D=="Z") %>% group_by(W1) %>% count()
print(xtable(dane_zaopatrzenie %>% t(),caption="Tablica liczności zmiennej W1 dla działu zaopatrzenia"), table.placement = "H", include.colnames = FALSE)

dane_produkcja <- dane %>% filter(D=="P") %>% group_by(W1) %>% count()
print(xtable(dane_produkcja %>% t(),caption="Tablica liczności zmiennej W1 dla działu produkcji"), table.placement = "H", include.colnames = FALSE)

dane_sprzedaz <- dane %>% filter(D=="S") %>% group_by(W1) %>% count()
print(xtable(dane_sprzedaz %>% t(),caption="Tablica liczności zmiennej W1 dla działu sprzedaży"), table.placement = "H", include.colnames = FALSE)

dane_obsluga <- dane %>% filter(D=="O") %>% group_by(W1) %>% count()
print(xtable(dane_obsluga %>% t(),caption="Tablica liczności zmiennej W1 dla działu obsługi"), table.placement = "H", include.colnames = FALSE)


dane_zaw <- dane %>% filter(Wyk=="1") %>% group_by(W1) %>% count()
print(xtable(dane_zaw %>% t(),caption="Tablica liczności zmiennej W1 dla wykształcenia zwowodwego"), table.placement = "H", include.colnames = FALSE)

dane_sr <- dane %>% filter(Wyk=="2") %>% group_by(W1) %>% count()
print(xtable(dane_sr %>% t(),caption="Tablica liczności zmiennej W1 dla wykształcenia średniego"), table.placement = "H", include.colnames = FALSE)

dane_wyz <- dane %>% filter(Wyk=="3") %>% group_by(W1) %>% count()
print(xtable(dane_wyz %>% t(),caption="Tablica liczności zmiennej W1 dla wykształcenia wyższego"), table.placement = "H", include.colnames = FALSE)



graphics.off()
library(epiDisplay)
tab1(dane['A1'], main='Tablica liczności zmiennej A1',  xlab='Ocena atmosfery w pracy', ylab='Ilość głosów')
tab1(dane['W1'], main='Tablica liczności zmiennej W1',  xlab='Zadowolenie z wynagordzenia', ylab='Ilość głosów')

#zad2
dane_a <- dane %>% group_by(A1) %>% count()

dane_kobiety <- dane %>% filter(P=="K")

table(dane_kobiety["A1"])
dane1 <- dane_kobiety %>% group_by(A1) %>% count()

structable(W1~P,dane)
structable(W1~S,dane)
structable(A1~D,dane)

View(structable(W1~P,dane))
View(structable(W1~S,dane))
View(structable(A1~D,dane))

struc1 <- structable(W1~P, dane)
print(xtableFtable(ftable(struc1), method = "compact", caption="Tabela wielodzielcza uwzględniająca zmienną W1 i P."))

struc2 <- structable(W1~S, dane)
print(xtableFtable(ftable(struc2), method = "compact", caption="Tabela wielodzielcza uwzględniająca zmienną W1 i S."))

struc3 <- structable(A1~D, dane)
print(xtableFtable(ftable(struc3), method = "compact", caption="Tabela wielodzielcza uwzględniająca zmienną A1 i D."))



#zad3
pie(table(dane$W1), col=c("red", "blue", "yellow", "green"))
pie(table(dane$W2), col=c("red", "blue", "yellow", "green"))

barplot(table(dane$W1), main="Ocena w pierwszym okresie",xlab="Zadowolenie z zarobków",ylab="Liczba głosów", col=c("red", "blue", "yellow", "green"))
barplot(table(dane$W2),main="Ocena w drugim okresie", xlab="Zadowolenie z zarobków",ylab="Liczba głosów", col=c("red", "blue", "yellow", "green"))

#zad4
mosaicplot(~D+A1,dane,shade=FALSE,color=c('red', 'orange', 'yellow', 'green', 'darkgreen'), main='Wykres mozaikowy zmiennych D i A1')
mosaicplot(~D+W1,dane,shade=FALSE,color=c('red', 'orange', 'green', 'darkgreen'), main='Wykres mozaikowy zmiennych D i W1')
mosaicplot(~S+P,dane,shade=FALSE,color=c('orange', 'darkgreen'), main='Wykres mozaikowy zmiennych S i P')
mosaicplot(~P+W1,dane,shade=FALSE,color=c('red', 'orange', 'green', 'darkgreen'), main='Wykres mozaikowy zmiennych P i W1')


##################### CZĘŚĆ 2 ####################
#zad5
library(stats)
#dane = personel, losowanie ze zwracaniem (proste niezależne), bez zwracania (proste zależne)
#True dla niezaleznych, False dla zależnych
zal1 <- sample(nrow(dane), size=0.1*nrow(dane), replace = FALSE)
zal0 <- sample(nrow(dane), size=0.1*nrow(dane), replace = TRUE)


#zad6
library(likert)
dane$A1_l <- as.factor(dane$A1)
dane$A2_l <- as.factor(dane$A2)
dane$A1
X <- likert(dane["A1_l"])
summary(X)
plot(X)
likert.density.plot(X)
likert.bar.plot(X)

Y <- likert(dane["A2_l"])
summary(Y)
likert.density.plot(Y)
likert.bar.plot(Y)


#zad7
#1) qbeta(alpha/2, x, n-x+1), 2) qbeta(1-alpha/2, x+1, n-x)
# z uwagą rozkład F-Snedekora 1) qf(alpha/2, 2x, 2(n-x+1)), 2) qf(1-alpha/2, 2(x+1), 2(n-x))
library(binom)
Clopper <- function(alpha, x, n) {
  l <- qbeta(alpha/2, x, n-x+1)
  u <- qbeta(1-alpha/2, x+1, n-x)
  if (x==0) {
    L <- 0
  } else {
    L <- l
  }
  if (x==n) {
    P <- 1
  } else {
    P <- u
  }
  paste("Dolna granica: ", L, "Górna granica: ", P)
}
  
y <- 0
i <- 1
while (i<=200) {
  if (dane$W1[i] == 1 | dane$W1[i] == 2) {
    y <- y+1
    i <- i+1
  } else {
    i <- i+1
  }
}
y
  
x <- y
n <- 200
alpha <- 0.05
binom.confint(x, n, conf.level=1-alpha, methods="exact")
Clopper(alpha,x,n)


Snedecor <- function(alpha, x, n) {
  l <- qf(alpha/2, 2*x, 2*(n-x+1))
  u <- qf(1-alpha/2, 2*(x+1), 2*(n-x))
  if (x==0) {
    L <- 0
  } else {
    L <- l
  }
  if (x==n) {
    P <- 1
  } else {
    P <- u
  }
  paste("Dla snedecora Dolna granica: ", L, "Górna granica: ", P)
}
  
Snedecor(alpha,x,n)


##################### CZĘŚĆ 3 ####################

#zadanie 8

binomi<-function(N,n,p){
  bin<-c()
  for (i in 1:N){
    U<-runif(n,min=0,max=1)
    inds<-sum(U<p)
    bin<-append(bin,inds)
  }
  return(bin)
}
x<-binomi(10000,30,0.4)
y<-rbinom(10000,30,0.4)
hist(x, col='green', xlab='Liczba sukcesów', main='Histogramy dla N=10000, n=30, p=0.4', ylab='Częstość występowania')
hist(y, col='darkgreen', add=TRUE)
legend('topright', c('binomi', 'rbinom'), fill=c('green', 'darkgreen'))


plot(ecdf(x), main='Dystrybuanty empiryczne dla N=10000, n=30, p=0.4', 
     xlab='Liczba sukcesów', col='green', ylab=' ')
lines(ecdf(y), col='darkgreen')
legend('bottomright', c('binomi', 'rbinom'), fill=c('green', 'darkgreen'))

#zadanie 9
library(binom)
library(lattice)
n = 30
x = rbinom(1, n, 0.4)
binom.confint(x, n, conf.level = 0.95, methods = "exact")
binom.confint(x, n, conf.level = 0.95, methods = "asymptotic")
binom.confint(x, n, conf.level = 0.95, methods = "ac")

#method = binom.lrt
n=100

binom.plot(n, methods = "exact", np = 1000,
           conf.level = 0.95, 
           type = c("xyplot"))

binom.plot(n, methods = "asymptotic", np = 1000,
           conf.level = 0.95, 
           type = c("xyplot"))



library(binom)
library(lattice)
srednia_dlugosc1 <- list()
srednia_dlugosc2 <- list()
srednia_dlugosc3 <- list()

p_list <- seq(0.1, 0.9, by=0.01)
for (i in p_list){
  N <- 1000
  n <- 1000
  p <- i
  x <- rbinom(N, n, p)
  result1 <- binom.confint(x, n, conf.level=0.95, methods="exact")
  result2 <- binom.confint(x, n, conf.level=0.95, methods="asymptotic")
  result3 <- binom.confint(x, n, conf.level=0.95, methods="ac")
  
  len1 <- result1["upper"] - result1["lower"]
  len2 <- result2["upper"] - result2["lower"]
  len3 <- result3["upper"] - result3["lower"]
  
  srednia1 <- sum(len1)/N
  srednia2 <- sum(len2)/N
  srednia3 <- sum(len3)/N
  
  srednia_dlugosc1 <- append(srednia_dlugosc1, srednia1)
  srednia_dlugosc2 <- append(srednia_dlugosc2, srednia2)
  srednia_dlugosc3 <- append(srednia_dlugosc3, srednia3)
}


plot(p_list, srednia_dlugosc1, xlab="p", ylab="Średnia długość przedziału",
     main="Wykresy średnich długości przedziałów ufności",
     type = "l", col=('red'))

lines(p_list, srednia_dlugosc2, type = "l", col=('darkgreen'))

lines(p_list, srednia_dlugosc3, type = "l", col=('orange'))


legend("bottom", legend=c("Cloppera-Pearsona", "Walda", "Agrestiego-Coulla"), 
       col=c('red', 'darkgreen', 'orange'), lty=1:1, cex=1)

#################### SAM CP
pr_pokrycia <- list()
p_list <- seq(0, 1, by=0.01)
for (i in p_list){
  n <- 30
  N <- 1000
  p <- i
  x <- rbinom(N, n, p)
  result <- binom.confint(x, n, conf.level=0.95, methods="exact")
  p_in <- p > result["lower"] & p < result["upper"]
  pokrycie <- sum(p_in)/N
  pr_pokrycia <- append(pr_pokrycia, pokrycie)
}

plot(p_list, pr_pokrycia, xlab="p", ylab="Prawdopodobieństwo pokrycia",
     main="Wykres prawdopodobieństwa pokrycia dla przedziałów Cloppera-Pearsona",
     type = "l", col=('red'))

legend("bottom",legend=c("Cloppera-Pearsona"), col=c('red'), lty=1:1, cex=1)
####################

pr_pokrycia1 <- list()
pr_pokrycia2 <- list()
pr_pokrycia3 <- list()
skr <- list()
p_list <- seq(0.1, 0.9, by=0.01)
for (i in p_list){
  n <- 1000
  N <- 1000
  p <- i
  x <- rbinom(N, n, p)
  result1 <- binom.confint(x, n, conf.level=0.95, methods="exact")
  result2 <- binom.confint(x, n, conf.level=0.95, methods="asymptotic")
  result3 <- binom.confint(x, n, conf.level=0.95, methods="ac")
  p_in1 <- p > result1["lower"] & p < result1["upper"]
  p_in2 <- p > result2["lower"] & p < result2["upper"]
  p_in3 <- p > result3["lower"] & p < result3["upper"]
  pokrycie1 <- sum(p_in1)/N
  pokrycie2 <- sum(p_in2)/N
  pokrycie3 <- sum(p_in3)/N
  pr_pokrycia1 <- append(pr_pokrycia1, pokrycie1)
  pr_pokrycia2 <- append(pr_pokrycia2, pokrycie2)
  pr_pokrycia3 <- append(pr_pokrycia3, pokrycie3)
  skr <- append(skr, 0.85)
}

plot(p_list, pr_pokrycia1, xlab="p", ylab="Prawdopodobieństwo pokrycia",
     main="Wykresy prawdopodobieństwa pokrycia przedziałów ufności",
     type = "l", col=('red'))

lines(p_list, skr, type = "l", col=('white'))

plot(p_list, pr_pokrycia3, xlab="p", ylab="Prawdopodobieństwo pokrycia",
     main="Wykresy prawdopodobieństwa pokrycia przedziałów ufności",
     type = "l", col=('orange'))

lines(p_list, pr_pokrycia1, type = "l", col=('red'))

lines(p_list, pr_pokrycia2, type = "l", col=('darkgreen'))

lines(p_list, pr_pokrycia3, type = "l", col=('orange'))


legend("bottom", legend=c("Cloppera-Pearsona", "Walda", "Agrestiego-Coulla"), 
       col=c('red', 'darkgreen', 'orange'), lty=1:1, cex=1)






