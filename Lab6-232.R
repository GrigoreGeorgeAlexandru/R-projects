#Lucrul cu pachetul DiscreteRV
X <- RV(c(-5,2,3,5,9),c(1/10,3/10,2/10,1/10,3/10))
#P(X<4)
P(X<4)
library(MASS)
fractions(P(X<4))

#P(X<4|X>=2)=5/10 / 9/10
#comportament ciudat al functiei fractions
fractions(P(X<4|X>=2))
#Nu vrea, nu stim de ce!!! :)
fractions(round(P(X<4|X>=2), digits=2))
fractions(0.56)
#P(2<X<9)
P((X>2)%AND%(X<9))

X1 <- RV(1:100)
P(X1>90)
X1[15:20]

X2 <- RV(-50:50)
X2[3:5]
probs(X2)[3:5]

#OBS: Daca avem o v.a. cu un numar mai mare de 12 valori pentru a vizualiza v.a.
#de la un anumit indice incolo folosim probs()[]
str(probs(X2)[3:5])
str(probs(X2)[3])

plot(X)
#Atentie la cum faceti plot
plot(X1[1:13])

X3 <- RV(c(-1,0,2),c(1/3,1/2,1/6))
#Functia de repartitie pentru X3
X4 <- RV(c(0,3),c(1/6,5/6))

FrepX4 <- function(x)
{
  ifelse(x<0,h <- 0,ifelse(x<3,h <- 1/6,h <- 5/6))
  return(h)
}
#Reprezentam grafic FrepX4
t <- seq(-1,4,0.001)
#Nu merge, deoarece FrepX4 nu stie sa evalueze decat valori numerice, nu si vectori
#Adaptati functia FrepX4 a.i. sa stie sa primeasca ca parametru de intrare
#un vector de valori
plot(t,FrepX4(t),col="magenta")

#De continuat pentru FrepX3
FrepX3 <- function(x)
{
  if(x<-1) h <- 0
  
}