# Constructia de functii in R

f <- function()
{
  #optional return()
}

f1 <- function(x)
{
  x^(1/4)
}
f1(16)
#Pentru reprezentari grafice mereu facem intai discretizarea intervalului
t <- seq(0,20,0.001)
t[1:5]
plot(t,f1(t),col="magenta")

#Functia integrate
a1 <- integrate(f1,5,10)

#Extragem din a1 doar valoarea integralei cu operatorul $
a1$value
a1$abs.error

#Folosim $ si pentru a extrage o coloana dintr-un dataframe
#Sa nu uitam sa facem import bibliotecii prob
library(prob)
s <- cards()
#Functia cards primeste ca argument jokers=T daca dorim pachetul cu 54 de carti
cards(jokers=T)
str(s)
s$rank
#Tema: Folosindu-va de functia cards() si extragerea unei submultimi din ea
#calculati probabilitatea ca extragand o carte sa obtinem o valoarea <7
#de inima

######################################################################

#Revenim la functii

#Sa construim o functie care sa implementeze comportamentul functiei gama
#varianta 1

f2 <- function(x,a)
{
  x^(a-1)*exp(-x)
}

a2 <- integrate(f2,0,Inf,a=2)$value

#Tema creati o functie in R numita gama_nume care sa implementeze proprietatile
#pe care le are functia gama(vezi documentul Integrale euleriene) si sa 
#foloseasca apelul functiei integrate doar atunci cand parametrul nu satisface 
#nicio conditie "buna"

# gama_nume <- function(....)
#{
#daca n e natural atunci foloseste propr. 3)     #folosim functia din R numita factorial
#daca n e de forma b/2(cu b natural) foloseste formula 2) si 4)
#altfel foloseste formula 2) pana cand argumentul devine subunitar
#si doar pentru acea valoare calculeaza cu integrate

#}

#Compunerea a doua functii
#TO DO
f3 <- function(f,g)
{
  h <- function(x)
  {
    f(g(x))
  }
  return(h)
}
f11 <- function(x)
{
  x^4
}
f3(f1,f11)(5)

#BONUS: a) Generalizati compunerea a doua functii impunand restrictii cu privire la domeniu si codomeniu
#astfel incat compunerea sa fie realizabila
#       b) Scrieti o functie in R care intoarce inversa unei functii date ca parametru(cu validarea faptului ca e bijectiva)