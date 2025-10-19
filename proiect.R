
#1
#functia ceruta
frepcomgen<-function(m,n){

X<-1:n
Y<-1:m
table1<-data.frame(X) #initializarea v. a. 
table2<-data.frame(Y)  


  aux<-runif(n, 0, 1)          #generarea unor numere intre 0 si 1 care 
                                #adunate dau 1 si adaugarea lor in v. a.
  table1$column<-aux/(sum(aux))
  aux<-runif(m, 0, 1)
  table2$column<-aux/(sum(aux))
  


table<-data.frame()  #initializarea tabelului pentru repartitia comuna


val<-1
for(v2 in Y){
  for(v1 in X){
    table[v1, v2]=table1[v1,2]*table2[v2,2] #inserarea valorilor in tabel
    
    
  }
  names(table)[val] <- val  #schimbarea denumirilor coloanelor
                            #pentru a pastra  forma tabelului
  val=val+1
  
}


  
  
  return(table)
}



m<-4
n<-5
t<-frepcomgen(m,n)


#calculul pentru P(0<X<8/Y>3)

sum<-0
for(v1 in 1:n){
  for(v2 in 1:m){
    if((v1>0&&v1<8) ||v2>3){
      sum=sum+t[v1,v2] 
     
      }
  }
}
print(sum)


#calculul pentru P(X>2,Y<7)

sum<-0
for(v1 in 1:n){
  for(v2 in 1:m){
    if(v1>2&&v2<7){
      sum=sum+t[v1,v2] 
      
    }
  }
}
print(sum)


#3
funct3<-function(n){
  xi<-0 #initializare x1
  nr2<-1 #initializare variabila ce memoreaza lungimea vectorului
  vector=c() #initializare vector 
while(.GlobalEnv$a<3){
  .GlobalEnv$a=.GlobalEnv$a+1

#atribuirea unei valori lui x1 dupe cerinte
nr<-as.integer(format(Sys.time(), "%M%S"))
if(nr%%12==0){
  xi<-rnorm(as.integer(format(Sys.time(), "%M")),as.integer(format(Sys.time(), "%S")))
break
  }else if(nr%%12==1){
xi<-rpois(as.integer(format(Sys.time(), "%M"))+runif(2,0,1), lambda = 4)
break
}else if(nr%%12==5){
  xi<-rexp(as.integer(format(Sys.time(), "%H")))
  break
}else if(nr%%12==7){
  xi<-rbinom(as.integer(format(Sys.time(), "%H")),as.integer(1/as.integer(format(Sys.time(), "%M"))+runif(1,0,5)),0.5)
  break
  }else if(nr%%12==8){
  xi<- runif(2,5,12)
  break
}else if(nr%%12==9){
  xi<- rgamma(1,1)-rhyper(1,1,1,1)
  break
}
}
  
  if(xi==0){
    xi<-rnorm(1,0,1)
  }

  #calculul celorlalte xn numere 
vector<-c(vector,xi)  
  for(val in 2:n){
    x2<-(rexp(1,2)*vector[nr2]+rnorm(1,5,1))
    vector<-c(vector,x2)  
    nr2<-nr2+1
    
  }
hist(vector) #afisarea histogramei vectorului
 return(vector) 
}
.GlobalEnv$a <- 0 #initializare variabila globala cu 
                  #care numar de cate ori s-a incercat atribuirea lui xi
funct3(4)





#4

#apelarea functiilor cerute
set<-PlantGrowth
mean(set$weight)
var(set$weight)
quantile(set$weight, 0.25)
quantile(set$weight, 0.75)
boxplot(set$weight, 0.75)
hist(set$weight)
