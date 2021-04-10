

#H0 Médias dos tempos de coagulação são iguais.
#H1 Médias dos tempos de coagulação são diferentes.

x <- c(8.8, 8.4, 7.9,8.7,9.1,9.6)
y <- c(9.9,9.0,11.1,9.6,8.7,10.4,9.5)
nc <- 0.95
ns <- 1-nc

aux <- t.test(x, y, alternative = "two.sided", conf.level = nc, var.equal = T)

verifcaH0(aux$p.value, ns)

verifcaNormalidade(x)
verifcaNormalidade(y)

verifcaVariancias(x, y)



verifcaH0 <- function(pvalue, ns){
  
  if(pvalue>ns){
    
    print("Aceita a Hipotése Nula")
  }else{
    
    print("Rejeita a hipotése nula")
  }
  
  
}


verifcaNormalidade <- function(x){
  
  if(length(x)<=50){
    
    x<-shapiro.test(x)
  }else{
    require(nortest)
    x <- ad.test(x)
    
  }
  
  
  if(x$p.value>0.05){
    
    print("Os dados seguem uma distribuição normal.")
  }else{
    
    print("Os dados NÃO seguem uma distribuição normal.")
  }
  
  
}



verifcaVariancias <- function(x, y){
  
  x <- var.test(x, y)
  
  if(x$p.value>0.05){
    
    print("As variâncias são Homogêneas")
  }else{
    
    print("As variância NÃO são Homogêneas")
    
  }
  
  
}


################################################################
#H0 As médias são iguais
#HA Piratas que usam tapa-olhos têm em média um número menor de tatuagens
install.packages("yarrr")
library(yarrr)

aux <- t.test(formula = tattoos ~ eyepatch,
              data = pirates,
              alternative="less",
              var.equal=T)


verifcaH0(aux$p.value, ns=0.05)

verifcaNormalidade(pirates$tattoos[pirates$eyepatch==0])
verifcaNormalidade(pirates$tattoos[pirates$eyepatch==1])

verifcaVariancias(pirates$tattoos[pirates$eyepatch==0], pirates$tattoos[pirates$eyepatch==1])


#H0 Médias são iguais 
#H1 Médias são diferentes 

aux <- t.test(
  formula = tattoos ~ age,
  data=pirates,
  subset = age %in% c(29, 30),
  alternative = "two.sided",
  var.equal = T
)

verifcaH0(aux$p.value, ns=0.05)

verifcaNormalidade(pirates$tattoos[pirates$age==29])
verifcaNormalidade(pirates$tattoos[pirates$age==30])



#H0 As médias para níveis de ozônio é a mesma
#H1 as médias para níveis de ozônio são diferentes


colnames(airquality)

aux <- wilcox.test(
  formula = Ozone ~ Month,
  data=airquality,
  paired=T,
  subset = Month %in% c(5, 6),
  alternative = "two.sided",
  var.equal = T
)

verifcaH0(aux$p.value, ns=0.05)

verifcaNormalidade(airquality$Ozone[airquality$Month==5])
verifcaNormalidade(airquality$Ozone[airquality$Month==6])

######################################################



#H0 O calibre da veia é, em média, o mesmo antes e depois.
#H1 O calibre da veia é diferente.


antes <- c(75, 50, 50, 60, 50, 70)

depois <- c(85, 75, 70, 65, 60, 90)


aux <- t.test(antes, depois, alternative = "two.sided", paired = T , var.equal = T)

verifcaH0(aux$p.value, ns=0.05)

verifcaNormalidade(antes)
verifcaNormalidade(depois)

verifcaVariancias(antes, depois)




######################################################


#H0 O suplemento não surtio efeito
#H1 O suplemento surtio efeito



antes <- c(15, 18, 20, 14, 16, 19)

depois <- c(16, 12, 15, 15, 14, 18)


aux <- t.test(antes, depois, alternative = "two.sided" , var.equal = T)

verifcaH0(aux$p.value, ns=0.05)

verifcaNormalidade(antes)
verifcaNormalidade(depois)

verifcaVariancias(antes, depois)
































