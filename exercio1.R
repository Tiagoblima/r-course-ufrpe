X <- 133
Y <- 36
Z <- X+Y
Z <- sqrt(Z)
Z <- round(log(Z))

round(3.44343, 1)


fib <- 0
aux1 <- 0
aux2 <- 1
fib <- aux1 + aux2
print(aux1)
print(aux2)
soma <- fib
for (i in 0:2){
  
  fib <- aux1 + aux2
  aux1 <- aux2
  aux1 <- fib
  print(fib)
  soma <- soma + fib
 
}

resultado <- round(log(soma), 3)
install.packages("numDeriv")
library("numDeriv")


f = function(x) sin(x) + x 

cat("\nUsing D() function:\n") 
print(round(grad(f, 2), 3))

round(log(factorial(10), 10), 2)


e <- 10 * 300000000 ^ 2
round(log(e),2)

g <-  function(x) x^3 + x^2 + x
v<-c(2,5,7,8,9)
print(v)
mean(sapply(v,f))


