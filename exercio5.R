head(iris)

apply(iris[ , 1:4], 2, FUN = mean)
sapply(iris[ , 1:4], 2, FUN = mean)
mapply(iris[ , 1:4], mean)

tapply(iris[,3], iris$Species, mean)

tapply(iris$Petal.Length, iris$Species, mean)

with(iris, tapply(Petal.Length, Species, mean))

for (i in 1:4){ }

for (i in 1:4){ break }

for (i in 1:4) {next }


head(USArrests)
apply(USArrests[ , 1:4], 2, FUN = sum)


student.df = data.frame (nome= c ("Sue", "Eva", "Henry", "Jan"), sexo= c ("f", "f", "m", "m"), anos= c (21,15,17,19))
saida <- vector("character", nrow(student.df))
seq <- 1:4
for (i in seq){
  if (student.df[i,]["sexo"]=="m"){
    if (student.df[i,]["anos"] < 18){
      saida[i] <- "V"
    }else{
      saida[i] <- "F"
    }
  }else{
    saida[i] <- "F"
  }
}
class(saida)
student.df$menor <- saida


student.df


for (i in 1:length(1:3)){
  for(j in 1:10){
    print(i+(j-1))
  }
}
mapply( FUN=function(n){ rep(c("Rural", "Amo")[(n%%2)+1], n)}, 10:1)
v <- c("Rural", "Amo")
print(v[2])

for (i in 1:10){
  print((i%%2)+1)
  print(v[(i%%2)+1])
}
mapply(rep, c("Rural", "Amo"), 10:1)

for (i in 1:4) {next }

x <- 0

while(x <100){
  x <- x+2
}
print(x)
a<-0
x<-0
if(a>0){
  
}else{
  x <- x+2
}
print(x)
