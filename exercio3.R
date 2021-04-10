x <- factor(c("Solteiro", "Casado", "Casado", "Solteiro"))
x <- factor(x, levels=c("Solteiro", "Casado", "Divorciado"))
x[2] <- "Divorciado"
x[3] <- "Viuvo"
levels(x) <- c(levels(x), "viuvo")
levels(x)[1] <- "não casado"
length(gl(2,100,labels = c("M", "F")))


# Questão 2
drinks <- factor(c("beer","beer","wine","water"))
t <- table(drinks)
a <- as.array(table(drinks))
p<-t["beer"]/mean(a)

drinks <- factor(c("beer","beer","wine","water"))
levels(drinks)[1] <- "water"


nomes=c("João", "Paula", "Maria", "Ingrid", "José", "Marcos")
pesos=c(80, 65, 70, 58, 78, 70)
alturas=c(1.70, 1.66, 1.65, 1.60, 1.76, 1.70)

lista_pessoas <- list(
  nomes=c("João", "Paula", "Maria", "Ingrid", "José", "Marcos"),
  pesos=c(80, 65, 70, 58, 78, 70),
  alturas=c(1.70, 1.66, 1.65, 1.60, 1.76, 1.70)
)
str(lista_pessoas)

lista_pessoas$IMC = pesos/(alturas^2)
lista_pessoas[[4]][3]



lista<-lapply(airquality, function(x){mean(x)})
lista$Temp
load("chuvas.RData")
chuvas[(1:30)]
aux<-chuvas["mun_81" , ]; aux<-sum(aux[1:10])
s<-summary(chuvas, na.rm = TRUE)
s[2]
rownames(chuvas)
aux<-chuvas[81 , ]; aux<-sum(aux[1:10])

aux<-chuvas["mun_81" ]; aux<-sum(aux[1:10])
aux<-chuvas["mun_81" , ]; aux<-sum(aux[c(1:10)])

aux <- chuvas["mun_81", ]; aux <- sum

View(chuvas[max(rowSums(chuvas))==rowSums(chuvas),])
View(chuvas[max(chuvas)==chuvas[,]])
