
install.packages("splitstackshape")
library(splitstackshape)
View(airquality)

options(digits = 4)
mean(airquality$Temp)

rowMeans(airquality[c("Temp", "Wind")])

median(airquality$Temp)

y <- table(airquality$Temp)

names(y)[which(y==max(y))]

moda <- function(x){
  
  y <- table(x)
  
  return(names(y)[which(y==max(y))])
  
}

moda(airquality$Temp)


summary(airquality$Temp)


min(airquality$Temp)



max(airquality$Temp)


amp <- max(airquality$Temp)-min(airquality$Temp)


var(airquality$Temp)

sd(airquality$Temp)

mean(airquality$Temp)

cat("[", mean(airquality$Temp)-sd(airquality$Temp), "-", mean(airquality$Temp)+sd(airquality$Temp), "]")


x <- c("cara", "coroa")

table(sample(x, size=1000, replace = TRUE, prob=c(0.2, 0.8)))


stratified(iris, group = "Species", size=5, replace = T)



View(infert)

levels(infert$education)

p1 <- sum(infert$education=="0-5yrs")/length(infert$education)
p2 <- sum(infert$education=="6-11yrs")/length(infert$education)
p3 <- sum(infert$education=="12+ yrs")/length(infert$education)

stratified(infert, group = "education", size=c(0.1, 0.4, 0.5), replace = T)

#Você deseja estimar a renda média dos ex-alunos da Ruralinda; 
#admite-se que esta população seja infinita.

sd <- 4250

erro <- 300

nc <- (1-0.90)/2

n <- ((qnorm(nc, lower.tail=F)*sd)/erro)^2


#Suponha que a variável escolhida num estudo seja o peso dos alunos da Ruralinda
#e que a população é composta de 1500 alunos.

sd <- 5

N <- 1500

erro <- 1.5

nc <- (1-0.955)/2


n <- (qnorm(nc, lower.tail = F)^2*sd^2*N)/((erro^2*(N-1))+(qnorm(0.975)^2*sd^2))


#A ministro da educação está interessado em conhecer o gasto médio por estudante
#nos restaurantes universitários.


sd <- 10

erro <- 1

nc <- (1-0.98)/2

n <- ((qnorm(nc, lower.tail=F)*sd)/erro)^2



#Supondo que a mesma pesquisa seja realizada na Rural e que os valores acima 
#sejam verdadeiros para a Rural. 


sd <- 10

N <- 5000

erro <- 1

nc <- (1-0.98)/2


n <- (qnorm(nc, lower.tail = F)^2*sd^2*N)/((erro^2*(N-1))+(qnorm(0.975)^2*sd^2))


z <- (30-50)/10



set.seed(100)

head(rnorm(50, mean = 2.5, sd=0.1))


x <- rnorm(100, mean = 50, sd=10)

y <- dnorm(x)



plot(x,y)

#Em um hospital psiquiátrico, os pacientes permaneceram internados, em média, 
#cinquenta dias, com desvio padrão de dez dias. 

#Qual a proporção de casos acima de z = 1 ? 
(1-pnorm(60, mean = 50, sd=10))/1

#Qual a proporção de casos abaixo de z = -2 ?


(pnorm(30, mean = 50, sd=10))/1


#Qual a proporção de casos acima de z = 1,28 ?
(1-pnorm(50+12.8, mean = 50, sd=10))/1




hist(cars$speed, probability = T, main="Histogram")


qqnorm(cars$speed, pch=19)

qqline(cars$speed)

shapiro.test(cars$speed)
install.packages("nortest")
library(nortest)
ad.test(airquality$Temp)

length(airquality$Temp)
hist(airquality$Temp)





e <- 3.8/sqrt(100)



print(30.2-1.96*e, "-", 30.2+1.96*e)


658-1.28*( 47/sqrt(100))

658+1.28*( 47/sqrt(100))


data <- c(4.37, 3.63, 2.78, 5.46, 2.18, 6.07, 3.24, 5.89, 4.86,4.64)


mean(data)

sd(data)



e <- sd(data)/sqrt(10)

mean(data)-1.28*(sd(data)/sqrt(10))



mean(data)+1.28*(sd(data)/sqrt(10))
