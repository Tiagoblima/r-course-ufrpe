MRT_1F <-c(517.1468515630205, 85.13094142168089, 30.333207896694553, 12.694776264558937, 3.3041601673945418, 1.1823111717498882, 1.1892293502386786)

MRT_3F <-c(156.68929936163462, 11.540837783562276, 0.4512835621696538, 0.4509797929766453, 0.4502068233039181, 0.4496185276300172, 0.4543157082191288)

MRT_5F <-c(83.90319666471157, 0.3068151086494968, 0.30522314133037304, 0.3072588968084928, 0.30655265997285697, 0.3055812715727718, 0.3053297166713006)

MRT_10F <-c(29.55430642951759, 0.19832832665772515, 0.1971923924717474, 0.19796648905716516, 0.19615594370806338, 0.2034569237883263, 0.19617420889447737)

MRT_15F <-c(11.317736530583566, 0.167364215666193, 0.16172168266811013, 0.16701085329580515, 0.1598052657153692, 0.1645934043532696, 0.16216563797118075)

MRT_sem_F <-c(11.93430909937736, 0.6095414637034009, 0.6060645101029295, 0.612167181646899, 0.6146761002685637, 0.6096747087200697, 0.6125810476877268)

clock <- c(0.1, 0.5, 1, 1.5, 2, 2.5, 3)


plot(clock,MRT_1F, 
     type = "o", 
     
     main="Gráfico",
     xlab = "Time Between Requests",
     ylab="Responses Time",
     xlim=c(0, 3),
     ylim=c(0, 700),
     col="red")

lines(MRT_3F, type = "o", col='Blue' , pch=2)

lines(MRT_5F, type = "o", col='Green', pch=3)

lines(MRT_10F, type = "o", col='Yellow', pch=4)

lines(MRT_15F, type="o", col="Black", pch=5)

lines(MRT_sem_F, type="o", col="Purple", pch=6)

legend("topright", pch = c(1,2,3,4, 5,6), col = c("red", "blue", "green", "yellow", "Black"),
       legend=c("MRT_3F", "MRT_5F", "MRT_5F", "MRT_10F", "MRT_15F", "MRT_sem_F"))


layout(matrix(c(1, 2,
                3, 4,
                5,6), nr=3,
              byrow=T))

barplot(MRT_1F, col='red' , 
        
      
       
        xlab = "Time Between Requests",
        ylab="Responses Time",
        xlim=c(0, 3),
   
        names.arg=clock,
        ylim=c(0, 400))

barplot(MRT_3F, col='Blue' , 
        names.arg=clock,
       
        xlab = "Time Between Requests",
        ylab="Responses Time",
        xlim=c(0, 3),
        ylim=c(0, 100),)

barplot(MRT_5F, col='Green', 
        
        names.arg=clock,
        xlab = "Time Between Requests",
        ylab="Responses Time",
        xlim=c(0, 3),
        ylim=c(0, 60),)

barplot(MRT_10F, col='Yellow', 
        
        names.arg=clock,
        xlab = "Time Between Requests",
        ylab="Responses Time",
        xlim=c(0, 3),
        ylim=c(0, 20),)

barplot(MRT_15F, col="Black", 
        
        names.arg=clock,
        xlab = "Time Between Requests",
        ylab="Responses Time",
        xlim=c(0, 3),
        ylim=c(0, 8),)

barplot(MRT_sem_F, col="Purple", 
        
        names.arg=clock,
        xlab = "Time Between Requests",
        ylab="Responses Time",
        xlim=c(0, 3),
        ylim=c(0, 8),)
layout.show(n=4)






values <- matrix(
  c(53.8,43.6,2.6,
    33.9, 54.2, 11.9,
    2.6, 60.5, 36.8,
    0.0, 21.4, 78.6),
  nrow = 3,
  ncol=4,
)

colors <- c("red","green", "blue", "black")
par(mfrow=c(1,1))
prices <- c("$10-19", "$20-29", "$30-39", "$40-49")
barplot(values, main="Food Quality", 
        xlab="Price",
        ylab="Quality %",
        names.arg =prices, col=colors )


Quality <- c("Good", "Very Good", "Excellent")




legend("topright", pch = c(15,15,15), col =colors,
       legend=Quality)




head(airquality)


airMay <- subset(airquality, airquality$Month==5)

head(airMay)

to_celsius <- function(temp) {
  
  return((temp − 32) / 1.8)
}

airMay$Temp <- sapply(airMay$Temp,FUN=to_celsius )


TemperaturaMaio <- airMay$Temp
hist(TemperaturaMaio, col = "blue", density = 10, freq = F)
lines(density(TemperaturaMaio))



sales <- read.table("https://training-course-material.com/images/8/8f/Sales.txt",header=TRUE)




per <- round((sales$SALES*100)/sum(sales$SALES))

labels <- sales$COUNTRY

lbls <- paste(per,"%", sep = "")

pie(sales$SALES, lbls, main="Gráfico de Vendas", col=rainbow(nrow(sales)))

legend("topleft", legend = labels, cex = 0.7, fill=rainbow(nrow(sales)))




head(InsectSprays)

boxplot(count ~ spray, data = InsectSprays 
        , col=c("yellow"),
        xlab = "Contagem de Insetos", 
        ylab="Spray",
        main="Insetos/Spray")



head(mtcars)

plot(mtcars$wt, mtcars$mpg, main="MtCars: Wt vs Mpg",
       xlab = "Peso", ylab="Milhas/h")


abline(v = mean(mtcars$wt), col="red", lwd=3, lty=2)




monitoringCloudData_0.1 <- read.csv('datasets/monitoringCloudData_0.1.csv')

head(monitoringCloudData_0.1)

























