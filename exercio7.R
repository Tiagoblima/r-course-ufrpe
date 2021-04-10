

V1 <- read.table('Pica-pau.txt', header = TRUE, dec='.')

sum(V1$especie=="verde")


Caracol_data <- as.data.frame(read.csv(file="https://www.dropbox.com/s/9wnr69i6bjhqyct/Snail_feeding.csv?dl=1",
                                       header = T,
                                       strip.white = T,
                                       na.strings = ""))
str(Caracol_data)

Caracol_data <- Caracol_data[,1:7]


Caracol_data$Sex <- replace(as.character(Caracol_data$Sex), Caracol_data$Sex == "males", "male")

Caracol_data$Sex <- replace(as.character(Caracol_data$Sex), Caracol_data$Sex == "Male", "male")

Caracol_data$Sex <- replace(as.character(Caracol_data$Sex), Caracol_data$Sex == "female s", "female")

unique(Caracol_data$Sex)

Caracol_data$Distance <- as.numeric(Caracol_data$Distance)

which(is.na(Caracol_data$Distance))

Caracol_data[682, "Distance"] <- 0.58

Caracol_data[755, "Distance"] <- 0.356452

which(duplicated(Caracol_data))

index <- which(duplicated(Caracol_data))

Caracol_data <- Caracol_data[-index, ]

summary(Caracol_data)



Caracol_data[which(Caracol_data$Depth > 2),]

Caracol_data[8, 6] <- 1.62

mean(Caracol_data$Depth, na.rm = T)




sb <- subset(x=Caracol_data, subset = Sex=="female")
max(sb$Distance)

# -------------------------------------



# Questão 1

catsM <- as.data.frame(read.csv('https://www.dropbox.com/s/w4xv9urbowbig3s/catsM.csv?dl=1'))

str(catsM)

mean(catsM$Bwt, na.rm=TRUE)



#--------------------------------------------

Sparrows <- read.table('https://www.dropbox.com/s/jci311cfsj6uva7/Sparrows.csv?dl=1', header = T, sep=",")

Sparrows <- as.data.frame(read.csv('https://www.dropbox.com/s/jci311cfsj6uva7/Sparrows.csv?dl=1'),
                          header = T,
                          strip.white = T,
                          na.strings = "")
indexes <- which(duplicated(Sparrows))

Sparrows <- Sparrows[-indexes, ]

str(Sparrows)
View(Sparrows_tab)
View(Sparrows)

head_SSTS <- Sparrows[Sparrows$Species == "SSTS",]$Head
min(head_SSTS)
max(head_SSTS, na.rm = T)

unique(Sparrows$Sex)

Sparrows$Sex <- replace(as.character(Sparrows$Sex), Sparrows$Sex == "Males", "Male")

Sparrows$Sex <- replace(as.character(Sparrows$Sex), Sparrows$Sex == "Femal", "Female")

Sparrows$Sex <- replace(as.character(Sparrows$Sex), Sparrows$Sex == "Femal e", "Female")

median(Sparrows[Sparrows$Sex == "Female","Tarsus"])
median(Sparrows[Sparrows$Sex == "Male","Tarsus"])

which(is.na(Sparrows$Wing))

Sparrows$Wing[62] <- 59
Sparrows$Wing[247] <- 56.5
Sparrows$Wing[803] <- 57

median(Sparrows$Wing)
which(is.na(Sparrows$Head))
Sparrows$Head[811] <- 0
Sparrows_Ordenado <- Sparrows[order(Sparrows$Wing, Sparrows$Head),]

