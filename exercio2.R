
boat.names <- c("a","b", "c", "d")
boat.color <- c(143, 53, 356,23)
boat.prices <- c(53, 87, 54, 66)
boat.costs <- c(53, 80, 20, 100)

boat.names[(boat.prices > 70 | boat.prices<60)]


test.nomes <- c("Maria", "Joana")
test.abdominais <- c(42, 38)
test.salto <- c(102, 173)
test.suspensao <- c(38, 71)
test.correr <- c(2149, 1554)
test.con <- c(97, 70)


test.abdominais.n <- (test.abdominais-30)/6
test.salto.n <- (test.salto-155)/23
test.suspensao.n <- (test.suspensao-50)/8
test.correr.n <- (test.correr-1829)/274
test.con.n <- (test.con-75)/12

media <- (test.abdominais.n+test.salto.n+ test.suspensao.n+test.correr.n+ test.con.n)/5

print(test.nomes[max(media)==media])




v <- load("vetor.RData")
summary(vetor01, na.rm = TRUE)
sd(vetor01, na.rm = TRUE)
vetor01 <- vetor01[!is.na(vetor01)]
length((vetor01[(vetor01 > 7.00 & vetor01 < 8.00)]))

length(vetor01[((vetor01 > 9) | (vetor01 < 1))])



