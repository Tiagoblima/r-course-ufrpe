
mouse.color <- c("purple", "red", "yellow", "brown")
mouse.weight <- c(23, 21, 18, 26)

mouse.info <- data.frame("colour" = mouse.color, "weight"=mouse.weight)
str(mouse.info)
head(mouse.info, 4,1)
tail(mouse.info, 1)

head(airquality)
min(airquality["Ozone"], na.rm = T)


sb <- subset(x=airquality, subset = Ozone>25 && Temp<90)
mean(sb$Solar.R, na.rm = T)
sum(!is.na(airquality))


airquality.n <- length(airquality[is.na(airquality)==F])
genomas <- as.data.frame(read.csv("https://www.dropbox.com/s/vgh6qk395ck86fp/genomes.csv?dl=1"))

head(genomas)

aux <- subset(x=genomas, subset = Chromosomes > 40)
aux$Organism


aux <- subset(x=genomas, subset = Plasmids > 0 & Chromosomes > 1)
View(aux$Organism)
View(genomas$Groups)
genomas$Groups


cancer_stats <- as.data.frame(read.csv("https://www.dropbox.com/s/g97bsxeuu0tajkj/cancer_stats.csv?dl=1"))

cancer_stats.txM <- cancer_stats$Male.Deaths/cancer_stats$Male.Cases
cancer_stats.txF <- cancer_stats$Female.Deaths/cancer_stats$Female.Cases
cancer_stats.ds <- cancer_stats[cancer_stats$Class == "Digestive System", ]

aux <- subset(cancer_stats.ds, subset = Female.Cases > Male.Cases) 
aux$Site

aux <- subset(cancer_stats, subset = cancer_stats.txM == min(cancer_stats.txM, na.rm = T))
aux$Site

aux <- subset(cancer_stats, subset = cancer_stats.txF == max(cancer_stats.txF, na.rm = T))
aux$Site


