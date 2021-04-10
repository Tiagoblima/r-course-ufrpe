install.packages("dplyr")
library(dplyr)
risco.survey <- data.frame(
  "ID.part1" =c(1,2,3,4,5),
  "av.risco"=c(3,4,5,3,1)
)

felicidade.survey <- data.frame(
  "ID.part2"=c(4,2,5,1,3,30),
  "feli.score"=c(20,40,50,90,53, 43)
)

merged_df <- merge(risco.survey, felicidade.survey, by.x="ID.part1", by.y = "ID.part2", all=T)

View(merged_df)

#Qual foi a mpedia de temparatura em NY para os mese do estudo descrito no dataset airquality

aggregate (
  formula = Temp ~ Month,
  FUN = mean,
  data = airquality,
  subset = Wind < 10
)

##############################################################

#Qual o comando seleciona apenas a coluna Dose de df ? Ao submeter a resposta remova todos os espaços em branco. 

df<-data.frame(Theoph)


df %>% select(Dose)

#Qual o comando apresenta os dados para as doses maiores que 5 mg/kg ? Ao submeter a resposta remova todos os espaços em branco. 

filter(df, Dose > 5)

slice(df, c(10:20))


filter(df,Dose>5,Time>mean(df$Time))

arrange(df,Wt,desc(Time))

df <- mutate(df,tendencia=df$Time-mean(df$Time))

summarise(df,max(Dose))


#########################################################################3


ontime <- as.data.frame(
  read.csv(
    'https://www.dropbox.com/s/gi59a1nq3ga9gb7/673598238_T_ONTIME_REPORTING.csv?dl=1',quote="\"", sep = "," 
  )
)

unique <- as.data.frame(
  read.csv(
    'https://www.dropbox.com/s/73bp8dl8nph6ufz/L_UNIQUE_CARRIERS.csv_?dl=1',quote="\"", sep = "," 
  )
)

head(unique)

head(ontime)

df <- merge(ontime,unique , by.x="OP_UNIQUE_CARRIER", by.y = "Code", all=F)

head(df)

tail(df)

#Qual companhia atrasa mais na média ? 

df %>% group_by(Description) %>% summarise(
  meanDelay = mean(DEP_DELAY_NEW, na.rm = T)
) %>% filter(meanDelay==max(meanDelay))

#Qual companhia atrasa menos na média ?


df %>% group_by(Description) %>% summarise(
  meanDelay = mean(DEP_DELAY_NEW, na.rm = T)
) %>% filter(meanDelay==min(meanDelay))



#Qual companhia teve a maior proporção de atrasos ? 

head(df)

indexes <- which(is.na(df$DEP_DELAY_NEW))

df <- df[-indexes,]

df %>% group_by(Description) %>% summarise(
  propDelay = sum(DEP_DELAY_NEW)/sum(df$DEP_DELAY_NEW)
) %>% filter(propDelay==max(propDelay))



####################################################

install.packages('tidyr')
library(tidyr)

TB <- as.data.frame(
  read.csv(
    'http://stat405.had.co.nz/data/tb.csv'
  )
)
head(TB[,(3:23)])
 
gat <- TB %>% gather(c(3:23), key = "Código",value = "N_casos")

head(gat)

new_TB <- gat %>% separate("Código",c("caso", "tipo", "sexo_faixa"), sep = "_") %>%
  separate("sexo_faixa", c("sexo", "faixa"), sep="(?<=[A-Za-z])(?=[0-9])")


head(new_TB)

unique(new_TB$sexo)

# Qual foi a quantidade de casos para a Tailândia (TH) de pessoas do sexo Masculino?

new_TB$sexo <- replace(as.character(new_TB$sexo), new_TB$sexo == "mu", "m")
new_TB %>% group_by(iso2)  %>% filter(iso2=="TH", sexo=="m") %>% summarise(
  N_casos = sum(N_casos, na.rm = T)
)

#Qual a proporção de casos para os estados unidos (US) ? Não considerar valores NAs da coluna "Valores".
indexes <- which(is.na(new_TB$sexo))

new_TB <- new_TB[-indexes, ]


groupedUS <- new_TB %>% group_by(iso2)  %>% filter(iso2=="US")


sum(groupedUS$N_casos, na.rm = T)/sum(new_TB$N_casos, na.rm = T)
#Qual a quantidade de casos para a faixa etária 2534 do sexo feminino? *
new_TB$sexo <- replace(as.character(new_TB$sexo), new_TB$sexo == "fu", "f")



f_2534 <- new_TB %>% group_by(sexo)  %>% filter(sexo=="f" & faixa==2534 )

sum(f_2534$N_casos, na.rm = T)

#Qual foi a quantidade de casos para a década de 2000 ? A década de 2000, também referida como anos 2000, compreende o período de tempo entre 1 de janeiro de 2000 e 31 de dezembro de 2009. 
dec_2000 <- new_TB  %>% filter(year>=2000 & year<2010 )

sum(dec_2000$N_casos, na.rm = T)










