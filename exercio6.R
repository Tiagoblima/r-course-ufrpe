head(USArrests)

USArrests

prisoes <- function(estados, tiposPrisoes){
  
  if(any(is.na(USArrests[estados,])==T)){return("Estado Inválido")}
  if(any(any(tiposPrisoes %in% colnames(USArrests)==F))){return("Tipo de prinsão Inválida")}
  number <- sum(apply(USArrests[estados,tiposPrisoes],1, FUN=sum))
  return(paste("O total de prisoes dos estado de", estados, "é de", number))
}
print(any(c("Rape","Murder") %in% colnames(USArrests)==F))
prisoes(estados="Tennessee", tiposPrisoes=c("Rape","Murder")) 
prisoes(estados=c("California ","Miami", "Arizona"), tiposPrisoes=("Assault"))
prisoes(estados=c("Pennsylvania","Mississippi", "Nebraska"), tiposPrisoes=c("Rape","UrbanPop","Assault"))

prisoes(estados=c("Vermont","Wisconsin", "Texas"), tiposPrisoes=c("Rape","Assalto"))



minhasNotas_1 <-function(Exe_1=0, VA_1=0, Exe2=0, Proj=0, VA_2=0, VA_3=0, Opt=1, threshold=7){
  e1 <- mean(Exe_1, na.rm = T)
  nota_va1 <- (((e1*5) + (VA_1*5))/10)
  
  e2 <- mean(Exe2, na.rm = T)
  nota_va2 <- (((e2*2) + (VA_2*3) +(Proj*5))/10)
  
  notas = c(nota_va1, nota_va2, VA_3)
  max_1 = max(notas)
  max_2 <- max(notas[notas!=max_1])
  nota_final <- mean(c(max_1, max_2))
 
  if(Opt==1){
    
    if(nota_va1>threshold){
      msg <- "Acima da Média"
    }else if(nota_va1==threshold){
      msg <- "Na média"
    }else{
      msg <- "Abaixo da Média"
    }
    return(paste("Média da 1VA: ", nota_va1, "-", msg))
   
  }
  else if(Opt==2){
    
    if(nota_va2>threshold){
      msg <- "Acima da Média"
    }else if(nota_va2==threshold){
      msg <- "Na média"
    }else{
      msg <- "Abaixo da Média"
    }
    return(paste("Média da 2VA: ", nota_va2, "-", msg))
 
  }
  else if(Opt==3){
    
    if(VA_3>threshold){
      msg <- "Acima da Média"
    }else if(VA_3==threshold){
      msg <- "Na média"
    }else{
      msg <- "Abaixo da Média"
    }
    return(paste("Média da 3VA: ", VA_3, "-", msg))
  
  }
  else if(Opt==4){
    if (nota_final >= threshold){
       msg <- " -- Aprovado"
    }else{
      msg <- "-- Reprovado"
    }
    return(paste("Média Final: ", nota_final, "-", msg))
  }
  
}

minhasNotas_1(Exe_1=c(10,9,7,5,10,4,5,6,7,8,10), VA_1=8, Exe2=c(6,8,9,8,5,7,8), Proj=7, VA_2=7, VA_3=9, Opt=4, threshold=8)