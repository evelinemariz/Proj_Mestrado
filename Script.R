rm(list=ls())
cat("\014")
library("readr")
library("tidyverse")
dados <- read_delim("MICRODADOS_2018.txt", ";", escape_double = FALSE, 
                      locale = locale(encoding = "ISO-8859-1"), 
                             trim_ws = TRUE)
dados <- dados[c(-2,-3,-5,-12:-15,-19,-21:-82,-87:-90,-95:-98,-100:-103)]
#dados3 <- dados2 %>% select(-2,-3,-5,-12:-80)
#dados4 <- dados3 %>% select(-23:-32)
#dados <- dados4 %>% select(-9:-14,-29:-55)
#rm(dados2,dados3,dados4)
View(dados)
dados <- dados[!is.na(dados$NU_NOTA_CN),]
dados$nt_final <- (rowSums(dados[,c(17,18, 19, 20, 28)]))/5
summary(dados$nt_final)
dados <- dados[!is.na(dados$nt_final),]



nt_cat <- matrix(ncol = 1, nrow = length(dados$nt_final))
for (i in 1:length(dados$nt_final)) {
  
  nt_cat[i,] <- ifelse(dados$nt_final[i,]<317.58,0,dados$nt_final[i,])
  nt_cat[i,] <- ifelse(dados$nt_final[i,]>=317.58 & dados$nt_final[i,]<423.06, 1, dados$nt_final[i,])
  
  nt_cat[i,] <- ifelse(dados$nt_final[i,]>=423.06 & dados$nt_final[i,]<528.55, 2, dados$nt_final[i,])
  nt_cat[i,] <- ifelse(dados$nt_final[i,]>=528.55 & dados$nt_final[i,]<634.03, 3, dados$nt_final[i,])
  nt_cat[i,] <- ifelse(dados$nt_final[i,]>=634.03 & dados$nt_final[i,]<739.52, 4, dados$nt_final[i,])
  nt_cat[i,] <- ifelse(dados$nt_final[i,]>=739.52 & dados$nt_final[i,]<845, 5, dados$nt_final[i,])
  
  
  
    
}

nt_cat[i,] <- if(dados$nt_final<317.58){0} else {
  if(dados$nt_final>=317.58 & dados$nt_final<423.06){1} else {
    if(dados$nt_final>=423.06 & dados$nt_final<528.55){2} else {
      if(dados$nt_final>=528.55 & dados$nt_final<634.03){3} else {
        if(dados$nt_final>=634.03 & dados$nt_final<739.52){4} else {
          5  
        }  
      }  
    }  
  }
}



dados$nt_cat <- ifelse(dados$nt_final<317.58,0,dados$nt_final)
dados$nt_cat <- ifelse(dados$nt_final>=317.58 & dados$nt_final<423.06, 1, dados$nt_final)
dados$nt_cat <- ifelse(dados$nt_final>=423.06 & dados$nt_final<528.55, 2, dados$nt_final)
dados$nt_cat <- ifelse(dados$nt_final>=528.55 & dados$nt_final<634.03, 3, dados$nt_final)
dados$nt_cat <- ifelse(dados$nt_final>=634.03 & dados$nt_final<739.52, 4, dados$nt_final)
dados$nt_cat <- ifelse(dados$nt_final>=739.52 & dados$nt_final<845, 5, dados$nt_final)

head(dados$nt_cat)
write.table(dados,"dados.csv",col.names = TRUE)
library(data.table)
dados <- fread("dados.csv")
