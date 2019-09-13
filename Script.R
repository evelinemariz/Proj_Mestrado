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
#var(dados[,17],na.rm = TRUE)
View(dados)

