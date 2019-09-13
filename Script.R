rm(list=ls())
cat("\014")
library("readr")
library("tidyverse")
dados2 <- read_delim("MICRODADOS_100.txt", ";", escape_double = FALSE, 
                      locale = locale(encoding = "ISO-8859-1"), 
                             trim_ws = TRUE)
#dados <- dados[c(-2,-3,-5,-12:-123)]
dados3 <- dados2 %>% select(-2,-3,-5,-12:-80)
dados4 <- dados3 %>% select(-23:-32)
dados <- dados4 %>% select(-9:-14,-29:-55)
rm(dados2,dados3,dados4)
View(dados)
