rm(list=ls())
cat("\014")
library("readr")
library("tidyverse")
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
dados <- read_delim("MICRODADOS_2018.txt", ";", escape_double = FALSE,
                    locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE)



dados <- dados[c(-2,-3,-5,-12:-15,-19,-21:-82,-87:-90,-95:-98,-100:-103)]
View(dados)
dados <- dados[!is.na(dados$NU_NOTA_CN),]
dados$nt_final <- (rowSums(dados[,c(17,18, 19, 20, 28)]))/5
dados <- dados[!is.na(dados$nt_final),]
summary(dados$nt_final)
summary(dados$TP_ST_CONCLUSAO)
table(dados$TP_ST_CONCLUSAO)
table(dados$Q001)
table(dados$Q002)
pie(table(dados$Q003))
pie(table(dados$Q004))
table(dados$Q005)
table(dados$Q006)
summary(dados$NU_IDADE)
table(dados$TP_SEXO)
pie(table(dados$TP_SEXO))
dados$nt_cat <- cut(dados$nt_final, 
                    breaks=c(-Inf, 317.58, 423.06, 528.55, 634.03, 739.52, 845), 
                    labels=c("0","1","2", "3", "4", "5"))
table(dados$nt_cat)
hist(dados$NU_IDADE)
barplot(prop.table(table(dados$TP_SEXO)))
pie(prop.table(table(dados$TP_SEXO)))
with(dados, table(dados$TP_SEXO, dados$nt_cat))

#MÉDIA DE IDADE POR ESTADO CIVIL E POR SEXO
dados2 <- dados[!is.na(dados$TP_ESTADO_CIVIL),]
dados3 <- dados2[!is.na(dados2$NU_IDADE),]
with(dados3, do.call(rbind, tapply(dados3$NU_IDADE, dados3$TP_ESTADO_CIVIL, function(x) c(Media = mean(x), SD = sd(x)))))

with(dados3, do.call(rbind, tapply(dados3$NU_IDADE, dados3$TP_SEXO, function(x) c(Media = mean(x), SD = sd(x)))))

#Numero de municipios que possuem candidatos no ENEM
length(unique(dados$NO_MUNICIPIO_RESIDENCIA))
#Numero de municipios por estado que possuem candidatos no ENEM
Mun_Est <- dados %>% 
  group_by(SG_UF_RESIDENCIA) %>% 
  summarise(Contagem = length(unique(NO_MUNICIPIO_RESIDENCIA)))

#Categorias de idade
summary(dados$NU_IDADE)
Cat_Idade <- cut(dados$NU_IDADE, 
                 breaks=c(0, 13, 20, 30, 40, 50, 100), 
                 labels=c("13","20","30", "40", "50", ">50"))
table(Cat_Idade)
barplot(table(Cat_Idade))

#Cor/Raca
table(dados$TP_COR_RACA)
barplot(table(dados$TP_COR_RACA))

#Nacionalidade
table(dados$TP_NACIONALIDADE)
barplot(table(dados$TP_NACIONALIDADE))


tb <- table(dados$nt_cat)
names(tb)<- c("317.58", "423.06", "528.55", "634.03", "739.52", "845")
bp <- barplot(tb, beside = T, las = 1, xlab = 'Discriminação das Notas',
              ylab = 'Frequência', ylim = c(0, 40000), main="Frequência das Notas")
text(x = c(bp), y = c(tb), labels = tb, pos = 3)
title(sub="Figura x")

bp=barplot(table(dados$TP_SEXO,dados$nt_cat), args.legend = list(x = "topleft",bty = "n"), ylim=c(0, 25000), beside=T, leg=c("Feminino", "Masculino" ),
           ylab="Quantidade", xlab="Categorias de Notas",names.arg = c("0", "1", "2", "3", "4", "5"), main="Quantidade de candidatos em cada \n categoria discriminados por sexo")
text(bp, table(dados$TP_SEXO,dados$nt_cat)+800, table(dados$TP_SEXO,dados$nt_cat))
title(sub="Figura x")


library("rlang")

aggSetor <-dados%>%group_by(SG_UF_RESIDENCIA) %>% summarise(quantidade = n(), 
                                                            notaMedia = mean(nt_final,na.rm = TRUE))

aggSetor$escala <- scale(aggSetor$notaMedia) #necessário para criar valores negativos para deixar as disparidades mais evidentes
library("treemap")
x <- treemap(aggSetor, index = "SG_UF_RESIDENCIA", vSize = "quantidade", vColor = "escala",
             type = "value", palette = "-RdGy", lowerbound.cex.labels = 0.3,
             title  =  "Treemap da Média das Notas do Enem por Estado")

library("ggrepel")
df <- dados %>%
  group_by(SG_UF_RESIDENCIA) %>% subset(!is.na(SG_UF_RESIDENCIA)) %>%
  summarise(notaMedia = round(mean(nt_final,na.rm = TRUE),2),
            numeroDeCandidatos = n()) 

ggplot(df,aes(numeroDeCandidatos, notaMedia)) +
  geom_point() +
  geom_text_repel(aes(label = SG_UF_RESIDENCIA)) +
  geom_vline(xintercept = median(df$numeroDeCandidatos)) +
  geom_hline(yintercept = median(dados$nt_final)) +
  labs(title = "Nota média e número de candidatos por estado (2018)", x = "Número de Candidatos", y = "Nota média")

# library("forcats")
# library("ggridges")
# library("gridExtra")
# library("ggridges")

theme_set(theme_classic())
# Plot
#Densidade da distribuição da nota dos alunos por Estado
df <- dados %>% select(SG_UF_RESIDENCIA, nt_final)
g <- ggplot(df, aes(nt_final))
g + geom_density(aes(fill=factor(SG_UF_RESIDENCIA)), alpha=0.8)

#Densidade da distribuição da nota comparando dois ou mais Estados
df2 <- df %>% filter( SG_UF_RESIDENCIA %in% c('MG', 'AC'))
g <- ggplot(df2, aes(nt_final))
g + geom_density(aes(fill=factor(SG_UF_RESIDENCIA)), alpha=0.8)

#Somente um Estado
dfMG <- dados %>% subset(SG_UF_RESIDENCIA=="MG")

ggplot(dfMG, aes(nt_final))+
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="",
       caption="Source: Banco de dados do ENEM",
       x="Nota dos alunos de MG",
       fill="# Nota Média")


#write.csv(dados,"dados.csv")


pie(table(dados$TP_SEXO))

#REALIZANDO ANÁLISES DA LOGÍSTICA
with(dados, table(nt_cat, Q006))
with(dados, table(nt_cat, Q027))
with(dados, table(nt_cat, TP_COR_RACA))
with(dados, table(nt_cat, TP_SEXO))
#with(dados, do.call(rbind, tapply(Q027, nt_cat, function(x) c(M = mean(x), SD = sd(x)))))

#REALIZANDO OS TESTES DE CADA MODELO
dados$nt_cat2 <- relevel(dados$nt_cat, ref = "0")

test1234 <- multinom(nt_cat2 ~ Q006 + Q027 + TP_COR_RACA + TP_SEXO, data = dados)
summary(test1234)
z1234 <- summary(test1234)$coefficients/summary(test1234)$standard.errors
z1234

# 2-tailed z test1234
p1234 <- (1 - pnorm(abs(z1234), 0, 1)) * 2
p1234

## extract the coefficients from the model and exponentiate
exp(coef(test1234))

head(pp1234 <- fitted(test1234))


test123456 <- multinom(nt_cat2 ~ Q006 + Q027 + TP_COR_RACA + TP_SEXO + NU_IDADE + TP_ESTADO_CIVIL, data = dados)
summary(test123456)
z123456 <- summary(test123456)$coefficients/summary(test123456)$standard.errors
z123456

# 2-tailed z test1234
p123456 <- (1 - pnorm(abs(z123456), 0, 1)) * 2
p123456

## extract the coefficients from the model and exponentiate
exp(coef(test123456))

head(pp123456 <- fitted(test123456))
