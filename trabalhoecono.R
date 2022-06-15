library(stargazer)
library(writexl)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(sandwich)
library(car)

link <- "https://raw.githubusercontent.com/girardimateus/trabalhoecono/main/basefinal.csv"
base <- read_csv(url(link),show_col_types = FALSE)

#Criando as dummies de restrição política
base$pclow <- ifelse(base$polcon < 3,1,0)
base$pchigh <- ifelse(base$polcon > 5,1,0)
base$pcmed <- ifelse(base$pclow == 0 & base$pchigh == 0,1,0)

#Criano a dummies de renda do pais
base$rendabaixa <- ifelse(base$pib <1026, 1, 0)
base$rendaalta <- ifelse(base$pib >12375, 1, 0)
base$rendamedia <- ifelse(base$rendabaixa == 0 & base$rendaalta == 0,1,0)

#############################################################################

#Modelo simples
regsimples <- lm(inflacao ~ cbi_paises, base)
summary(regsimples)
ersimples <- vcovHC(regsimples, type = "HC")
robust_simples <- sqrt(diag(ersimples))

#Modelo controlando por gasto
reg1 <- lm(inflacao ~ cbi_paises + gasto, base)
summary(reg1)
er1 <- vcovHC(reg1, type = "HC")
robust_1 <- sqrt(diag(er1))

#Modelo controlando por gasto e variação do pib
reg2 <- lm(inflacao ~ cbi_paises + gasto + pibvar, base)
summary(reg2)
er2 <- vcovHC(reg2, type = "HC")
robust_2 <- sqrt(diag(er2))

#Modelo controlando por gasto, variação do pib e nivel de renda do país
reg3 <- lm(inflacao ~ cbi_paises + gasto + pibvar + rendabaixa + rendaalta + rendamedia, base)
summary(reg3)
er3 <- vcovHC(reg3, type = "HC")
robust_3 <- sqrt(diag(er3))

#Modelo controlando por gasto, variação do pib, nivel de renda do país e nivel de political constrain
reg4 <- lm(inflacao ~ cbi_paises + gasto + pibvar + rendabaixa + rendaalta + rendamedia + pclow + pchigh + pcmed, base)
summary(reg4)
er4 <- vcovHC(reg4, type = "HC")
robust_4 <- sqrt(diag(er4))

#Resultados - corrigidos para heterocedasticidade
stargazer(regsimples, reg1, reg2, reg3, reg4, se=list(robust_simples,robust_1,robust_2,robust_3,robust_4),
          title="Resultados",
          align=T,
          type = "text")


#Estatisticas descritivas
stargazer(base, type = "text", summary.stat = c("n","mean","median","sd","max","min"))

#Estatísticas descritivas por renda
base_rb <- filter(base, rendabaixa == 1)
base_rm <- filter(base, rendamedia == 1)
base_ra <- filter(base, rendaalta == 1)

stargazer(base_rb, type = "text")
stargazer(base_rm, type = "text")
stargazer(base_ra, type = "text")

#Estatísticas descritivas por restrição
base_pcb <- filter(base, pclow == 1)
base_pcm <- filter(base, pcmed == 1)
base_pca <- filter(base, pchigh == 1)

stargazer(base_pcb, type = "text")
stargazer(base_pcm, type = "text")
stargazer(base_pca, type = "text")


#Regressão simples - Gráfico
regsimples_grafico <- ggplot(data = base) + geom_point(aes(x=cbi_paises, y=inflacao), color = "#1F1B1B", size = 1, shape = 20) + ylim(0,15)+ 
  geom_smooth(aes(x=cbi_paises, y=inflacao), color = "#9C1C2D", size = 0.5, method = "lm", se=FALSE) +
  labs(x = 'Independência do Banco Central',y = 'Inflação') +  theme_few() 
regsimples_grafico

#Gráfico CBI
cbi_grafico <- ggplot(data = base) + geom_density(aes(x=cbi_paises), color = "#1F1B1B", size = 1, shape = 20) + ylim(0,2.5) +
  labs(x = 'Independência do Banco Central',y = 'Inflação') +  theme_few() 
cbi_grafico


#Apendice
base_arredondada <- base %>% mutate(across(where(is.numeric), round, 3))
stargazer(base_arredondada[1:100,], summary=FALSE, rownames=FALSE, type = "latex", digits = 2)

