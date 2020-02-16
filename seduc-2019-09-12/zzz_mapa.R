library(RColorBrewer)
library(tidyverse)
library(rgdal)
library(tmap)
library(sp)

# Mudando diretorio
setwd("C:/Users/Dell/Desktop/Estudo ICMS")

# Funcao util
source("get_breaks_pp.R")
source("get_palette.R")

# Desativando conversao de strings em fatores
options(stringsAsFactors = F)

#### PREPARANDO DADOS ####

# Carregando bases de dados
# df_municipios <- readRDS('2019_08_14_simulacao_secretaria/df_municipios_etapa5.rds') # Resultados e dados utilizados nas simulacoes
codigos_sp <- readRDS('Dados/codigos_sp.rds') # Relacao nome-codigo dos municipios
br.mapa <- readRDS('Dados/mapa_brasil.rds') # Dados geograficos
legendas <- readRDS('Dados/legendas.rds')

# Mudando variavel indicativa de imputacao
df_municipios <- df_municipios %>% 
  mutate(Dados_imputados = ifelse(dados_imputados, 'Sim', NA))

# Filtrando base de dados geograficos: apenas SP 
br.mapa$UF = substr(br.mapa$CD_GEOCMU,1,2)
sp.mapa = br.mapa[br.mapa$UF %in% c("35"), ]

# Mudando variavel de codigo do municipio para formato numerico
sp.mapa$CD_GEOCMU = as.numeric(sp.mapa$CD_GEOCMU)

# Merge com os dados das simulacoes, para o ano de 2018
sp.mapa = merge(sp.mapa, df_municipios[df_municipios$ano_distribuicao %in% c('2018'),], by.x = c('CD_GEOCMU'), by.y = c('Codmun7'))

#### DESENHANDO O MAPA ####

# Modo estático
# tmap_mode("plot")

# Modo interativo (nao usar no loop!)
# tmap_mode("view")

sp.mapa$`Variacao (%)` <- sp.mapa[["var_perc_transf_alt1"]] * 100

title <- 'V.A. (75%), Educ. (18,5%), RT (6%), Area Protegida (0,5%)'

breaks <- get_breaks_pp(sp.mapa$`Variacao (%)`)
palette <- get_palette(breaks)

mapa_variacao <- tm_shape(sp.mapa) + 
  tm_style("beaver", legend.format = list(text.separator = " a ")) +
  tm_fill("Variacao (%)",
          palette = palette,
          style = "fixed",
          breaks = breaks,
          alpha = 1,
          id = "municipio") +
  tm_layout(main.title.size = 1.5, 
            scale = 1.1, 
            bg.color = "white",
            inner.margins = c(.1, .1, .1, .1),
            main.title = title) +
  tm_compass(north = 0, type = "8star", size = 2, position = c("right", "bottom")) +
  tm_scale_bar(size = 0.6, text.color = NA, lwd = 1,
               color.dark = "black", color.light = "white") +
  tm_legend(legend.position = c(0.01,0.08)) +
  # tm_dots("Dados_imputados", size = 0.1, col = "green") +
  tm_borders(col = "grey90", lwd = 0.5)

tmap_save(mapa_variacao, 'mapa_var_perc_alt1.png')
