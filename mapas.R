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
df_municipios <- readRDS('df_municipios_etapa5.rds') # Resultados e dados utilizados nas simulacoes
codigos_sp <- readRDS('Dados/codigos_sp.rds') # Relacao nome-codigo dos municipios
br.mapa <- readRDS('Dados/mapa_brasil.rds') # Dados geograficos
legendas <- readRDS('legendas.rds')

# Filtrando base de dados geograficos: apenas SP 
br.mapa$UF = substr(br.mapa$CD_GEOCMU,1,2)
sp.mapa = br.mapa[br.mapa$UF %in% c("35"), ]

# Mudando variavel de codigo do municipio para formato numerico
sp.mapa$CD_GEOCMU = as.numeric(sp.mapa$CD_GEOCMU)

# Merge com os dados das simulacoes, para o ano de 2018
sp.mapa = merge(sp.mapa, df_municipios[df_municipios$ano_distribuicao %in% c('2018'),], by.x = c('CD_GEOCMU'), by.y = c('Codmun7'))


#### DESENHANDO O MAPA ####

# Modo estático
tmap_mode("plot")

# Modo interativo (nao usar no loop!)
# tmap_mode("view")

# Indices definindo o intervalo de colunas das quais queremos fazer mapas
primeira_coluna <- which(names(sp.mapa@data) == 'var_perc_transf10_evolucao')
ultima_coluna <- which(names(sp.mapa@data) == 'var_perc_transf25_nota_X_matriculas_publica')

# Criando e salvando mapas
for (i in primeira_coluna:ultima_coluna) {
  
  var_name <- names(sp.mapa@data)[i]
  sp.mapa$`Variação (%)` <- sp.mapa[[var_name]] * 100
  
  title <- legendas$legenda[legendas$variavel == var_name]
  
  breaks <- get_breaks_pp(sp.mapa$`Variação (%)`)
  palette <- get_palette(breaks)
  
  mapa_variacao <- tm_shape(sp.mapa) + 
    tm_style("beaver", legend.format = list(text.separator = " a ")) +
    tm_fill("Variação (%)",
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
    tm_borders(col = "grey90", lwd = 0.5)
  
  tmap_save(mapa_variacao, str_c('Resultados2/Mapas/', var_name, '.png'))
  
}
