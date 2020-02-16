library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

# Funcoes importantes para limpar e juntar bases
source("substituir_caracteres_especiais.R")
# limpar_nomes_municipios_dados_demograficos <- function(x) {str_remove(x, '\\(SP\\)') %>% substituir_caracteres_especiais() %>% str_trim(side = 'both') %>% str_replace('BIRITIBA MIRIM', 'BIRITIBA-MIRIM')}
source("limpar_nomes_municipios_dados_demograficos.R")

#### ABRINDO BASES ####
df_municipios <- readRDS('2019_08_14_simulacao_secretaria/df_municipios_etapa2.rds')

# Receita corrente
receitas_correntes_2018 <- readxl::read_excel('Dados/receitas_correntes_municipios_sp.xlsx', sheet = 1) %>% 
  mutate(valor = valor / 1e+6) %>% 
  select(Codmun7, receitas_correntes_2018 = valor)
receitas_correntes_2017 <- readxl::read_excel('Dados/receitas_correntes_municipios_sp.xlsx', sheet = 2) %>% 
  mutate(valor = valor / 1e+6) %>% 
  select(Codmun7, receitas_correntes_2017 = valor)

# IDH
IDH_2010 <- readxl::read_excel('Dados/IDH Municipal_AtlasBrasil.xlsx', sheet = 1, range = 'A3:F647',
                               col_names = c('Codmun7', 'Espacialidades', 'IDH', 'IDHM_renda', 'IDHM_longevidade', 'IDHM_educacao')) %>% 
  select(-Espacialidades)

# Area geografica
area_geografica_2018 <- readxl::read_excel('Dados/Dados Geográficos_SP.xlsx', sheet = 4, range = 'B3:H648') %>% 
  select(Codmun7 = Codigo, area_geografica = `2018`)

# Regioes metropolitanas
regioes_metropolitanas <- readxl::read_excel('Dados/Dados Geográficos_SP.xlsx', sheet = 2, range = 'A2:B169', col_names = c('regiao_metrop', 'municipio')) %>% 
  mutate(municipio = substituir_caracteres_especiais(municipio),
         regiao_metrop = str_remove(regiao_metrop, 'Região Metropolitana d. '))

# Populacao urbana (percentual do total)
pop_urbana_2010 <- readxl::read_excel('Dados/Dados Demográficos_SP.xlsx', sheet = 2, range = 'A4:D648', col_names = c('municipio', 'ano', 'total', 'pop_urbana')) %>% 
  select(-ano, -total) %>% mutate(municipio = limpar_nomes_municipios_dados_demograficos(municipio))

# Populacao por faixas etarias
pop_faixa_etaria <- readxl::read_excel('Dados/Dados Demográficos_SP.xlsx', sheet = 4, range = 'A5:L649',
                                       col_names = c('municipio', 'total', str_c('faixa_',  c('5_a_9', '10_a_14', '15_a_19', '20_a_24', '25_a_29', '30_a_39', '40_a_49', '50_a_59', '60_a_69', '70_ou_mais')))) %>% 
  select(-total) %>% mutate(municipio = limpar_nomes_municipios_dados_demograficos(municipio))


#### JUNTANDO BASES ####
df_municipios <- df_municipios %>% 
  left_join(receitas_correntes_2018, by = 'Codmun7') %>% 
  left_join(receitas_correntes_2017, by = 'Codmun7') %>% 
  left_join(IDH_2010, by = 'Codmun7') %>% 
  left_join(area_geografica_2018, by = 'Codmun7') %>% 
  left_join(regioes_metropolitanas, by = 'municipio') %>% 
  left_join(pop_urbana_2010, by = 'municipio') %>% 
  left_join(pop_faixa_etaria, by = 'municipio')

#### SALVANDO BASES EM FORMATO RDS ####
# saveRDS(IDH_2010, 'IDH_2010.rds')
# saveRDS(area_geografica_2018, 'area_geografica_2018.rds')
# saveRDS(regioes_metropolitanas, 'regioes_metropolitanas.rds')
# saveRDS(pop_urbana_2010, 'pop_urbana_2010.rds')
# saveRDS(pop_faixa_etaria, 'pop_faixa_etaria.rds')
saveRDS(df_municipios, '2019_08_14_simulacao_secretaria/df_municipios_etapa3.rds')

# write.table(df_municipios, file = 'df_municipios.csv', sep = ';', dec = ',', col.names = TRUE, row.names = FALSE, quote = FALSE)
