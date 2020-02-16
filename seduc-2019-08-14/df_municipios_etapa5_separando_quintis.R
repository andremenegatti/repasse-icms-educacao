library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

#### ABRINDO BASES ####
df_municipios <- readRDS('2019_08_14_simulacao_secretaria/df_municipios_etapa4.rds')

#### CRIANDO VARIAVEIS PARA CLASSIFICACAO ####
df_municipios <- df_municipios %>% 
  mutate(idosos_sobre_jovens = (faixa_60_a_69 + faixa_70_ou_mais) / (faixa_5_a_9 + faixa_10_a_14),
         fracao_pop_abaixo_15 = faixa_5_a_9 + faixa_10_a_14,
         receitas2018_pc = receitas_correntes_2018 / pop,
         receitas2017_pc = receitas_correntes_2017 / pop)

# Calculando quanto a variacao representa das receitas correntes liquidas
# df_var_transf_perc_rc <- df_municipios %>%
#   gather(var_transf10_evolucao:var_transf25_nota_X_matriculas_publica, key = 'simulacao', value = 'variacao_total_transf') %>%
#   group_by(municipio, Codmun7, simulacao) %>%
#   summarise(var_transf_perc_rc2018 = variacao_total_transf / receitas_correntes_2018,
#             var_transf_perc_rc2017 = variacao_total_transf / receitas_correntes_2017) %>%
#   ungroup()

# Incluindo percentuais acima na base de municipios
# df_municipios <- df_municipios %>% 
#   left_join(df_var_transf_perc_rc %>% select(Codmun7, simulacao, var_transf_perc_rc2018) %>% mutate(simulacao = str_replace(simulacao, 'var_transf', 'var_perc_rc2018_') ) %>% spread(key = simulacao, value = var_transf_perc_rc2018),
#             by = 'Codmun7') %>% 
#   left_join(df_var_transf_perc_rc %>% select(Codmun7, simulacao, var_transf_perc_rc2017) %>% mutate(simulacao = str_replace(simulacao, 'var_transf', 'var_perc_rc2017_') ) %>% spread(key = simulacao, value = var_transf_perc_rc2017),
#             by = 'Codmun7')

#### FUNCAO PARA DIVIDIR EM QUINTIS ####
source("dividir_em_quintis.R")

#### SEPARANDO EM QUINTIS ####
# Separando municipios em quintis segundo variaveis de classificacao
df_quintis_variaveis_classificacao <- df_municipios %>% 
  select(municipio, pop, pop_urbana, idosos_sobre_jovens, fracao_pop_abaixo_15,
         area_geografica, IDH, area_cultivada, area_inundada, receitas_correntes_2018, receitas_correntes_2018,
         receitas2018_pc, receitas2017_pc) %>% 
  gather(-municipio, key = 'variavel_classificacao', value = 'valor') %>% 
  group_by(variavel_classificacao) %>% 
  mutate(quintil = dividir_em_quintis(valor)) %>% 
  ungroup() %>% 
  mutate(variavel_classificacao = str_c('quintil_', variavel_classificacao)) %>% 
  select(-valor) %>%
  spread(key = variavel_classificacao, value = quintil)

# Separando municipios em quintis segundo variacao percentual nas transferencias
# Aqui, teremos 5 grupos com o mesmo numero de municipios
# Possivelmente um dos grupos tera tanto municipios beneficiados quanto municipios prejudicados
df_quintis_var_perc <- df_municipios %>% 
  select(municipio, starts_with('var_perc_transf')) %>% 
  gather(-municipio, key = 'criterio', value = 'var_perc') %>% 
  group_by(criterio) %>% 
  mutate(grupo = dividir_em_quintis(var_perc)) %>% 
  ungroup() %>% 
  mutate(criterio = str_c('quintil_', criterio)) %>% 
  select(-var_perc) %>% 
  spread(key = criterio, value = grupo)

# Separando municipios em quintis, APOS separacao entre ganhadores e perdedores
# Ou seja, para cada simulacao, teremos 10 grupos: 5 quantis de ganhadores e 5 quantis de perdedores
df_quintis_var_perc_separados <- df_municipios %>% 
  select(municipio, starts_with('var_perc_transf')) %>% 
  gather(-municipio, key = 'criterio', value = 'var_perc') %>% 
  nest(-criterio) %>% 
  mutate(beneficiados = map(.x = data, .f = ~ .x %>% filter(var_perc >= 0) %>% mutate(quintil_ben = dividir_em_quintis(var_perc))),
         prejudicados = map(.x = data, .f = ~ .x %>% filter(var_perc < 0) %>% mutate(quintil_prej = dividir_em_quintis(var_perc)))) %>% 
  mutate(data = map2(.x = beneficiados, .y = prejudicados, .f = ~ bind_rows(.x, .y))) %>% 
  select(-beneficiados, -prejudicados) %>% 
  unnest() %>% 
  select(-var_perc)

# Para incluir esses novos grupos em df_municipios, precisamos mudar o formato de 'long' para 'wide'
# Alem disso, precisamos criar colunas separadas para os quantis dos beneficiados e dos prejudicados
# Os dois blocos a seguir fazem isso
df_quintis_var_perc_beneficiados <- df_quintis_var_perc_separados %>% 
  select(-quintil_prej) %>%
  mutate(criterio = str_c('quintil_ben_', criterio)) %>% 
  spread(key = criterio, value = quintil_ben)

df_quintis_var_perc_prejudicados <- df_quintis_var_perc_separados %>% 
  select(-quintil_ben) %>%
  mutate(criterio = str_c('quintil_prej_', criterio)) %>% 
  spread(key = criterio, value = quintil_prej)

#### INCLUINDO VARIAVEIS DE QUINTIS NA BASE DE MUNICIPIOS ####
df_municipios <- df_municipios %>% 
  left_join(df_quintis_variaveis_classificacao, by = 'municipio') %>% 
  left_join(df_quintis_var_perc, by = 'municipio') %>% 
  left_join(df_quintis_var_perc_beneficiados, by = 'municipio') %>% 
  left_join(df_quintis_var_perc_prejudicados, by = 'municipio')

# Variavel indicando faixa de populacao
df_municipios <- df_municipios %>% 
  mutate(faixa_pop = case_when(pop <= 20e+3 ~ 'Até 20 mil',
                               pop > 20e+3 & pop <= 50e+3 ~ '20 mil a 50 mil',
                               pop > 50e+3 & pop <= 150e+3 ~ '50 mil a 150 mil',
                               pop > 150e+3 & pop <= 250e+3 ~ '150 mil a 250 mil',
                               pop > 250e+3 & pop <= 500e+3 ~ '250 mil a 500 mil',
                               pop > 500e+3 ~ 'Mais de 500 mil'))

# Dummy indicando imputacao de nota
df_municipios <- df_municipios %>%
  mutate(nota_imputada = if_else(is.na(nota_ideb_2015), TRUE, FALSE))

#### SALVANDO ####
saveRDS(df_municipios, '2019_08_14_simulacao_secretaria/df_municipios_etapa5.rds')
source("salvar_tabela.R")

df_municipios %>% 
  mutate_all(as.character) %>% 
  mutate_all(str_replace, pattern = '\\.', replacement = ',') %>% 
  salvar_tabela(file = "2019_08_14_simulacao_secretaria/df_municipios_etapa5.csv")
