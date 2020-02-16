library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

# Funcao para salvar tabelas de resultados
source("salvar_tabela.R")

#### ABRINDO BASES ####
df_municipios <- readRDS('Dados/df_municipios_etapa5.rds')

# DF com variacao total de cada simulacao, em formato long
df1 <- df_municipios %>% 
  select(municipio,
         var_transf10_evolucao:var_transf25_nota_X_matriculas_publica) %>% 
  gather(-municipio, key = 'simulacao', value = var_transf_total) %>% 
  mutate(simulacao = str_replace(string = simulacao, pattern = 'var_transf', replacement = 'sim_'))

# DF com variacao percentual (em relacao a cota-parte) e outras variaveis
df2 <- df_municipios %>% 
  select(municipio,
         pop,
         fracao_pop_abaixo_15,
         receitas_correntes_2018,
         var_perc_transf10_evolucao:var_perc_transf25_nota_X_matriculas_publica
         ) %>% 
  gather(-municipio, -pop, -fracao_pop_abaixo_15, -receitas_correntes_2018, key = 'simulacao', value = 'var_transf_perc_cota2018') %>% 
  mutate(simulacao = str_replace(simulacao, 'var_perc_transf', 'sim_'))

# Juntando DFs
df_variacoes <- df1 %>% 
  left_join(df2, by = c('municipio', 'simulacao'))

# Criando nova variavel
df_variacoes <- df_variacoes %>% 
  mutate(var_transf_perc_rc2018 = var_transf_total / receitas_correntes_2018,
         pop_abaixo_15anos = pop * fracao_pop_abaixo_15 / 100,
         var_transf_por_hab_abaixo_15anos = var_transf_total * 1e+6 / pop_abaixo_15anos) %>% 
  select(municipio, simulacao, var_transf_total, var_transf_perc_cota2018, var_transf_perc_rc2018, var_transf_por_hab_abaixo_15anos, pop, receitas_correntes_2018)

# Selecionando apenas os 10 municipios com maior aumento e os 10 com maior reducao
maiores_variacoes_perc_cota2018 <- df_variacoes %>% 
  group_by(simulacao) %>% 
  arrange(var_transf_perc_cota2018) %>% 
  slice(1:10, 636:645) %>% 
  ungroup()

maiores_variacoes_perc_rc2018 <- df_variacoes %>% 
  group_by(simulacao) %>% 
  filter(!is.na(var_transf_perc_rc2018)) %>% 
  arrange(var_transf_perc_rc2018) %>% 
  slice(1:10, 636:645) %>% 
  ungroup()

maiores_variacoes_perc_por_hab_abaixo_15anos <- df_variacoes %>% 
  group_by(simulacao) %>% 
  arrange(var_transf_por_hab_abaixo_15anos) %>% 
  slice(1:10, 636:645) %>% 
  ungroup()

# Salvando
df_variacoes %>% 
  mutate_all(as.character) %>% 
  mutate_all(str_replace, pattern = '\\.', replacement = ',') %>% 
  salvar_tabela('Resultados2/df_variacoes.csv')

maiores_variacoes_perc_cota2018 %>% 
  mutate_all(as.character) %>% 
  mutate_all(str_replace, pattern = '\\.', replacement = ',') %>% 
  salvar_tabela('Resultados2/maiores_variacoes_perc_cota2018.csv')

maiores_variacoes_perc_rc2018 %>% 
  mutate_all(as.character) %>% 
  mutate_all(str_replace, pattern = '\\.', replacement = ',') %>% 
  salvar_tabela('Resultados2/maiores_variacoes_perc_rc2018.csv')

maiores_variacoes_perc_por_hab_abaixo_15anos %>% 
  mutate_all(as.character) %>% 
  mutate_all(str_replace, pattern = '\\.', replacement = ',') %>% 
  salvar_tabela('Resultados2/maiores_variacoes_perc_por_hab_abaixo_15anos.csv')



#### IDENTIFICANDO MUNICIPIOS COM MAIORES VARIACOES PERCENTUAIS NO TOTAL RECEBIDO EM 2018 ####
source("selecionar_municipios_maior_variacao.R")

municipios_maior_aumento <- list()
municipios_maior_reducao <- list()
var_names <- names(df_municipios %>% select(starts_with('transf10_'), starts_with('transf25_')))

for (i in 1:length(var_names)) {
  var_name <- var_names[i]
  message(var_name)
  df_municipios$temp_var <- df_municipios[[var_name]]
  message(' -- Positiva')
  municipios_maior_aumento[[var_name]] <- selecionar_municipios_maior_variacao(df = df_municipios, valor_inicial = total_recebido_municipio,
                                                                               valor_final = temp_var, sentido_variacao = 'positiva', size = 15)
  message('-- Negativa')
  municipios_maior_reducao[[var_name]] <- selecionar_municipios_maior_variacao(df = df_municipios, valor_inicial = total_recebido_municipio,
                                                                               valor_final = temp_var, sentido_variacao = 'negativa', size = 15)
  df_municipios$temp_var <- NULL
}

# as_tibble(municipios_maior_aumento) %>% salvar_tabela('municipios_maior_aumento.csv')
# as_tibble(municipios_maior_reducao) %>% salvar_tabela('municipios_maior_reducao.csv')
