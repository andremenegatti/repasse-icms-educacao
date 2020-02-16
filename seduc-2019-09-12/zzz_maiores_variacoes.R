library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

# Funcao para salvar tabelas de resultados
source("salvar_tabela.R")

#### ABRINDO BASES ####
df_municipios <- readRDS('df_municipios_20190912.rds')

# receitas_correntes_2018 <- readxl::read_excel('Dados/receitas_correntes_municipios_sp.xlsx', sheet = 1) %>% 
#   mutate(valor = valor / 1e+6) %>% 
#   select(Codmun7, receitas_correntes_2018 = valor)

# DF com variacao total de cada simulacao, em formato long
df1 <- df_municipios %>% 
  select(municipio,
         var_transf_alt3, var_transf_alt4) %>% 
  gather(-municipio, key = 'simulacao', value = var_transf_total) %>% 
  mutate(simulacao = str_replace(string = simulacao, pattern = 'var_transf', replacement = 'sim'))

# DF com variacao percentual (em relacao a cota-parte) e outras variaveis
df2 <- df_municipios %>% 
  select(municipio,
         pop,
         fracao_pop_abaixo_15,
         receitas_correntes_2018,
         dados_imputados,
         var_perc_transf_alt3, var_perc_transf_alt4
  ) %>% 
  gather(-municipio, -pop, -fracao_pop_abaixo_15, -receitas_correntes_2018, -dados_imputados, key = 'simulacao', value = 'var_transf_perc_cota2018') %>% 
  mutate(simulacao = str_replace(simulacao, 'var_perc_transf_', 'sim_'))

# Juntando DFs
df_variacoes <- df1 %>% 
  left_join(df2, by = c('municipio', 'simulacao'))

# Criando nova variavel
df_variacoes <- df_variacoes %>% 
  mutate(var_transf_perc_rc2018 = var_transf_total / receitas_correntes_2018) %>% 
  select(municipio, simulacao, dados_imputados, var_transf_total, var_transf_perc_cota2018, var_transf_perc_rc2018, pop, receitas_correntes_2018)

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

temp_maiores_variacoes_perc_rc2018 <- df_variacoes %>% 
  filter(!is.na(var_transf_perc_rc2018))

begin <- nrow(temp_maiores_variacoes_perc_rc2018) / length(unique(temp_maiores_variacoes_perc_rc2018$simulacao)) - 9
end <- nrow(temp_maiores_variacoes_perc_rc2018) / length(unique(temp_maiores_variacoes_perc_rc2018$simulacao))

maiores_variacoes_perc_rc2018 <- temp_maiores_variacoes_perc_rc2018 %>% 
  group_by(simulacao) %>% 
  arrange(var_transf_perc_rc2018) %>% 
  slice(1:10, begin:end) %>% 
  ungroup()


maiores_variacoes_valor_absoluto <- df_variacoes %>% 
  group_by(simulacao) %>% 
  arrange(var_transf_total) %>% 
  slice(1:10, 636:645) %>% 
  ungroup()

# Salvando
df_variacoes %>% 
  mutate_all(as.character) %>% 
  mutate_all(str_replace, pattern = '\\.', replacement = ',') %>% 
  salvar_tabela('Proposta Seduc/Resultados/df_variacoes.csv')

maiores_variacoes_perc_cota2018 %>% 
  mutate_all(as.character) %>% 
  mutate_all(str_replace, pattern = '\\.', replacement = ',') %>% 
  salvar_tabela('Proposta Seduc/Resultados/maiores_variacoes_perc_cota2018.csv')

maiores_variacoes_perc_rc2018 %>% 
  mutate_all(as.character) %>% 
  mutate_all(str_replace, pattern = '\\.', replacement = ',') %>% 
  salvar_tabela('Proposta Seduc/Resultados/maiores_variacoes_perc_rc2018.csv')

maiores_variacoes_valor_absoluto %>% 
  mutate_all(as.character) %>% 
  mutate_all(str_replace, pattern = '\\.', replacement = ',') %>% 
  salvar_tabela('Proposta Seduc/Resultados/maiores_variacoes_valor_absoluto.csv')



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
