library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

source("salvar_tabela.R")

# Abrindo base
df_municipios <- readRDS('2019_08_14_simulacao_secretaria/df_municipios_etapa5.rds')

# Montante disponível, em milhões de reais
df_municipios$montante_disponivel %>% mean()


df_municipios <- df_municipios %>% 
  mutate(total_var_transf_positiva = sum(if_else(var_transf_alt1 >= 0, var_transf_alt1, 0)),
         total_var_transf_negativa = sum(if_else(var_transf_alt1 < 0, var_transf_alt1, 0)))


df2 <- df_municipios %>% 
  filter(var_transf_alt1 < 0) %>% 
  mutate(dummy_reg_metrop = if_else(is.na(regiao_metrop), FALSE, TRUE)) %>% 
  group_by(dummy_reg_metrop) %>% 
  summarise(total_var_transf_negativa = mean(total_var_transf_negativa),
            contribuicao_reg_metrop = sum(var_transf_alt1),
            contrib_perc_reg_metrop = contribuicao_reg_metrop / total_var_transf_negativa) %>% 
  ungroup() %>% 
  filter(dummy_reg_metrop) %>% 
  select(-dummy_reg_metrop)

# df2 %>%
#   mutate_all(as.character) %>%
#   mutate_all(str_replace, pattern = '\\.', replacement = ',') %>%
#   salvar_tabela("2019_08_14_simulacao_secretaria/contribuição_regiões_metropolitanas.csv")

df3 <- df_municipios %>% 
  filter(var_transf_alt1 < 0,
         regiao_metrop == 'São Paulo') %>% 
  summarise(total_var_transf_negativa = mean(total_var_transf_negativa),
            contribuicao_reg_met_sp = sum(var_transf_alt1),
            contrib_perc_reg_met_sp = contribuicao_reg_met_sp / total_var_transf_negativa)

# df3 %>%
#     mutate_all(as.character) %>%
#     mutate_all(str_replace, pattern = '\\.', replacement = ',') %>%
#     salvar_tabela("2019_08_14_simulacao_secretaria/contribuição_reg_met_sp.csv")

df4 <- df_municipios %>% 
  filter(var_transf_alt1 < 0,
         municipio == 'SAO PAULO') %>% 
  summarise(total_var_transf_negativa = mean(total_var_transf_negativa),
            contribuicao_sp = sum(var_transf_alt1),
            contrib_perc_sp = contribuicao_sp / total_var_transf_negativa)

# df4 %>%
#     mutate_all(as.character) %>%
#     mutate_all(str_replace, pattern = '\\.', replacement = ',') %>%
#     salvar_tabela("2019_08_14_simulacao_secretaria/contribuição_sp.csv")

