library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

source("salvar_tabela.R")

# Abrindo base
df_municipios <- readRDS('Dados/df_municipios_simplificada.rds')

# Montante disponível, em milhões de reais
df_municipios$montante_disponivel %>% mean()

df_municipios %>% 
  select(total_recebido_municipio,
         transf25_nota, transf25_evolucao_e_nota, transf25_evolucao_e_nota_X_matriculas_municipal,
         transf10_nota, transf10_evolucao_e_nota, transf10_evolucao_e_nota_X_matriculas_municipal) %>% 
  as.matrix() %>% 
  apply(FUN = sum, MARGIN = 2)


df_var_transf_gathered <- df_municipios %>% 
  gather(var_transf25_nota:var_transf10_evolucao_e_nota_X_matriculas_municipal, key = 'simulacao', value = 'var_transf') %>% 
  group_by(simulacao) %>% 
  mutate(total_var_transf_positiva = sum(if_else(var_transf >= 0, var_transf, 0)),
            total_var_transf_negativa = sum(if_else(var_transf < 0, var_transf, 0))) %>% 
  ungroup()

  
df1 <- df_var_transf_gathered %>% 
  group_by(simulacao) %>% 
  summarise(total_var_transf_positiva = sum(if_else(var_transf >= 0, var_transf, 0)),
            total_var_transf_negativa = sum(if_else(var_transf < 0, var_transf, 0)))


df2 <- df_var_transf_gathered %>% 
  filter(var_transf < 0) %>% 
  mutate(dummy_reg_metrop = if_else(is.na(regiao_metrop), FALSE, TRUE)) %>% 
  group_by(simulacao, dummy_reg_metrop) %>% 
  summarise(total_var_transf_negativa = mean(total_var_transf_negativa),
            contribuicao_reg_metrop = sum(var_transf),
            contrib_perc_reg_metrop = contribuicao_reg_metrop / total_var_transf_negativa) %>% 
  ungroup() %>% 
  filter(dummy_reg_metrop) %>% 
  select(-dummy_reg_metrop)

# df2 %>% 
#   mutate_all(as.character) %>% 
#   mutate_all(str_replace, pattern = '\\.', replacement = ',') %>% 
#   salvar_tabela("contribuição_regiões_metropolitanas.csv")

df3 <- df_var_transf_gathered %>% 
  filter(var_transf < 0,
         regiao_metrop == 'São Paulo') %>% 
  group_by(simulacao) %>% 
  summarise(total_var_transf_negativa = mean(total_var_transf_negativa),
            contribuicao_reg_met_sp = sum(var_transf),
            contrib_perc_reg_met_sp = contribuicao_reg_met_sp / total_var_transf_negativa)
  
# df3 %>%
#     mutate_all(as.character) %>%
#     mutate_all(str_replace, pattern = '\\.', replacement = ',') %>%
#     salvar_tabela("contribuição_reg_met_sp.csv")

df4 <- df_var_transf_gathered %>% 
  filter(var_transf < 0,
         municipio == 'SAO PAULO') %>% 
  group_by(simulacao) %>% 
  summarise(total_var_transf_negativa = mean(total_var_transf_negativa),
            contribuicao_sp = sum(var_transf),
            contrib_perc_sp = contribuicao_sp / total_var_transf_negativa)

# df4 %>% 
#     mutate_all(as.character) %>%
#     mutate_all(str_replace, pattern = '\\.', replacement = ',') %>%
#     salvar_tabela("contribuição_sp.csv")

