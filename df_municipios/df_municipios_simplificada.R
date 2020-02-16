library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

# Abrindo base
df_municipios_full <- readRDS('Dados/df_municipios_etapa5.rds')

# Selecionando variaveis de interesse
df_municipios <- df_municipios_full %>% 
  select(municipio:evolucao_e_nota,
         nota_ideb_2015, nota_ideb_2017,
         evolucao_e_nota_X_matriculas_municipal,
         share_nota, share_evolucao_e_nota, share_evolucao_e_nota_X_matriculas_municipal,
         ind25_nota, ind25_evolucao_e_nota, ind25_evolucao_e_nota_X_matriculas_municipal,
         ind10_nota, ind10_evolucao_e_nota, ind10_evolucao_e_nota_X_matriculas_municipal,
         transf25_nota, transf25_evolucao_e_nota, transf25_evolucao_e_nota_X_matriculas_municipal,
         transf10_nota, transf10_evolucao_e_nota, transf10_evolucao_e_nota_X_matriculas_municipal,
         var_transf25_nota, var_transf25_evolucao_e_nota, var_transf25_evolucao_e_nota_X_matriculas_municipal,
         var_transf10_nota, var_transf10_evolucao_e_nota, var_transf10_evolucao_e_nota_X_matriculas_municipal,
         var_perc_transf25_nota, var_perc_transf25_evolucao_e_nota, var_perc_transf25_evolucao_e_nota_X_matriculas_municipal,
         var_perc_transf10_nota, var_perc_transf10_evolucao_e_nota, var_perc_transf10_evolucao_e_nota_X_matriculas_municipal,
         receitas_correntes_2018:pop_urbana,
         idosos_sobre_jovens:receitas2017_pc,
         var_perc_rc2018_25_nota, var_perc_rc2018_25_evolucao_e_nota, var_perc_rc2018_25_evolucao_e_nota_X_matriculas_municipal,
         var_perc_rc2018_10_nota, var_perc_rc2018_10_evolucao_e_nota, var_perc_rc2018_10_evolucao_e_nota_X_matriculas_municipal,         
         quintil_area_geografica:quintil_receitas2018_pc,
         quintil_var_perc_transf25_nota, quintil_var_perc_transf25_evolucao_e_nota, quintil_var_perc_transf25_evolucao_e_nota_X_matriculas_municipal,
         quintil_var_perc_transf10_nota, quintil_var_perc_transf10_evolucao_e_nota, quintil_var_perc_transf10_evolucao_e_nota_X_matriculas_municipal,
         quintil_ben_var_perc_transf25_nota, quintil_ben_var_perc_transf25_evolucao_e_nota, quintil_ben_var_perc_transf25_evolucao_e_nota_X_matriculas_municipal,
         quintil_ben_var_perc_transf10_nota, quintil_ben_var_perc_transf10_evolucao_e_nota, quintil_ben_var_perc_transf10_evolucao_e_nota_X_matriculas_municipal,
         quintil_prej_var_perc_transf25_nota, quintil_prej_var_perc_transf25_evolucao_e_nota, quintil_prej_var_perc_transf25_evolucao_e_nota_X_matriculas_municipal,
         quintil_prej_var_perc_transf10_nota, quintil_prej_var_perc_transf10_evolucao_e_nota, quintil_prej_var_perc_transf10_evolucao_e_nota_X_matriculas_municipal
         )

# Variavel indicando faixa de populacao
df_municipios <- df_municipios %>% 
  mutate(faixa_pop = case_when(pop <= 20e+3 ~ 'Até 20 mil',
                               pop > 20e+3 & pop <= 50e+3 ~ '20 mil a 50 mil',
                               pop > 50e+3 & pop <= 100e+3 ~ '50 mil a 100 mil',
                               pop > 100e+3 & pop <= 150e+3 ~ '100 mil a 150 mil',
                               pop > 150e+3 & pop <= 250e+3 ~ '150 mil a 250 mil',
                               pop > 250e+3 & pop <= 500e+3 ~ '250 mil a 500 mil',
                               pop > 500e+3 ~ 'Mais de 500 mil'))

# Dummy indicando imputacao de nota
df_municipios <- df_municipios %>%
  mutate(nota_imputada = if_else(is.na(nota_ideb_2015), TRUE, FALSE))

# saveRDS(df_municipios, 'Dados/df_municipios_simplificada.rds')
# salvar_tabela(df_municipios, 'Dados/df_municipios_simplificada.csv')
