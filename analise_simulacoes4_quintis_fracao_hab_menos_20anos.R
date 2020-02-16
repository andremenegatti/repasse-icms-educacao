library(tidyverse)

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

#### ABRINDO BASES ####
df_municipios <- readRDS('df_municipios_etapa5.rds')

#### FUNÇAO UTIL ####
cross_tab_quintiles <- function(df = df_municipios, var1, var2) {
  
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  
  df %>% 
    select(!!var1, !!var2) %>% 
    nest(-!!var1) %>% 
    arrange(!!var1) %>% 
    mutate(obs_and_share = map(.x = data,
                               .f = ~ .x %>% 
                                 group_by(!!var2) %>% 
                                 summarise(n = n(), share_perc = n / nrow(.x) * 100)
    )
    ) %>% 
    select(-data) %>% 
    unnest()
  
}

#### NOTA IDEB ####

# Dentre os 20% que mais se beneficiaram, 56,6% pertencem aos 40% com MENOR proporcao de habitantes abaixo de 20 anos
# e 33,1% pertencem aos dois quintis superiores
cross_tab_quintiles(var1 = quintil_ben_var_perc_sim_nota, var2 = quintil_fracao_pop_abaixo_20) %>% 
  filter(quintil_ben_var_perc_sim_nota == '1')

# No entanto, dentre todos os que foram prejudicados, 45.8% pertencem aos grupo dos 40% com MAIOR proporcao de habitantes abaixo de 20 anos
# mas apenas 33% pertencem aos dois quintis inferiores
df_municipios %>% 
  filter(var_perc_sim_nota < 0) %>% 
  group_by(quintil_fracao_pop_abaixo_20) %>% 
  tally() %>% 
  mutate(share = n / df_municipios %>% filter(var_perc_sim_nota < 0) %>% nrow() * 100)

# Dentre os 20% que mais se beneficiaram, o aumento de transferencia por habitante com menos de 20 anos é de 0.07551103 %
(df_municipios %>% 
    filter(quintil_ben_var_perc_sim_nota == '1') %>% 
    mutate(var_transf_por_hab_menos_20anos = var_sim_nota / pop * fracao_pop_abaixo_20))$var_transf_por_hab_menos_20anos %>% mean(na.rm = TRUE)

# Dentre todos os beneficiados, o aumento de transferencia por habitante com menos de 20 anos é de 0.0288139 %
(df_municipios %>% 
    filter(var_perc_sim_nota >= 0) %>% 
    mutate(var_transf_por_hab_menos_20anos = var_sim_nota / pop * fracao_pop_abaixo_20))$var_transf_por_hab_menos_20anos %>% mean(na.rm = TRUE)

# Dentre todos os prejudicados, a variação média da transferência por habitante com menos de 20 anos é de -0.001958913
(df_municipios %>% 
    filter(var_perc_sim_nota < 0) %>% 
    mutate(var_transf_por_hab_menos_20anos = var_sim_nota / pop * fracao_pop_abaixo_20))$var_transf_por_hab_menos_20anos %>% mean(na.rm = TRUE)

### Ou seja: os municipios com maior fracao de jovens parecem ser mais prejudicados do que aqueles com menor fracao de jovens, que tendem a ser beneficiados


#### EVOLUCAO IDEB ####

# Dentre os 20% que mais se beneficiaram, 45.2% pertencem aos 40% com MENOR proporcao de habitantes abaixo de 20 anos
# e 40.4% pertencem aos dois quintis superiores - ou seja: nao parece haver uma relacao muito clara
cross_tab_quintiles(var1 = quintil_ben_var_perc_sim_evolucao, var2 = quintil_fracao_pop_abaixo_20) %>% 
  filter(quintil_ben_var_perc_sim_evolucao == '1')

# Dentre todos os que foram prejudicados, 36.8% pertencem aos grupo dos 40% com MAIOR proporcao de habitantes abaixo de 20 anos
# e apenas 41.6% pertencem aos dois quintis inferiores - nao parece haver uma relacao muito clara
df_municipios %>% 
  filter(var_perc_sim_evolucao < 0) %>% 
  group_by(quintil_fracao_pop_abaixo_20) %>% 
  tally() %>% 
  mutate(share = n / df_municipios %>% filter(var_perc_sim_evolucao < 0) %>% nrow() * 100)

# Dentre os 20% que mais se beneficiaram, o aumento de transferencia por habitante com menos de 20 anos é de 0.06804416 %
(df_municipios %>% 
    filter(quintil_ben_var_perc_sim_evolucao == '1') %>% 
    mutate(var_transf_por_hab_menos_20anos = var_sim_evolucao / pop * fracao_pop_abaixo_20))$var_transf_por_hab_menos_20anos %>% mean(na.rm = TRUE)

# Dentre todos os beneficiados, o aumento de transferencia por habitante com menos de 20 anos é de 0.02839092 %
(df_municipios %>% 
    filter(var_perc_sim_evolucao >= 0) %>% 
    mutate(var_transf_por_hab_menos_20anos = var_sim_evolucao / pop * fracao_pop_abaixo_20))$var_transf_por_hab_menos_20anos %>% mean(na.rm = TRUE)

# Dentre todos os prejudicados, a variação percentual da transferencia por habitante com menos de 20 anos é de -0.001726824 %, em média
(df_municipios %>% 
    filter(var_perc_sim_evolucao < 0) %>% 
    mutate(var_transf_por_hab_menos_20anos = var_sim_evolucao / pop * fracao_pop_abaixo_20))$var_transf_por_hab_menos_20anos %>% mean(na.rm = TRUE)

### Ou seja: nesse caso ha poucas evidencias em favor de diferencas de variacao entre municipios com maior ou menor proporcao de jovens


#### NOTA IDEB PONDERADA PELO NUMERO DE MATRICULAS ####

# Dentre os 20% que mais se beneficiaram, 32,4% e 43.2% pertencem ao primeiro e segundo quintis
# com maior proporcao de habitantes abaixo de 20 anos;
# Mas apenas 2.7% e 8.11% pertencem ao quarto e quinto quintis com menor proporcao de jovens
# Parece que municipios com maior proporcao de jovens sao mais frequentes entre os beneficiados
cross_tab_quintiles(var1 = quintil_ben_var_perc_sim_nota_X_matriculas, var2 = quintil_fracao_pop_abaixo_20) %>% 
  filter(quintil_ben_var_perc_sim_nota_X_matriculas == '1')

# No entanto, dentre todos os que foram prejudicados, nao ha muita diferenca:
# 37.4% dos prejudicados pertencem aos 40% com maior proporcao de jovens
# e 42.2% pertencem aos 40% com menor proporcao de jovens
df_municipios %>% 
  filter(var_perc_sim_nota_X_matriculas < 0) %>% 
  group_by(quintil_fracao_pop_abaixo_20) %>% 
  tally() %>% 
  mutate(share = n / df_municipios %>% filter(var_perc_sim_nota_X_matriculas < 0) %>% nrow() * 100)

# Dentre os 20% que mais se beneficiaram, o aumento de transferencia por habitante com menos de 20 anos é de 0.002648026 %
(df_municipios %>% 
    filter(quintil_ben_var_perc_sim_nota_X_matriculas == '1') %>% 
    mutate(var_transf_por_hab_menos_20anos = var_sim_nota_X_matriculas / pop * fracao_pop_abaixo_20))$var_transf_por_hab_menos_20anos %>% mean(na.rm = TRUE)

# Dentre todos os beneficiados, o aumento da transferencia por hab. com menos de 20 anos é, em média, 0.001463522 %
(df_municipios %>% 
    filter(var_perc_sim_nota_X_matriculas >= 0) %>% 
    mutate(var_transf_por_hab_menos_20anos = var_sim_nota_X_matriculas / pop * fracao_pop_abaixo_20))$var_transf_por_hab_menos_20anos %>% mean(na.rm = TRUE)

# Dentre todos os prejudicados, a variação percentual da transferencia por hab. com menos de 20 anos é de -0.006365323 %, em média
(df_municipios %>% 
    filter(var_perc_sim_nota_X_matriculas < 0) %>% 
    mutate(var_transf_por_hab_menos_20anos = var_sim_nota_X_matriculas / pop * fracao_pop_abaixo_20))$var_transf_por_hab_menos_20anos %>% mean(na.rm = TRUE)

### Ou seja: parece que, dentre os beneficiados, existem uma proporcao maior de municipios 'mais jovens' (com maior proporcao de hab. abaixo de 20 anos)
# Mas nao ha evidencias de que existam mais municípios 'mais velhos' dentre os prejudicados


#### NOTA IDEB PONDERADA PELO LOG DO NUMERO DE MATRICULAS ####

# Dentre os 20% que mais se beneficiaram, 12.7 % pertencem ao primeiro quintil e 35.5% pertence ao último quintil;
# Ou seja: proporcionalmente, parece haver mais municipios 'velhos' entre os beneficiados (i.e., com menor proporcao de hab. abaixo de 20 anos)
cross_tab_quintiles(var1 = quintil_ben_var_perc_sim_nota_X_log_matriculas, var2 = quintil_fracao_pop_abaixo_20) %>% 
  filter(quintil_ben_var_perc_sim_nota_X_log_matriculas == '1')

# Dentre todos os que foram prejudicados, 28.4% pertencem ao primeiro quintil (20% com maior fracao de hab. abaixo de 20 anos)
# mas apenas 12.6% pertencem ao último quintil
df_municipios %>% 
  filter(var_perc_sim_nota_X_log_matriculas < 0) %>% 
  group_by(quintil_fracao_pop_abaixo_20) %>% 
  tally() %>% 
  mutate(share = n / df_municipios %>% filter(var_perc_sim_nota_X_log_matriculas < 0) %>% nrow() * 100)

# Dentre os 20% que mais se beneficiaram, o aumento de transferencia por habitante com menos de 20 anos é de 0.05079787 %, em média
(df_municipios %>% 
    filter(quintil_ben_var_perc_sim_nota_X_log_matriculas == '1') %>% 
    mutate(var_transf_por_hab_menos_20anos = var_sim_nota_X_log_matriculas / pop * fracao_pop_abaixo_20))$var_transf_por_hab_menos_20anos %>% mean(na.rm = TRUE)

# Dentre todos os beneficiados, o aumento de transferencia por habitante com menos de 20 anos é de  0.02122781 %, em média
(df_municipios %>% 
    filter(var_perc_sim_nota_X_log_matriculas >= 0) %>% 
    mutate(var_transf_por_hab_menos_20anos = var_sim_nota_X_log_matriculas / pop * fracao_pop_abaixo_20))$var_transf_por_hab_menos_20anos %>% mean(na.rm = TRUE)

# Dentre todos os prejudicados, a variação percentual da transferencia por hab. com menos de 20 anos é de 0.001948916 %, em média
(df_municipios %>% 
    filter(var_perc_sim_nota_X_log_matriculas < 0) %>% 
    mutate(var_transf_por_hab_menos_20anos = var_sim_nota_X_log_matriculas / pop * fracao_pop_abaixo_20))$var_transf_por_hab_menos_20anos %>% mean(na.rm = TRUE)

### Ou seja: os municipios com mais jovens sao mais frequentes entre os prejudicados e os municipios com menos jovens sao mais frequentes entre os beneficiados