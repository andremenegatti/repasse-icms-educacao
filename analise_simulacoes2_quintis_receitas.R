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

# Dentre os 20% que mais se beneficiaram, 75,5% pertencem ao grupo dos 20% com menores receitas correntes
# e nenhum pertence ao grupo dos 20% que tem maiores receitas
cross_tab_quintiles(var1 = quintil_ben_var_perc_sim_nota, var2 = quintil_receitas_correntes_2018) %>% 
  filter(quintil_ben_var_perc_sim_nota == '1')

# No entanto, dentre todos os que foram prejudicados, 79,7% pertencem aos grupo dos 20% com maiores receitas correntes
# e apenas 1 (0,847%) pertence ao grupo dos 20% de menores receitas
df_municipios %>% 
  filter(var_perc_sim_nota < 0) %>% 
  group_by(quintil_receitas_correntes_2018) %>% 
  tally() %>% 
  mutate(share = n / df_municipios %>% filter(var_perc_sim_nota < 0) %>% nrow() * 100)

# Dentre os 20% que mais se beneficiaram, o aumento da transferencia representa, em média, 2.25817e-4 % das receitas correntes de 2018
(df_municipios %>% 
  filter(quintil_ben_var_perc_sim_nota == '1') %>% 
  mutate(var_transf_receita_ratio = var_sim_nota / receitas_correntes_2018 * 100))$var_transf_receita_ratio %>% mean(na.rm = TRUE)

# Dentre todos os beneficiados, o aumento da transferencia representa, em média, 7.126863e-05 % das receitas correntes de 2018
(df_municipios %>% 
    filter(var_perc_sim_nota >= 0) %>% 
    mutate(var_transf_receita_ratio = var_sim_nota / receitas_correntes_2018 * 100))$var_transf_receita_ratio %>% mean(na.rm = TRUE)

# Dentre todos os prejudicados, a reducao na transferencia representa, em media, 2.007144e-06 % das receitas correntes de 2018
(df_municipios %>% 
    filter(var_perc_sim_nota < 0) %>% 
    mutate(var_transf_receita_ratio = var_sim_nota / receitas_correntes_2018 * 100))$var_transf_receita_ratio %>% mean(na.rm = TRUE)

### Ou seja: os municipios 'pobres' sao beneficiados e os 'ricos' sao prejudicados
# mas a media da razao variacao_transferencia/receitas_correntes para os beneficiados é cerca de 35,5 vezes
# superior à média dos prejudicados


#### EVOLUCAO IDEB ####

# Dentre os 20% que mais se beneficiaram, 71,2% pertencem ao grupo dos 20% com menores receitas correntes
# e nenhum pertence ao grupo dos 20% que tem maiores receitas
cross_tab_quintiles(var1 = quintil_ben_var_perc_sim_evolucao, var2 = quintil_receitas_correntes_2018) %>% 
  filter(quintil_ben_var_perc_sim_evolucao == '1')

# No entanto, dentre todos os que foram prejudicados, 80% pertencem aos grupo dos 20% com maiores receitas correntes
# e apenas 4 (3,2%) pertencem ao grupo dos 20% de menores receitas
df_municipios %>% 
  filter(var_perc_sim_evolucao < 0) %>% 
  group_by(quintil_receitas_correntes_2018) %>% 
  tally() %>% 
  mutate(share = n / df_municipios %>% filter(var_perc_sim_evolucao < 0) %>% nrow() * 100)

# Dentre os 20% que mais se beneficiaram, o aumento da transferencia representa, em média, 2.254824e-4 % das receitas correntes de 2018
(df_municipios %>% 
    filter(quintil_ben_var_perc_sim_evolucao == '1') %>% 
    mutate(var_transf_receita_ratio = var_sim_evolucao / receitas_correntes_2018 * 100))$var_transf_receita_ratio %>% mean(na.rm = TRUE)

# Dentre todos os beneficiados, o aumento da transferencia representa, em média,  6.979324e-05 % das receitas correntes de 2018
(df_municipios %>% 
    filter(var_perc_sim_evolucao >= 0) %>% 
    mutate(var_transf_receita_ratio = var_sim_evolucao / receitas_correntes_2018 * 100))$var_transf_receita_ratio %>% mean(na.rm = TRUE)

# Dentre todos os prejudicados, a reducao na transferencia representa, em media, 1.729458e-06 % das receitas correntes de 2018
(df_municipios %>% 
    filter(var_perc_sim_evolucao < 0) %>% 
    mutate(var_transf_receita_ratio = var_sim_evolucao / receitas_correntes_2018 * 100))$var_transf_receita_ratio %>% mean(na.rm = TRUE)

### Ou seja: os municipios 'pobres' sao beneficiados e os 'ricos' sao prejudicados
# mas a média da razao variacao_transferencia/receitas_correntes para os beneficiados é cerca de 40 vezes
# superior à média dos prejudicados


#### NOTA IDEB PONDERADA PELO NUMERO DE MATRICULAS ####

# Dentre os 20% que mais se beneficiaram, 54,1% e 35,1% pertencem ao primeiro e segundo quintis
# com maiores receitas correntes em 2018, respectivamente
# e nenhum pertence ao grupo dos 20% que tem as menores receitas
cross_tab_quintiles(var1 = quintil_ben_var_perc_sim_nota_X_matriculas, var2 = quintil_receitas_correntes_2018) %>% 
  filter(quintil_ben_var_perc_sim_nota_X_matriculas == '1')

# No entanto, dentre todos os que foram prejudicados,
# apenas 8,05% pertencem aos 2 quintis superiores de receitas correntes (40% com receitas mais altas)
# mas 54,3% pertencem aos quintis inferiores (40% com receitas mais baixas)
df_municipios %>% 
  filter(var_perc_sim_nota_X_matriculas < 0) %>% 
  group_by(quintil_receitas_correntes_2018) %>% 
  tally() %>% 
  mutate(share = n / df_municipios %>% filter(var_perc_sim_nota_X_matriculas < 0) %>% nrow() * 100)

# Dentre os 20% que mais se beneficiaram, o aumento da transferencia representa, em média, 3.328671e-06 % das receitas correntes de 2018
(df_municipios %>% 
    filter(quintil_ben_var_perc_sim_nota_X_matriculas == '1') %>% 
    mutate(var_transf_receita_ratio = var_sim_nota_X_matriculas / receitas_correntes_2018 * 100))$var_transf_receita_ratio %>% mean(na.rm = TRUE)

# Dentre todos os beneficiados, o aumento da transferencia representa, em média, 1.653906e-06 % das receitas correntes de 2018
(df_municipios %>% 
    filter(var_perc_sim_nota_X_matriculas >= 0) %>% 
    mutate(var_transf_receita_ratio = var_sim_nota_X_matriculas / receitas_correntes_2018 * 100))$var_transf_receita_ratio %>% mean(na.rm = TRUE)

# Dentre todos os prejudicados, a reducao na transferencia representa, em media, -1.147611e-05 % das receitas correntes de 2018
(df_municipios %>% 
    filter(var_perc_sim_nota_X_matriculas < 0) %>% 
    mutate(var_transf_receita_ratio = var_sim_nota_X_matriculas / receitas_correntes_2018 * 100))$var_transf_receita_ratio %>% mean(na.rm = TRUE)

### Ou seja: os municipios 'ricos' sao beneficiados e os 'pobres' sao prejudicados
# No entanto, a media da razao variacao_transferencia/receitas_correntes para os beneficiados (ricos) representa apenas 
# 0.1441173 da razão média dos prejudicados (pobres).
# Dito de outra forma, a média da razão variacao_transferencia/receitas correntes para os prejudicados (pobres) é cerca de
# 6.93 vezes superior à média dessa razão para os beneficiados (ricos)


#### NOTA IDEB PONDERADA PELO LOG DO NUMERO DE MATRICULAS ####

# Dentre os 20% que mais se beneficiaram, 66,4% pertencem ao ultimo quintil de receitas correntes (20% mais 'pobres')
# e nenhum pertence ao primeiro quintil
cross_tab_quintiles(var1 = quintil_ben_var_perc_sim_nota_X_log_matriculas, var2 = quintil_receitas_correntes_2018) %>% 
  filter(quintil_ben_var_perc_sim_nota_X_log_matriculas == '1')

# Dentre todos os que foram prejudicados, 78,9% pertencem ao primeiro quintil de receitas correntes (20% mais 'ricos')
# mas apenas 1,05% e 7,37% pertencem aos dois últimos quintis, respectivamente
df_municipios %>% 
  filter(var_perc_sim_nota_X_log_matriculas < 0) %>% 
  group_by(quintil_receitas_correntes_2018) %>% 
  tally() %>% 
  mutate(share = n / df_municipios %>% filter(var_perc_sim_nota_X_log_matriculas < 0) %>% nrow() * 100)

# Dentre os 20% que mais se beneficiaram, o aumento da transferencia representa, em média, 1.56e-4 % das receitas correntes de 2018
(df_municipios %>% 
    filter(quintil_ben_var_perc_sim_nota_X_log_matriculas == '1') %>% 
    mutate(var_transf_receita_ratio = var_sim_nota_X_log_matriculas / receitas_correntes_2018 * 100))$var_transf_receita_ratio %>% mean(na.rm = TRUE)

# Dentre todos os beneficiados, o aumento da transferencia representa em media 4.959528e-05 % das receitas correntes de 2018
(df_municipios %>% 
    filter(var_perc_sim_nota_X_log_matriculas >= 0) %>% 
    mutate(var_transf_receita_ratio = var_sim_nota_X_log_matriculas / receitas_correntes_2018 * 100))$var_transf_receita_ratio %>% mean(na.rm = TRUE)

# Dentre todos os prejudicados, a reducao na transferencia representa, em media, 1.939115e-06 % das receitas correntes de 2018
(df_municipios %>% 
    filter(var_perc_sim_nota_X_log_matriculas < 0) %>% 
    mutate(var_transf_receita_ratio = var_sim_nota_X_log_matriculas / receitas_correntes_2018 * 100))$var_transf_receita_ratio %>% mean(na.rm = TRUE)

### Ou seja: os municipios 'pobres' sao beneficiados e os 'ricos' sao prejudicados
# mas a razao media variacao_transferencia/receitas_correntes para os beneficiados é cerca de 25 vezes
# superior à média dos prejudicados