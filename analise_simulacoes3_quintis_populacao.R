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

# Dentre os 20% que mais se beneficiaram, 68,9% pertencem ao grupo dos 20% com menor populacao
# e nenhum pertence aos dois quintis (40%) com maior populacao
cross_tab_quintiles(var1 = quintil_ben_var_perc_sim_nota, var2 = quintil_pop) %>% 
  filter(quintil_ben_var_perc_sim_nota == '1')

# No entanto, dentre todos os que foram prejudicados, 83,1% pertencem aos grupo dos 20% mais populosos
# e menos de 7% pertecem aos tres quintis inferiores de população (60% menos populosos)
df_municipios %>% 
  filter(var_perc_sim_nota < 0) %>% 
  group_by(quintil_pop) %>% 
  tally() %>% 
  mutate(share = n / df_municipios %>% filter(var_perc_sim_nota < 0) %>% nrow() * 100)

# Dentre os 20% que mais se beneficiaram, o aumento de transferencia per capita médio é de 0.3416729 %
(df_municipios %>% 
    filter(quintil_ben_var_perc_sim_nota == '1') %>% 
    mutate(var_transf_per_capita = var_sim_nota / pop * 100))$var_transf_per_capita %>% mean(na.rm = TRUE)

# Dentre todos os beneficiados, o aumento de transferencia per capita médio é de 0.1277535 %
(df_municipios %>% 
    filter(var_perc_sim_nota >= 0) %>% 
    mutate(var_transf_per_capita = var_sim_nota / pop * 100))$var_transf_per_capita %>% mean(na.rm = TRUE)

# Dentre todos os prejudicados, a variação média da transferencia per capita é de -0.008092347 %
(df_municipios %>% 
    filter(var_perc_sim_nota < 0) %>% 
    mutate(var_transf_per_capita = var_sim_nota / pop * 100))$var_transf_per_capita %>% mean(na.rm = TRUE)

### Ou seja: os municipios menos populosos sao beneficiados e os mais populosos sao prejudicados
# mas a variacao absoluta das transferencias, em termos per capita, é 15.78695 vezes maior para
# os municípios beneficiados (menos populosos), em comparacao com os prejudicados (mais populosos), em média


#### EVOLUCAO IDEB ####

# Dentre os 20% que mais se beneficiaram, 61,5% pertencem ao grupo dos 20% menos populosos
# (e 94,2% pertencem ao grupo dos 40% menos populsos);
# mas nenhum pertence ao grupo dos 40% mais populosos
cross_tab_quintiles(var1 = quintil_ben_var_perc_sim_evolucao, var2 = quintil_pop) %>% 
  filter(quintil_ben_var_perc_sim_evolucao == '1')

# No entanto, dentre todos os que foram prejudicados, 82,4% pertencem aos grupo dos 20% mais populosos;
# Mas apenas 4 (3,2%) pertencem ao grupo dos 20% menos populosos
# e apenas 4,8% pertencem aos 3 quintis inferiores de população
df_municipios %>% 
  filter(var_perc_sim_evolucao < 0) %>% 
  group_by(quintil_pop) %>% 
  tally() %>% 
  mutate(share = n / df_municipios %>% filter(var_perc_sim_evolucao < 0) %>% nrow() * 100)

# Dentre os 20% que mais se beneficiaram, o aumento de transferencia per capita médio é de 0.2993331 %
(df_municipios %>% 
    filter(quintil_ben_var_perc_sim_evolucao == '1') %>% 
    mutate(var_transf_per_capita = var_sim_evolucao / pop * 100))$var_transf_per_capita %>% mean(na.rm = TRUE)

# Dentre todos os beneficiados, o aumento de transferencia per capita médio é de 0.1234353 %
(df_municipios %>% 
    filter(var_perc_sim_evolucao >= 0) %>% 
    mutate(var_transf_per_capita = var_sim_evolucao / pop * 100))$var_transf_per_capita %>% mean(na.rm = TRUE)

# Dentre todos os prejudicados, a variação percentual da transferencia per capita é de -0.007467722 %, em média
(df_municipios %>% 
    filter(var_perc_sim_evolucao < 0) %>% 
    mutate(var_transf_per_capita = var_sim_evolucao / pop * 100))$var_transf_per_capita %>% mean(na.rm = TRUE)

### Ou seja: os municipios menos populosos sao beneficiados e os mais populosos sao prejudicados
# mas a variacao absoluta das transferencias, em termos per capita, é 16.52918 vezes maior para
# os municípios beneficiados (menos populosos), em comparacao com os prejudicados (mais populosos), em média


#### NOTA IDEB PONDERADA PELO NUMERO DE MATRICULAS ####

# Dentre os 20% que mais se beneficiaram, 56,8% e 32,4% pertencem ao primeiro e segundo quintis
# mais populosos, respectivamente;
# Mas nenhum pertence ao grupo dos 20% com menor população
cross_tab_quintiles(var1 = quintil_ben_var_perc_sim_nota_X_matriculas, var2 = quintil_pop) %>% 
  filter(quintil_ben_var_perc_sim_nota_X_matriculas == '1')

# No entanto, dentre todos os que foram prejudicados,
# apenas 6,52% e 15,2% ao primeiro e segundo quintis com maior população, respectivamente;
# mas 55,3% pertencem aos dois quintis inferiores (40% com menor população)
df_municipios %>% 
  filter(var_perc_sim_nota_X_matriculas < 0) %>% 
  group_by(quintil_pop) %>% 
  tally() %>% 
  mutate(share = n / df_municipios %>% filter(var_perc_sim_nota_X_matriculas < 0) %>% nrow() * 100)

# Dentre os 20% que mais se beneficiaram, o aumento de transferencia per capita médio é de 0.01065566 %
(df_municipios %>% 
    filter(quintil_ben_var_perc_sim_nota_X_matriculas == '1') %>% 
    mutate(var_transf_per_capita = var_sim_nota_X_matriculas / pop * 100))$var_transf_per_capita %>% mean(na.rm = TRUE)

# Dentre todos os beneficiados, o aumento da transferencia per capita médio é de 0.00608752 %
(df_municipios %>% 
    filter(var_perc_sim_nota_X_matriculas >= 0) %>% 
    mutate(var_transf_per_capita = var_sim_nota_X_matriculas / pop * 100))$var_transf_per_capita %>% mean(na.rm = TRUE)

# Dentre todos os prejudicados, a variação percentual da transferencia per capita é de -0.02743136 %, em média
(df_municipios %>% 
    filter(var_perc_sim_nota_X_matriculas < 0) %>% 
    mutate(var_transf_per_capita = var_sim_nota_X_matriculas / pop * 100))$var_transf_per_capita %>% mean(na.rm = TRUE)

### Ou seja: os municipios populosos sao beneficiados e os pequenos sao prejudicados.
# Mas, em termos per capita, a variacao absoluta das transferencias dos prejudicados (menos populosos)
# é 4.506163 vezes superior à dos beneficiados (populosos). 


#### NOTA IDEB PONDERADA PELO LOG DO NUMERO DE MATRICULAS ####

# Dentre os 20% que mais se beneficiaram, 57,3% pertencem ao ultimo quintil de população e 86.4% pertencem aos dois últimos quintis;
# Mas nenhum pertencem aos dois primeiros quintis mais populosos.
cross_tab_quintiles(var1 = quintil_ben_var_perc_sim_nota_X_log_matriculas, var2 = quintil_pop) %>% 
  filter(quintil_ben_var_perc_sim_nota_X_log_matriculas == '1')

# Dentre todos os que foram prejudicados, 77,9% pertencem ao primeiro quintil de população (20% mais populosos)
# mas apenas 2 municípios (2.10% dos prejudicados) pertencem aos dois últimos quintis
df_municipios %>% 
  filter(var_perc_sim_nota_X_log_matriculas < 0) %>% 
  group_by(quintil_pop) %>% 
  tally() %>% 
  mutate(share = n / df_municipios %>% filter(var_perc_sim_nota_X_log_matriculas < 0) %>% nrow() * 100)

# Dentre os 20% que mais se beneficiaram, o aumento de transferencia per capita médio é de 0.2282623 %
(df_municipios %>% 
    filter(quintil_ben_var_perc_sim_nota_X_log_matriculas == '1') %>% 
    mutate(var_transf_per_capita = var_sim_nota_X_log_matriculas / pop * 100))$var_transf_per_capita %>% mean(na.rm = TRUE)

# Dentre todos os beneficiados, o aumento da transferencia per capita médio é de 0.0934346 %
(df_municipios %>% 
    filter(var_perc_sim_nota_X_log_matriculas >= 0) %>% 
    mutate(var_transf_per_capita = var_sim_nota_X_log_matriculas / pop * 100))$var_transf_per_capita %>% mean(na.rm = TRUE)

# Dentre todos os prejudicados, a variação percentual da transferencia per capita é de -0.00802621 %, em média
(df_municipios %>% 
    filter(var_perc_sim_nota_X_log_matriculas < 0) %>% 
    mutate(var_transf_per_capita = var_sim_nota_X_log_matriculas / pop * 100))$var_transf_per_capita %>% mean(na.rm = TRUE)

### Ou seja: os municipios menos populosos sao beneficiados e os mais populosos sao prejudicados
# mas a variacao absoluta das transferencias, em termos per capita, é 11.64119 vezes maior para
# os municípios beneficiados (menos populosos), em comparacao com os prejudicados (mais populosos), em média