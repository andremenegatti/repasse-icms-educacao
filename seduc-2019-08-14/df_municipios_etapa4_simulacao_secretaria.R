library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

# Abrindo base
df_municipios <- readRDS('2019_08_14_simulacao_secretaria/df_municipios_etapa3.rds')

# Sera necessario imputar dados de RCL 2017 para MARABA PAULISTA (~4,8 mil habitantes)

# Analisando receitas de municipios pequenos para imputacao
df_peq_mun <- df_municipios %>% 
  filter(pop <= 5000,
         ano_calculo == '2017') %>% 
  mutate(faixa_peq_mun = case_when(pop >= 0 & pop < 3000 ~ '[0,3mil)',
                                   pop >= 3000 & pop < 4000 ~ '[3mil, 4mil)',
                                   pop >= 4000 & pop <= 5000 ~ '[4mil, 5mil]')) %>% 
  group_by(faixa_peq_mun) %>% 
  summarise(n = n(),
            rcl = mean(receitas_correntes_2017, na.rm = TRUE))

# Ha 50 municipios na faixa de 4mil-5mil hab (numero razoavel)
# Alem disso, a media da faixa anterior (3mil-4mil) nao parece muito diferente
df_peq_mun

# IMPUTANDO RCL 2017 PARA MARABA PAULISTA: MEDIA DOS MUNICIPIOS ENTRE 4mil e 5mil HABITANTES
df_municipios <- df_municipios %>% 
  mutate(receitas_correntes_2017 = if_else(is.na(receitas_correntes_2017), df_peq_mun$rcl[3], receitas_correntes_2017))

# Funcao simples para calcular share
source("calcular_share.R")

# Calculando share RCL 2017
df_municipios <- df_municipios %>% 
  group_by(ano_calculo) %>% 
  mutate(share_rcl_2017 = calcular_share(receitas_correntes_2017)) %>% 
  ungroup()

# Filtro de ano: ano_calculo 2016, para simular distribuicao de 2018
# Obs: dados do IDEB de 2015 e dados de RCL de 2017
df_municipios <- df_municipios %>% filter(ano_calculo == '2016')

# Funcao para calcular transferencia a partir do indice
calcular_transferencia <- function(indice_participacao) {df_municipios$montante_disponivel * indice_participacao / 100}

# Simulacao utilizando a proposta da Secretaria: 18,5% para educação
df_municipios <- df_municipios %>% 
  # Calculando novo indice de participacao
  mutate(ind_secretaria = df_municipios$share_valor_adicionado_2anos * 75 + share_evolucao_e_nota_X_matriculas_municipal * 18.5 + share_area_inundada * 0.5 + share_rcl_2017 * 6) %>% 
  # Calculando transferencia
  mutate(transf_secretaria = calcular_transferencia(ind_secretaria)) %>% 
  # Calculando diferenca bruta e diferenca percentual (em relacao a cota-parte de 2018)
  mutate(var_transf_secretaria = transf_secretaria - total_recebido_municipio,
         var_perc_transf_secretaria = var_transf_secretaria / total_recebido_municipio) %>% 
  # Calculando quanto a variacao na transferencia representa da RCL 2017
  mutate(var_transf_perc_rc2017_secretaria = var_transf_secretaria / receitas_correntes_2017)

saveRDS(df_municipios, '2019_08_14_simulacao_secretaria/df_municipios_etapa4.rds')
