library(tidyverse)
library(lubridate)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

#### ABRINDO BASES ####
df_municipios <- readRDS('Dados/df_municipios_etapa2.rds')

#### CRIANDO VARIAVEIS COM NOTAS DO IDEB EM 2015 E 2017 ####
df_notas_2015 <- df_municipios %>% 
  filter(ano_calculo == '2015') %>% 
  select(municipio, nota_ideb_2015 = nota_ideb)

df_notas_2017 <- df_municipios %>% 
  filter(ano_calculo == '2017') %>% 
  select(municipio, nota_ideb_2017 = nota_ideb)

df_municipios <- df_municipios %>% 
  left_join(df_notas_2015, by = 'municipio') %>% 
  left_join(df_notas_2017, by = 'municipio')

#### FILTRO DE ANO ####
df_municipios <- df_municipios %>%
  filter(ano_calculo == '2016')

# Analisando a distribuicao dos novos shares na amostra restrita
## Distribuicoes parecem ok; notar o formato exponencial das variaveis que incluem matricula (sem log)
# df_municipios %>%
#   select(municipio, starts_with("share_")) %>%
#   gather(-municipio, key = "variavel", value = "share") %>%
#   filter(variavel %in% names(df_municipios)[45:59]) %>%
#   mutate(variavel = str_remove(variavel, 'share_')) %>%
#   filter(municipio != "SAO PAULO") %>%
#   ggplot(mapping = aes(x = share, fill = variavel)) +
#   geom_histogram(alpha = 0.5, position = 'identity') +
#   facet_wrap(~ variavel, scales = 'free')
# 
# df_municipios %>%
#   select(starts_with('share_')) %>%
#   select(contains('evolucao'), contains('nota')) %>%
#   summary()


#### SIMULACOES COM INDICADORES DE QUALIDADE DA EDUCACAO ####

# Funcao para calcular a distribuicao com nova variavel ocupando os 25% 'disponiveis'
# recalcular_indice25 <- function(share_new_var) {df_municipios$share_valor_adicionado_2anos * 75 + share_new_var * 25}
# recalcular_indice10 <- function(share_new_var) {75 * df_municipios$share_valor_adicionado_2anos + 12.5 * df_municipios$share_pop + 2.5 * df_municipios$share_receita_tributaria + 10 * share_new_var}
source("recalcular_indice10.R")
source("recalcular_indice25.R")

# Calculando indices de participacao sob regimes alternativos

# 25%
df_municipios <- df_municipios %>% 
  mutate(ind25_nota = recalcular_indice25(share_nota),
         ind25_nota_X_matriculas_publica = recalcular_indice25(share_nota_X_matriculas_publica),
         ind25_nota_X_log_matriculas_publica = recalcular_indice25(share_nota_X_log_matriculas_publica),
         ind25_evolucao = recalcular_indice25(share_evolucao),
         ind25_evolucao_X_matriculas_publica = recalcular_indice25(share_evolucao_X_matriculas_publica),
         ind25_evolucao_X_log_matriculas_publica = recalcular_indice25(share_evolucao_e_nota_X_log_matriculas_publica),
         ind25_evolucao_e_nota = recalcular_indice25(share_evolucao_e_nota),
         ind25_evolucao_e_nota_X_matriculas_publica = recalcular_indice25(share_evolucao_e_nota_X_matriculas_publica),
         ind25_evolucao_e_nota_X_log_matriculas_publica = recalcular_indice25(share_evolucao_e_nota_X_log_matriculas_publica)) %>% 
  mutate(ind25_nota_X_matriculas_municipal = recalcular_indice25(share_nota_X_matriculas_municipal),
         ind25_nota_X_log_matriculas_municipal = recalcular_indice25(share_nota_X_log_matriculas_municipal),
         ind25_evolucao_X_matriculas_municipal = recalcular_indice25(share_evolucao_X_matriculas_municipal),
         ind25_evolucao_X_log_matriculas_municipal = recalcular_indice25(share_evolucao_e_nota_X_log_matriculas_municipal),
         ind25_evolucao_e_nota_X_matriculas_municipal = recalcular_indice25(share_evolucao_e_nota_X_matriculas_municipal),
         ind25_evolucao_e_nota_X_log_matriculas_municipal = recalcular_indice25(share_evolucao_e_nota_X_log_matriculas_municipal))

# 10%
df_municipios <- df_municipios %>% 
  mutate(ind10_nota = recalcular_indice10(share_nota),
         ind10_nota_X_matriculas_publica = recalcular_indice10(share_nota_X_matriculas_publica),
         ind10_nota_X_log_matriculas_publica = recalcular_indice10(share_nota_X_log_matriculas_publica),
         ind10_evolucao = recalcular_indice10(share_evolucao),
         ind10_evolucao_X_matriculas_publica = recalcular_indice10(share_evolucao_X_matriculas_publica),
         ind10_evolucao_X_log_matriculas_publica = recalcular_indice10(share_evolucao_e_nota_X_log_matriculas_publica),
         ind10_evolucao_e_nota = recalcular_indice10(share_evolucao_e_nota),
         ind10_evolucao_e_nota_X_matriculas_publica = recalcular_indice10(share_evolucao_e_nota_X_matriculas_publica),
         ind10_evolucao_e_nota_X_log_matriculas_publica = recalcular_indice10(share_evolucao_e_nota_X_log_matriculas_publica)) %>% 
  mutate(ind10_nota_X_matriculas_municipal = recalcular_indice10(share_nota_X_matriculas_municipal),
         ind10_nota_X_log_matriculas_municipal = recalcular_indice10(share_nota_X_log_matriculas_municipal),
         ind10_evolucao_X_matriculas_municipal = recalcular_indice10(share_evolucao_X_matriculas_municipal),
         ind10_evolucao_X_log_matriculas_municipal = recalcular_indice10(share_evolucao_e_nota_X_log_matriculas_municipal),
         ind10_evolucao_e_nota_X_matriculas_municipal = recalcular_indice10(share_evolucao_e_nota_X_matriculas_municipal),
         ind10_evolucao_e_nota_X_log_matriculas_municipal = recalcular_indice10(share_evolucao_e_nota_X_log_matriculas_municipal))

# Calculando recebimento anual de cada municipio sob regimes alternativos de distribuicao

calcular_transferencia <- function(indice_participacao) {df_municipios$montante_disponivel * indice_participacao / 100}

# 25%
df_municipios <- df_municipios %>% 
  mutate(transf25_nota = calcular_transferencia(ind25_nota),
         transf25_nota_X_matriculas_publica = calcular_transferencia(ind25_nota_X_matriculas_publica),
         transf25_nota_X_log_matriculas_publica = calcular_transferencia(ind25_nota_X_log_matriculas_publica),
         transf25_evolucao = calcular_transferencia(ind25_evolucao),
         transf25_evolucao_X_matriculas_publica = calcular_transferencia(ind25_evolucao_X_matriculas_publica),
         transf25_evolucao_X_log_matriculas_publica = calcular_transferencia(ind25_evolucao_e_nota_X_log_matriculas_publica),
         transf25_evolucao_e_nota = calcular_transferencia(ind25_evolucao_e_nota),
         transf25_evolucao_e_nota_X_matriculas_publica = calcular_transferencia(ind25_evolucao_e_nota_X_matriculas_publica),
         transf25_evolucao_e_nota_X_log_matriculas_publica = calcular_transferencia(ind25_evolucao_e_nota_X_log_matriculas_publica)) %>% 
  mutate(transf25_nota_X_matriculas_municipal = calcular_transferencia(ind25_nota_X_matriculas_municipal),
         transf25_nota_X_log_matriculas_municipal = calcular_transferencia(ind25_nota_X_log_matriculas_municipal),
         transf25_evolucao_X_matriculas_municipal = calcular_transferencia(ind25_evolucao_X_matriculas_municipal),
         transf25_evolucao_X_log_matriculas_municipal = calcular_transferencia(ind25_evolucao_e_nota_X_log_matriculas_municipal),
         transf25_evolucao_e_nota_X_matriculas_municipal = calcular_transferencia(ind25_evolucao_e_nota_X_matriculas_municipal),
         transf25_evolucao_e_nota_X_log_matriculas_municipal = calcular_transferencia(ind25_evolucao_e_nota_X_log_matriculas_municipal))

# 10%
df_municipios <- df_municipios %>% 
  mutate(transf10_nota = calcular_transferencia(ind10_nota),
         transf10_nota_X_matriculas_publica = calcular_transferencia(ind10_nota_X_matriculas_publica),
         transf10_nota_X_log_matriculas_publica = calcular_transferencia(ind10_nota_X_log_matriculas_publica),
         transf10_evolucao = calcular_transferencia(ind10_evolucao),
         transf10_evolucao_X_matriculas_publica = calcular_transferencia(ind10_evolucao_X_matriculas_publica),
         transf10_evolucao_X_log_matriculas_publica = calcular_transferencia(ind10_evolucao_e_nota_X_log_matriculas_publica),
         transf10_evolucao_e_nota = calcular_transferencia(ind10_evolucao_e_nota),
         transf10_evolucao_e_nota_X_matriculas_publica = calcular_transferencia(ind10_evolucao_e_nota_X_matriculas_publica),
         transf10_evolucao_e_nota_X_log_matriculas_publica = calcular_transferencia(ind10_evolucao_e_nota_X_log_matriculas_publica)) %>% 
  mutate(transf10_nota_X_matriculas_municipal = calcular_transferencia(ind10_nota_X_matriculas_municipal),
         transf10_nota_X_log_matriculas_municipal = calcular_transferencia(ind10_nota_X_log_matriculas_municipal),
         transf10_evolucao_X_matriculas_municipal = calcular_transferencia(ind10_evolucao_X_matriculas_municipal),
         transf10_evolucao_X_log_matriculas_municipal = calcular_transferencia(ind10_evolucao_e_nota_X_log_matriculas_municipal),
         transf10_evolucao_e_nota_X_matriculas_municipal = calcular_transferencia(ind10_evolucao_e_nota_X_matriculas_municipal),
         transf10_evolucao_e_nota_X_log_matriculas_municipal = calcular_transferencia(ind10_evolucao_e_nota_X_log_matriculas_municipal))


# Calculando variacao na transferencia (total e percentual)
df_variacao_transferencia <- df_municipios %>%
  gather(transf25_nota:transf10_evolucao_e_nota_X_log_matriculas_municipal, key = 'simulacao', value = 'transferencia_simulada') %>%
  group_by(municipio, Codmun7, simulacao) %>%
  summarise(variacao_transf = transferencia_simulada - total_recebido_municipio,
            variacao_perc_transf = variacao_transf / total_recebido_municipio) %>%
  ungroup()

# Incluindo variacoes na base de municipios
df_municipios <- df_municipios %>% 
  left_join(df_variacao_transferencia %>% select(Codmun7, simulacao, variacao_transf) %>% mutate(simulacao = str_c('var_', simulacao)) %>% spread(key = simulacao, value = variacao_transf),
            by = 'Codmun7') %>% 
  left_join(df_variacao_transferencia %>% select(Codmun7, simulacao, variacao_perc_transf) %>% mutate(simulacao = str_c('var_perc_', simulacao)) %>% spread(key = simulacao, value = variacao_perc_transf),
            by = 'Codmun7')

saveRDS(df_municipios, 'Dados/df_municipios_etapa3.rds')
