library(tidyverse)
library(lubridate)

# IMPORTANDO BASES ------------------------------------------------------------

# Montando base de municipios a partir das planilhas de Excel
df_municipios <- tibble()
for (i in 1:13) {
  sheet <- readxl::read_excel('data/dados_DIPAM_2005_2017.xlsx',
                              sheet = i) %>%
    # Lembrando: a distribuicao em t obedece os indices de participacao calcu-
    # lados para t-2. Variaveis de ano diferentes para lidar com isso:
    mutate(ano_calculo = as.character(2004 + i),
           ano_distribuicao = as.character(2004 + i + 2)) 
  df_municipios <- bind_rows(df_municipios, sheet)
  rm(sheet)
}

# Montando bases com dados da arrecadacao total de ICMS
historico_icms_anual <-
  readxl::read_excel('data/historico_arrecadacao_icms_sp.xlsx')  %>% 
  filter(str_detect(mes, 'Total')) %>% 
  mutate(mes = str_extract(mes, '\\d{4}')) %>% 
  rename(ano = mes)

# Incluindo dados de arrecadacao na base df_municipios
df_municipios <- historico_icms_anual %>% 
  mutate(ano_distribuicao = as.character(ano)) %>%
  select(ano_distribuicao, total_icms_estado = arrecadacao) %>% 
  right_join(df_municipios,by = 'ano_distribuicao') %>% # ano_distribuicao <<<<
  select(ano_distribuicao, ano_calculo,
         codigo:ind_participacao, total_icms_estado)


# CRIANDO VARIÁVEIS IMPORTANTES -----------------------------------------------
# Calculando montante destinado aos municipios (25% do total da arrecadacao de ICMS)
# ja descontando os 20% destinados ao FUNDEB
df_municipios <- df_municipios %>% 
  mutate(montante_disponivel = 0.25 * total_icms_estado * 0.8,
         total_recebido_municipio = montante_disponivel * ind_participacao / 100)

# Lei Estadual 8510/1993, art. 1, inc. I:
# Para o valor adicionado, considera-se a participacao do municipio no total
# estadual NOS DOIS EXERCICIOS ANTERIORES AO DA APURACAO.
# Vamos criar variaveis com lags do valor adicionado para calcular essa participacao
df_municipios <- df_municipios %>%
  arrange(ano_calculo, municipio) %>% 
  nest(-municipio) %>% 
  mutate(data = 
           map(.x = data,
               .f = ~ .x %>% 
                 mutate(
                   lag_valor_adicionado = lag(valor_adicionado),
                   lag2_valor_adicionado = lag(lag_valor_adicionado)
                   ))) %>% 
  unnest() %>% 
  mutate(valor_adicionado_2anos = 
           valor_adicionado + lag_valor_adicionado,
         valor_adicionado_2anos_2 = 
           lag_valor_adicionado + lag2_valor_adicionado)


# Calculando share anual de cada municipio para variaveis relevantes
df_municipios <- df_municipios %>%
  group_by(ano_calculo) %>% 
  mutate(share_valor_adicionado = valor_adicionado / sum(valor_adicionado),
         share_valor_adicionado_2anos = valor_adicionado_2anos / sum(valor_adicionado_2anos),
         share_valor_adicionado_2anos_2 = valor_adicionado_2anos_2 / sum(valor_adicionado_2anos_2),
         share_pop = pop / sum(pop),
         share_receita_tributaria = receita_tributaria / sum(receita_tributaria),
         share_area_cultivada = area_cultivada / sum(area_cultivada),
         share_area_inundada = area_inundada / sum(area_inundada)) %>% 
  ungroup()


# É POSSÍVEL CALCULAR O INDICE DE PARTICIPACAO? -------------------------------
df_municipios <- df_municipios %>% 
  mutate(
    ind_participacao_calculado = 76 * share_valor_adicionado 
    + 13 * share_pop
    + 5 * share_receita_tributaria
    + 3 * share_area_cultivada 
    + 0.5 * share_area_inundada 
    + 0.5 * ind_protegida / 100
    + 2 * 1/645,
    
    ind_participacao_calculado_2 = 
      ind_participacao_calculado - 76 * share_valor_adicionado + 76 * share_valor_adicionado_2anos,
    
    ind_participacao_calculado_3 = 
      ind_participacao_calculado - 76 * share_valor_adicionado + 76 * share_valor_adicionado_2anos_2)

# Calculando diferenca e diferenca percentual entre os indices
df_municipios <- df_municipios %>% 
  mutate(dif_dipam_calculado = ind_participacao - ind_participacao_calculado,
         dif_perc_dipam_calculado = dif_dipam_calculado / ind_participacao,
         dif_dipam_calculado_2 = ind_participacao - ind_participacao_calculado_2,
         dif_perc_dipam_calculado_2 = dif_dipam_calculado_2 / ind_participacao,
         dif_dipam_calculado_3 = ind_participacao - ind_participacao_calculado_3,
         dif_perc_dipam_calculado_3 = dif_dipam_calculado_3 / ind_participacao)

# Comparando indices calculados com indice disponibilizado pela Secretaria da Fazenda/SP
# Os histogramas indicam que a diferenca entre o valor calculado e o valor oficial fica mais proxima de zero quando
# se utiliza a variavel share_valor_adicionado_2anos para o calculo do ind_participacao
ggplot(data = df_municipios %>% 
         select(ano_calculo, starts_with('dif_perc_')) %>% 
         gather(-ano_calculo, key = 'var', value = 'value')) +
  geom_histogram(mapping = aes(x = value,
                               group = var, fill = var),
                 position = 'identity', alpha = 0.3) +
  facet_wrap(facets = 'ano_calculo', scales = 'free')

# Abaixo, temos evidencias adicionais de que o calculo do share de valor adicionado deve ser utilizar dados de t e t-1:
attach(df_municipios)

# O root mean squared error para share_valor_adicionado_2anos e mais proximo de zero do que os outros dois:
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))
map_dbl(.x = list(ind_participacao_calculado,
                  ind_participacao_calculado_2,
                  ind_participacao_calculado_3),
        .f = ~ rmse(ind_participacao, .x)) %>% 
  set_names(c('share_valor_adicionado',
              'share_valor_adicionado_2anos',
              'share_valor_adicionado_2anos2'))

# Apenas para share_valor_adicionado_2anos temos 1Q, mediana e 3Q = 0
summary(dif_dipam_calculado)
summary(dif_dipam_calculado_2)
summary(dif_dipam_calculado_3)

# Diferencas percentuais absolutas para os resultados obtidos com
# share_valor_adicionado_2anos parecem suficientemente proximos de zero,
# com media de 0,11% e maximo de 2,74%:
summary(abs(dif_perc_dipam_calculado_2))

# Considerando apenas 2018, a diferenca percentual absoluta máxima ? 0,7%
df_municipios %>% 
  filter(ano_distribuicao == '2018') %>%
  select(dif_perc_dipam_calculado_2) %>%
  mutate(dif_perc_dipam_calculado_2 = abs(dif_perc_dipam_calculado_2)) %>% 
  summary()

detach(df_municipios)

# Portanto, parece possivel replicar o calculo os indice de participacao a partir dos dados diponiveis no DIPAM
# Logo, podemos utilizar esses dados para realizar as simulacoes

# Excluindo variaveis que nao serao mais utilizadas e salvando base de dados
df_municipios %>% 
  select(-starts_with('dif'),
         -share_valor_adicionado,
         -share_valor_adicionado_2anos_2,
         -valor_adicionado_2anos_2,
         -lag_valor_adicionado,
         -lag2_valor_adicionado,
         -ind_participacao_calculado,
         -ind_participacao_calculado_3) %>% 
  rename(ind_participacao_calculado = ind_participacao_calculado_2) %>% 
  saveRDS('df_municipios_etapa1.rds')
  