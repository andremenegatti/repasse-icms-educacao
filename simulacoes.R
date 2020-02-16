library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

#### ABRINDO BASES ####

df_municipios <- readRDS('df_municipios_sp_icms.rds')

IDEB_1a5_ano <- readxl::read_excel('IDEB_SP.xlsx', sheet = 1) %>% 
  gather(-COD_MUN, -NO_MUNICIPIO, key = "ano", value = "nota_ideb") %>% 
  mutate(ano = str_remove(ano, "IDEB_"),
         nota_ideb = as.numeric(nota_ideb))

matriculas_sp <- tibble()
for (i in 1:8) {
  sheet <- readxl::read_excel('matriculas_sp.xlsx',
                              sheet = i,
                              range = "C5:AD649",
                              col_names = c('municipio', 'Codmun7', 'matriculas_total',
                                            map(.x = 1:5, .f = ~ str_c(c('total', 'federal', 'estadual', 'municipal', 'privada'), '_', .x, '_ano')) %>% unlist() ),
                              col_types = c('text', rep('numeric', 27))) %>%
    # Vamos criar variaveis de ano diferentes para lidar com isso
    mutate(ano = as.character(2018 - i),
           matriculas_rede_publica = federal_1_ano + estadual_1_ano + municipal_1_ano +
                                     federal_2_ano + estadual_2_ano + municipal_2_ano +
                                     federal_3_ano + estadual_3_ano + municipal_3_ano +
                                     federal_4_ano + estadual_4_ano + municipal_4_ano +
                                     federal_5_ano + estadual_5_ano + municipal_5_ano)
  matriculas_sp <- bind_rows(matriculas_sp, sheet)
  rm(sheet)
}
# saveRDS(matriculas_sp, 'matriculas_sp.rds')

#### JUNTANDO BASES ####
df_municipios <- df_municipios %>% 
  left_join(IDEB_1a5_ano %>%
              rename(Codmun7 = COD_MUN, ano_calculo = ano) %>% 
              select(-NO_MUNICIPIO),
            by = c("Codmun7", "ano_calculo")) %>% 
  left_join(matriculas_sp %>%
              select(Codmun7, ano_calculo = ano, matriculas_rede_publica, matriculas_total:privada_5_ano),
            by = c("Codmun7", "ano_calculo"))


#### ANALISANDO MUNICIPIOS SEM NOTA DO IDEB ####

df_sem_ideb <- df_municipios %>%
  filter(ano_calculo %in% as.character(seq(2005, 2017, by = 2))) %>% 
  filter(is.na(nota_ideb)) %>% 
  select(ano_calculo, municipio)

df_sem_ideb_completo <- df_municipios %>% 
  select(ano_calculo, municipio, nota_ideb) %>% 
  filter(ano_calculo %in% as.character(seq(2005, 2017, by = 2)),
         municipio %in% df_sem_ideb$municipio)

# Tabela com numero de municipios sem dados em cada ano
## Parece haver uma tendencia de diminuicao do numero de municipios sem dados
numero_municipios_sem_ideb <- df_sem_ideb %>%
  group_by(ano_calculo) %>% 
  tally()
numero_municipios_sem_ideb

# Tabela com numero de anos em que cada municipio nao tem dados
anos_sem_ideb <- df_sem_ideb %>% 
  group_by(municipio) %>% 
  mutate(anos_sem_ideb = n()) %>% 
  ungroup() %>% 
  arrange(desc(anos_sem_ideb))
anos_sem_ideb

# Juntando tabela acima com df_sem_ideb_completo
df_sem_ideb_completo <- df_sem_ideb_completo %>% 
  left_join(anos_sem_ideb, by = c('municipio', 'ano_calculo')) %>% 
  rename(n = anos_sem_ideb)

# Tabela relacionando numero de municipios e numero de anos sem nota do ideb
anos_sem_ideb %>% 
  group_by(anos_sem_ideb) %>%
  tally()

# A analise a seguir reforca a intuicao de que uma vez que o municipio
# tempo uma nota do ideb, ele tende a ter a nota nos anos seguintes;
# isso porque as observacoes sem notas sao mais frequentes no inicio da amostra e tendem a estar agrupadas
tabular_anos <- function(df = df_sem_ideb_completo, x) table(df[df$n == x & is.na(df$nota_ideb), ]$ano_calculo)
for (i in 1:7) {
  message(str_c("Contando, para cada ano, os municipios sem nota de ideb em ", as.character(i), " periodos"))
  print(tabular_anos(x = i))
  message('-----------------------------------')
}

#### CRIANDO VARIAVEIS DE EDUCACAO ####
df_municipios <- df_municipios %>% 
  mutate(nota_ideb_imp = nota_ideb) %>% 
  group_by(municipio) %>% 
  # Imputacao: replicando valores de t-1 para t
  fill(nota_ideb_imp) %>% 
  # Imputacao: se a nota de um municipio eh missing, imputa-se a media do ano
  group_by(ano_calculo) %>% 
  mutate(media_ideb_ano = mean(nota_ideb_imp, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(nota_ideb_imp = if_else(is.na(nota_ideb_imp), media_ideb_ano, nota_ideb_imp))

# Nota normalizada
df_municipios <- df_municipios %>% 
  group_by(ano_calculo) %>% 
  mutate(nota_ideb_imp_norm = (nota_ideb_imp - min(nota_ideb_imp)) / (max(nota_ideb_imp) - min(nota_ideb_imp)) ) %>%
  ungroup()

# Variavel de evolucao do IDEB
df_municipios <- df_municipios %>% 
  arrange(municipio, ano_calculo) %>% 
  group_by(municipio) %>% 
  mutate(nota_ideb_anterior = lag(nota_ideb_imp, 2),
         evolucao_ideb = nota_ideb_imp - nota_ideb_anterior) %>% 
  ungroup()

# df_municipios %>% 
#   select(municipio, ano_calculo, nota_ideb_imp, nota_ideb_anterior, evolucao_ideb) %>% View()

# Evolucao IDEB normalizada
df_municipios <- df_municipios %>% 
  group_by(ano_calculo) %>% 
  mutate(evolucao_ideb_norm = (evolucao_ideb - min(evolucao_ideb)) / (max(evolucao_ideb) - min(evolucao_ideb)) ) %>% 
  ungroup()

# Shares de educacao
df_municipios <- df_municipios %>% 
  group_by(ano_calculo) %>% 
  # Share nota IDEB original (apos imputacao)
  mutate(share_ideb = nota_ideb_imp / sum(nota_ideb_imp)) %>% 
  # Share nota IDEB normalizada
  mutate(share_ideb_norm = nota_ideb_imp_norm / sum(nota_ideb_imp_norm)) %>% 
  # Share IDEB NORMALIZADO ponderado por numero de matriculas DA REDE PUBLICA
  mutate(ideb_norm_X_matriculas_rede_publica = nota_ideb_imp_norm * matriculas_rede_publica,
         share_ideb_pond_matriculas_rede_publica = ideb_norm_X_matriculas_rede_publica / sum(ideb_norm_X_matriculas_rede_publica)) %>% 
  # Share IDEB NORMALIZADO ponderado pelo LOGARITMO do numero de matriculas DA REDE PUBLICA
  mutate(ideb_norm_X_log_matriculas_rede_publica = nota_ideb_imp_norm * log(matriculas_rede_publica),
         share_ideb_pond_log_matriculas_rede_publica = ideb_norm_X_log_matriculas_rede_publica / sum(ideb_norm_X_log_matriculas_rede_publica)) %>%
  # Share EVOLUCAO NORMALIZADA
  mutate(share_evolucao_ideb = evolucao_ideb_norm / sum(evolucao_ideb_norm)) %>% 
  # Share EVOLUCAO + NOTA
  mutate(share_evolucao_e_nota = (evolucao_ideb_norm + nota_ideb_imp_norm) / sum(evolucao_ideb_norm + nota_ideb_imp_norm)) %>%
  # Share EVOLUCAO + NOTA, ponderado por numero de matriculas na REDE PUBLICA
  mutate(evolucao_e_nota_X_matriculas_rede_publica = (evolucao_ideb_norm + nota_ideb_imp_norm) * matriculas_rede_publica,
         share_evolucao_e_nota_pond_matriculas_rede_publica = evolucao_e_nota_X_matriculas_rede_publica / sum(evolucao_e_nota_X_matriculas_rede_publica)) %>% 
  # Share EVOLUCAO + NOTA, ponderado pelo LOGARITMO de matriculas na REDE PUBLICA
  mutate(evolucao_e_nota_X_log_matriculas_rede_publica = (evolucao_ideb_norm + nota_ideb_imp_norm) * log(matriculas_rede_publica),
         share_evolucao_e_nota_pond_log_matriculas_rede_publica = evolucao_e_nota_X_log_matriculas_rede_publica / sum(evolucao_e_nota_X_log_matriculas_rede_publica)) %>% 
  ungroup()

# Analisando a distribuicao dos novos shares
# df_municipios %>%
#   select(municipio, starts_with("share_")) %>%
#   gather(-municipio, key = "variavel", value = "share") %>%
#   mutate(variavel = str_remove(variavel, 'share_')) %>%
#   filter(variavel %in% c("ideb_pond_log_matriculas_rede_publica", "ideb_norm", "ideb_pond_matriculas_rede_publica", "ideb")) %>%
#   filter(municipio != "SAO PAULO") %>%
#   ggplot(mapping = aes(x = share, fill = variavel)) +
#   geom_histogram(alpha = 0.5, position = 'identity') +
#   facet_wrap(~ variavel)


#### FILTRO DE ANO ####
# df_municipios <- df_municipios %>% 
#   filter(ano_calculo %in% as.character(2010:2016))
df_municipios <- df_municipios %>%
  filter(ano_calculo == '2016')

df_municipios %>% 
  select(starts_with('share_')) %>% 
  select(contains('evolucao'), contains('nota'), contains('ideb')) %>% 
  summary()


#### RECALCULANDO TOTAL RECEBIDO APOS O FILTRO DE ANO ####
# df_municipios <- df_municipios %>% 
#   group_by(municipio) %>% 
#   mutate(total_recebido_todos_periodos = sum(total_recebido_municipio)) %>% 
#   ungroup()

#### INCLUINDO CRITERIO DE EDUCACAO NAS SIMULACOES ####
recalcular_distribuicao <- function(new_var) {df_municipios$montante_disponivel * (df_municipios$share_valor_adicionado_2anos * 0.75 + new_var * 0.25)}
# Calculando recebimento anual de cada municipio sob regimes alternativos de distribuicao

df_municipios <- df_municipios %>% 
  # IDEB normalizado
  mutate(mod_alt_1 = recalcular_distribuicao(share_ideb_norm)) %>% 
  # IDEB normalizado PONDERADO pelo numero de matriculas na rede publica
  mutate(mod_alt_2 = recalcular_distribuicao(share_ideb_pond_matriculas_rede_publica)) %>% 
  # IDEB normalizado PONDERADO pelo LOGARITMO de matriculas na rede publica
  mutate(mod_alt_3 = recalcular_distribuicao(share_ideb_pond_log_matriculas_rede_publica)) %>% 
  # Evolucao normalizada
  mutate(mod_alt_4 = recalcular_distribuicao(share_evolucao_ideb)) %>% 
  # Evolucao normalizada poderada pelo numero de matriculas na rede publica
  mutate(mod_alt_5 = recalcular_distribuicao(share_evolucao_e_nota_pond_matriculas_rede_publica)) %>% 
  # Evolucao normalizada ponderada pelo LOGARITMO do numero de matriculas na rede publica
  mutate(mod_alt_6 = recalcular_distribuicao(share_evolucao_e_nota_pond_log_matriculas_rede_publica)) %>% 
  # Nota normalizada + Evolucao normalizada
  mutate(mod_alt_7 = recalcular_distribuicao(share_evolucao_e_nota)) %>% 
  # Nota normalizada + Evolucao normalizada, ponderado pelo numero de matriculas na rede publica
  mutate(mod_alt_8 = recalcular_distribuicao(share_evolucao_e_nota_pond_matriculas_rede_publica)) %>% 
  # Nota normalizada + Evolucao normalizada, ponderado pelo LOGARITMO de matriculas na rede publica
  mutate(mod_alt_9 = recalcular_distribuicao(share_evolucao_e_nota_pond_log_matriculas_rede_publica))
  
descricao_modelos <- c(
  'IDEB',
  'IDEB ponderado (matriculas)',
  'IDEB ponderado (log matriculas)',
  'Evolucao IDEB',
  'Evolucao IDEB ponderada (matriculas)',
  'Evolucao IDEB ponderada (log matriculas)',
  'Nota + Evolucao',
  'Nota + Evolucao, ponderado (matriculas)',
  'Nota + Evolucao, ponderado (log matriculas)'
)

# Calculando variacao na transferencia (total e percentual)
df_municipios <- df_municipios %>% 
  group_by(municipio) %>% 
  mutate(var_mod_1 = mod_alt_1 - total_recebido_municipio,
         var_mod_2 = mod_alt_2 - total_recebido_municipio,
         var_mod_3 = mod_alt_3 - total_recebido_municipio,
         var_mod_4 = mod_alt_4 - total_recebido_municipio,
         var_mod_5 = mod_alt_5 - total_recebido_municipio,
         var_mod_6 = mod_alt_6 - total_recebido_municipio,
         var_mod_7 = mod_alt_7 - total_recebido_municipio,
         var_mod_8 = mod_alt_8 - total_recebido_municipio,
         var_mod_9 = mod_alt_9 - total_recebido_municipio,
         var_perc_mod_1 = var_mod_1 / total_recebido_municipio,
         var_perc_mod_2 = var_mod_2 / total_recebido_municipio,
         var_perc_mod_3 = var_mod_3 / total_recebido_municipio,
         var_perc_mod_4 = var_mod_4 / total_recebido_municipio,
         var_perc_mod_5 = var_mod_5 / total_recebido_municipio,
         var_perc_mod_6 = var_mod_6 / total_recebido_municipio,
         var_perc_mod_7 = var_mod_7 / total_recebido_municipio,
         var_perc_mod_8 = var_mod_8 / total_recebido_municipio,
         var_perc_mod_9 = var_mod_9 / total_recebido_municipio) %>% 
  ungroup()


# saveRDS(df_municipios, 'df_municipios_sp_icms_2012-2018_educacao.rds')

#### IDENTIFICANDO MUNICIPIOS COM MAIORES VARIACOES PERCENTUAIS NO TOTAL RECEBIDO AO LONGO DE TODOS OS PERIODOS ####

source("selecionar_municipios_maior_variacao.R")

municipios_maior_aumento <- list()
municipios_maior_reducao <- list()

for (i in 1:9) {
  var_name <- str_c("mod_alt_", i)
  df_municipios$temp_var <- df_municipios[[var_name]]
  municipios_maior_aumento[[var_name]] <- selecionar_municipios_maior_variacao(df = df_municipios, valor_inicial = total_recebido_municipio,
                                                                        valor_final = temp_var, sentido_variacao = 'positiva')
  municipios_maior_reducao[[var_name]] <- selecionar_municipios_maior_variacao(df = df_municipios, valor_inicial = total_recebido_municipio,
                                                                        valor_final = temp_var, sentido_variacao = 'negativa')
  df_municipios$temp_var <- NULL
}

# Quantos municipios ganham e quantos perdem em cada modelo
df_municipios %>% 
  select(municipio, starts_with("var_perc_mod")) %>% 
  gather(-municipio, key = 'mod_alt', value = 'var_perc') %>% 
  group_by(mod_alt) %>% 
  summarise(n_aumenta = sum(if_else(var_perc > 0, 1, 0)),
            n_diminui = sum(if_else(var_perc < 0, 1, 0))) %>% 
  mutate(mod_alt = descricao_modelos)


df_categorias_de_impacto <- df_municipios %>%
  rename(var_perc = var_perc_mod_1) %>% 
  select(municipio, var_perc) %>% 
  mutate(categoria_impacto = case_when(var_perc > 0.3 ~ '(0.3, Inf)',
                                       var_perc > 0.15 & var_perc <= 0.3 ~ '(0.15, 0.3]',
                                       var_perc > 0 & var_perc <= 0.15 ~ '(0, 0.15]',
                                       var_perc > -0.15 & var_perc <= 0 ~ '(-0.15, 0]',
                                       var_perc > -0.3 & var_perc <= -0.15 ~ '(0.3, 0.15]',
                                       var_perc <= -0.3 ~ '(-Inf, -0.3]')) %>% 
  group_by(categoria_impacto) %>% 
  tally()

classIntervals(df_municipios$var_perc_mod_1, n = 10, style = 'quantile')
classIntervals(df_municipios$var_perc_mod_1, style = 'fixed', fixedBreaks = c(min(df_municipios$var_perc_mod_1), -0.15, 0, 0.15, 0.5, 1, 2, 3, 4, max(df_municipios$var_perc_mod_1)))
