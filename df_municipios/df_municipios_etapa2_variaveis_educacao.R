library(tidyverse)

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

#### ABRINDO BASES ####

# Abrindo base de municipios e ja incluindo codigo do IBGE
df_municipios <- readRDS('df_municipios_etapa1.rds') %>% 
  left_join(readRDS('Dados/codigos_sp.rds') %>% select(-Município), by = 'municipio')

# Abrindo base com notas do IDEB
IDEB_1a5_ano <- readxl::read_excel('Dados/IDEB_SP.xlsx', sheet = 1) %>% 
  gather(-COD_MUN, -NO_MUNICIPIO, key = "ano", value = "nota_ideb") %>% 
  mutate(ano = str_remove(ano, "IDEB_"),
         nota_ideb = as.numeric(nota_ideb))

# Criando base de dados de matriculas
matriculas_sp <- tibble()
for (i in 1:8) {
  sheet <- readxl::read_excel('Dados/matriculas_sp.xlsx',
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
             federal_5_ano + estadual_5_ano + municipal_5_ano,
           matriculas_rede_municipal = municipal_1_ano + municipal_2_ano + municipal_3_ano + municipal_4_ano + municipal_5_ano)
  matriculas_sp <- bind_rows(matriculas_sp, sheet)
  rm(sheet)
}
# saveRDS(matriculas_sp, 'matriculas_sp_completa.rds')

#### JUNTANDO BASES ####
df_municipios <- df_municipios %>% 
  left_join(IDEB_1a5_ano %>% rename(Codmun7 = COD_MUN, ano_calculo = ano) %>% select(-NO_MUNICIPIO),
            by = c("Codmun7", "ano_calculo")) %>% 
  # Selecionando apenas variavel 'matriculas_rede_publica'
  left_join(matriculas_sp %>% select(Codmun7, ano_calculo = ano, matriculas_rede_publica, matriculas_rede_municipal),
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

# Funcoes uteis
# normalizar_variavel <- function(x) {(x - min(x)) / (max(x) - min(x))}
# calcular_share <- function(x) {x / sum(x)}
source("normalizar_variavel.R")
source("calcular_share.R")

# Em alguns anos, nao temos a nota do IDEB para alguns municipios.
# Solucao: imputar a media do ano
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
  mutate(nota_ideb_imp_norm = normalizar_variavel(nota_ideb_imp)) %>%
  ungroup()

# Variavel de evolucao do IDEB
df_municipios <- df_municipios %>% 
  arrange(municipio, ano_calculo) %>% 
  group_by(municipio) %>% 
  # Lag de duas observacoes porque so temos notas do IDEB a cada dois anos
  mutate(nota_ideb_anterior = lag(nota_ideb_imp, 2),
         evolucao_ideb = nota_ideb_imp - nota_ideb_anterior) %>% 
  ungroup()

# Evolucao IDEB normalizada
df_municipios <- df_municipios %>% 
  group_by(ano_calculo) %>% 
  mutate(evolucao_ideb_norm = normalizar_variavel(evolucao_ideb),
         # Criando tambem variavel com a soma: nota normalizada + evolucao normalizada
         evolucao_e_nota = evolucao_ideb_norm + nota_ideb_imp_norm) %>% 
  ungroup()

# Correcao necessaria para ponderar pelo log da rede municipal
# Algumas observacoes possuem 0 alunos na rede municipal: precisamos garantir que conseguimos aplicar o log, somando um numero muito pequeno a esses zeros
# Esses casos vao entao possuir um log negativo, que corrigimos para ser igual a zero
df_municipios <- df_municipios %>% 
  mutate(log_matriculas_rede_municipal = log(if_else(matriculas_rede_municipal == 0, matriculas_rede_municipal + 1e-8, matriculas_rede_municipal)),
         log_matriculas_rede_municipal = if_else(log_matriculas_rede_municipal < 0, 0, log_matriculas_rede_municipal))

# Variaveis ponderadas
df_municipios <- df_municipios %>% 
  group_by(ano_calculo) %>% 
  # Ponderacoes pela rede publica
  mutate(nota_X_matriculas_publica = nota_ideb_imp_norm * matriculas_rede_publica,
         nota_X_log_matriculas_publica = nota_ideb_imp_norm * log(matriculas_rede_publica),
         evolucao_X_matriculas_publica = evolucao_ideb_norm * matriculas_rede_publica,
         evolucao_X_log_matriculas_publica = evolucao_ideb_norm * log(matriculas_rede_publica),
         evolucao_e_nota_X_matriculas_publica = evolucao_e_nota * matriculas_rede_publica,
         evolucao_e_nota_X_log_matriculas_publica = evolucao_e_nota * log(matriculas_rede_publica)) %>% 
  # Ponderacoes pela rede municipal
  mutate(nota_X_matriculas_municipal = nota_ideb_imp_norm * matriculas_rede_municipal,
         nota_X_log_matriculas_municipal = nota_ideb_imp_norm * log_matriculas_rede_municipal,
         evolucao_X_matriculas_municipal = evolucao_ideb_norm * matriculas_rede_municipal,
         evolucao_X_log_matriculas_municipal = evolucao_ideb_norm * log_matriculas_rede_municipal,
         evolucao_e_nota_X_matriculas_municipal = evolucao_e_nota * matriculas_rede_municipal,
         evolucao_e_nota_X_log_matriculas_municipal = evolucao_e_nota * log_matriculas_rede_municipal) %>% 
  ungroup()


# Calculando shares das variaveis de educacao
df_municipios2 <- df_municipios %>%
  group_by(ano_calculo) %>%
  mutate(share_nota = calcular_share(nota_ideb_imp_norm),
         share_nota_X_matriculas_publica = calcular_share(nota_X_matriculas_publica),
         share_nota_X_log_matriculas_publica = calcular_share(nota_X_log_matriculas_publica),
         share_evolucao = calcular_share(evolucao_ideb_norm),
         share_evolucao_X_matriculas_publica = calcular_share(evolucao_X_matriculas_publica),
         share_evolucao_X_log_matriculas_publica = calcular_share(evolucao_X_log_matriculas_publica),
         share_evolucao_e_nota = calcular_share(evolucao_e_nota),
         share_evolucao_e_nota_X_matriculas_publica = calcular_share(evolucao_e_nota_X_matriculas_publica),
         share_evolucao_e_nota_X_log_matriculas_publica = calcular_share(evolucao_e_nota_X_log_matriculas_publica)) %>%
  mutate(share_nota_X_matriculas_municipal = calcular_share(nota_X_matriculas_municipal),
         share_nota_X_log_matriculas_municipal = calcular_share(nota_X_log_matriculas_municipal),
         share_evolucao_X_matriculas_municipal = calcular_share(evolucao_X_matriculas_municipal),
         share_evolucao_X_log_matriculas_municipal = calcular_share(evolucao_X_log_matriculas_municipal),
         share_evolucao_e_nota_X_matriculas_municipal = calcular_share(evolucao_e_nota_X_matriculas_municipal),
         share_evolucao_e_nota_X_log_matriculas_municipal = calcular_share(evolucao_e_nota_X_log_matriculas_municipal)) %>%
  ungroup()

# Forma menos bracal de calcular shares
df_shares_educacao <- df_municipios %>%
  select(municipio, ano_calculo,
         nota = nota_ideb_imp_norm,
         nota_X_matriculas_publica, nota_X_log_matriculas_publica,
         nota_X_matriculas_municipal, nota_X_log_matriculas_municipal,
         evolucao = evolucao_ideb_norm,
         evolucao_X_matriculas_publica, evolucao_X_log_matriculas_publica,
         evolucao_X_matriculas_municipal, evolucao_X_log_matriculas_municipal,
         evolucao_e_nota,
         evolucao_e_nota_X_matriculas_publica, evolucao_e_nota_X_log_matriculas_publica,
         evolucao_e_nota_X_matriculas_municipal, evolucao_e_nota_X_log_matriculas_municipal) %>%
  gather(-municipio, -ano_calculo, key = 'variavel', value = 'valor') %>%
  group_by(ano_calculo, variavel) %>%
  mutate(share = calcular_share(valor)) %>%
  ungroup() %>%
  mutate(variavel = str_c('share_', variavel)) %>%
  select(-valor) %>%
  spread(key = variavel, value = share)

# Juntando shares calculados na base de municipios
df_municipios <- df_municipios %>%
  left_join(df_shares_educacao, by = c('municipio', 'ano_calculo'))

# Analisando a distribuicao dos novos shares
df_municipios %>%
  select(municipio, starts_with("share_")) %>%
  gather(-municipio, key = "variavel", value = "share") %>%
  filter(variavel %in% names(df_municipios)[45:59]) %>%
  mutate(variavel = str_remove(variavel, 'share_')) %>%
  filter(municipio != "SAO PAULO") %>%
  ggplot(mapping = aes(x = share, fill = variavel)) +
  geom_histogram(alpha = 0.5, position = 'identity') +
  facet_wrap(~ variavel, scales = 'free')

df_municipios %>% 
  select(starts_with('share_')) %>% 
  select(contains('evolucao'), contains('nota')) %>% 
  summary()

# Removendo variaveis que nao serao utilizadas e salvando base de dados
df_municipios %>% 
  select(-media_ideb_ano,
         -nota_ideb_imp,
         -nota_ideb_anterior) %>% 
  saveRDS('df_municipios_etapa2.rds')
