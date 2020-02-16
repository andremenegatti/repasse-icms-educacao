library(tidyverse)

#### ABRINDO #### 
# Municipios DIPAM
df_municipios_inicial <- readRDS('2019_08_14_simulacao_secretaria/df_municipios_etapa1.rds') %>% 
  left_join(readRDS('Dados/codigos_sp.rds') %>% select(-Município), by = 'municipio') %>% 
  filter(ano_calculo == '2016') 

# Reprovacao e abandono
rep_aband_2017 <- readxl::read_excel('taxas_reprovacao_abandono.xlsx', sheet = 2)

# Notas IDEB
IDEB_1a5_ano <- readxl::read_excel('Dados/IDEB_SP.xlsx', sheet = 1) %>% 
  gather(-COD_MUN, -NO_MUNICIPIO, key = "ano", value = "nota_ideb") %>% 
  mutate(ano = str_remove(ano, "IDEB_"),
         nota_ideb = as.numeric(nota_ideb))

# Matriculas
matriculas_sp <- readRDS('Dados/matriculas_sp_completa.rds') %>% 
  filter(ano == '2017')

# Taxa de participacao
taxa_part_p_brasil <- readRDS('df_taxa_part_p_brasil.rds')
taxa_part_ana <- readRDS('df_taxa_part_ana.rds')

# Pobreza educacional
pob_ed_p_brasil <- readRDS('df_pob_ed_p_brasil.rds')
pob_ed_ana <- readRDS('df_pob_ed_ana.rds')


#### JUNTANDO ####

df_municipios2 <- df_municipios_inicial %>% 
  left_join(matriculas_sp %>% 
              filter(ano == '2017') %>% 
              select(Codmun7, matriculas_rede_municipal),
            by = 'Codmun7') %>% 
  left_join(rep_aband_2017 %>% select(-municipio, -ano) %>% rename(Codmun7 = codigo),
            by = 'Codmun7') %>% 
  left_join(taxa_part_p_brasil %>% 
              filter(ID_PROVA_BRASIL == 2015) %>%
              select(Codmun7, taxa_part_p_brasil_2015 = taxa_participacao),
            by = 'Codmun7') %>% 
  left_join(taxa_part_p_brasil %>% 
              filter(ID_PROVA_BRASIL == 2017) %>% 
              select(Codmun7, taxa_part_p_brasil_2017 = taxa_participacao),
            by = 'Codmun7') %>% 
  left_join(taxa_part_ana %>% 
              filter(ID_EXAME_ANA == 2014) %>%
              select(Codmun7, taxa_part_ana_2014 = taxa_participacao),
            by = 'Codmun7') %>% 
  left_join(taxa_part_ana %>% 
              filter(ID_EXAME_ANA == 2016) %>% 
              select(Codmun7, taxa_part_ana_2016 = taxa_participacao),
            by = 'Codmun7') %>% 
  left_join(pob_ed_p_brasil %>% 
              filter(ID_PROVA_BRASIL == 2015) %>% 
              select(municipio, pob_ed_mt_2015 = pob_ed_mt, inten_pob_ed_mt_2015 = inten_pob_ed_mt,
                     pob_ed_lp_2015 = pob_ed_lp, inten_pob_ed_lp_2015 = inten_pob_ed_lp),
            by = 'municipio') %>% 
  left_join(pob_ed_p_brasil %>% 
              filter(ID_PROVA_BRASIL == 2017) %>% 
              select(municipio, pob_ed_mt_2017 = pob_ed_mt, inten_pob_ed_mt_2017 = inten_pob_ed_mt,
                     pob_ed_lp_2017 = pob_ed_lp, inten_pob_ed_lp_2017 = inten_pob_ed_lp),
            by = 'municipio') %>% 
  left_join(pob_ed_ana %>% 
              filter(ID_EXAME_ANA == 2014) %>% 
              select(municipio, pob_ed_lpo_2014 = pob_ed_lpo, inten_pob_ed_lpo_2014 = inten_pob_ed_lpo),
            by = 'municipio') %>% 
  left_join(pob_ed_ana %>% 
              filter(ID_EXAME_ANA == 2016) %>% 
              select(municipio, pob_ed_lpo_2016 = pob_ed_lpo, inten_pob_ed_lpo_2016 = inten_pob_ed_lpo),
            by = 'municipio')

source('normalizar_variavel.R')
source('calcular_share.R')
imputar_mediana <- function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)

df_dropped <- df_municipios2 %>% 
  drop_na()

imputados <- df_municipios_inicial$municipio[!df_municipios_inicial$municipio %in% df_dropped$municipio]

df_municipios3 <- df_municipios2 %>% 
  mutate(dados_imputados = ifelse(municipio %in% imputados, TRUE, FALSE)) %>%
  mutate_if(.predicate = is.numeric, .funs = imputar_mediana) %>%
  # drop_na() %>% 
  mutate(pob_ed_p_brasil_2015 = (pob_ed_lp_2015 + pob_ed_mt_2015) / 2,
         pob_ed_p_brasil_2017 = (pob_ed_lp_2017 + pob_ed_mt_2017) / 2) %>% 
  mutate(ia_p_brasil_2015 = (1 - pob_ed_p_brasil_2015) * ifelse(taxa_part_p_brasil_2015 > 1, 1, taxa_part_p_brasil_2015),
         ia_p_brasil_2017 = (1 - pob_ed_p_brasil_2017) * ifelse(taxa_part_p_brasil_2017 > 1, 1, taxa_part_p_brasil_2017)) %>% 
  mutate(avanco_ia_p_brasil = normalizar_variavel(ia_p_brasil_2017 - ia_p_brasil_2015)) %>% 
  mutate(ia_p_brasil_2015_norm = normalizar_variavel(ia_p_brasil_2015),
         ia_p_brasil_2017_norm = normalizar_variavel(ia_p_brasil_2017)) %>% 
  mutate(ia_ana_2014 = (1 - pob_ed_lpo_2014) * ifelse(taxa_part_ana_2014 > 1, 1, taxa_part_ana_2014),
         ia_ana_2016 = (1 - pob_ed_lpo_2016) * ifelse(taxa_part_ana_2016 > 1, 1, taxa_part_ana_2016)) %>% 
  mutate(avanco_ia_ana = normalizar_variavel(ia_ana_2016 - ia_ana_2014)) %>% 
  mutate(ia_ana_2014_norm = normalizar_variavel(ia_ana_2014),
         ia_ana_2016_norm = normalizar_variavel(ia_ana_2016)) %>% 
  mutate(ind_rendimento = normalizar_variavel(0.6 * abandono + 0.4 * reprovacao)) %>% 
  mutate(log_matriculas_rede_municipal = log(if_else(matriculas_rede_municipal == 0, matriculas_rede_municipal + 1e-8, matriculas_rede_municipal)),
         ind_matriculas = if_else(log_matriculas_rede_municipal < 0, 0, log_matriculas_rede_municipal)) %>% 
  mutate(ide = (0.8 * (0.25 * ia_p_brasil_2017_norm + 0.75 * avanco_ia_p_brasil) + 0.2 * ind_rendimento) * ind_matriculas) %>% 
  mutate(ide2 = ((0.4 * (0.25 * ia_p_brasil_2017_norm + 0.75 * avanco_ia_p_brasil)) + (0.4 * (0.25 * ia_ana_2016_norm + 0.75 * avanco_ia_ana)) + 0.2 * ind_rendimento ) * ind_matriculas) %>% 
  mutate(share_ide = calcular_share(ide)) %>% 
  mutate(share_ide2 = calcular_share(ide2)) %>% 
  mutate(ind_alt1 = 75 * share_valor_adicionado_2anos + 18.5 * share_ide + 6 * share_receita_tributaria + 0.5 * ind_protegida/100,
         ind_alt2 = 75 * share_valor_adicionado_2anos + 10 * share_ide + 8.5 * share_pop + 6 * share_receita_tributaria + 0.5 * ind_protegida/100) %>% 
  mutate(transf_alt1 = montante_disponivel * ind_alt1 / 100,
         transf_alt2 = montante_disponivel * ind_alt2 / 100) %>% 
  mutate(ind_alt3 = 75 * share_valor_adicionado_2anos + 18.5 * share_ide2 + 6 * share_receita_tributaria + 0.5 * ind_protegida/100,
         ind_alt4 = 75 * share_valor_adicionado_2anos + 10 * share_ide2 + 8.5 * share_pop + 6 * share_receita_tributaria + 0.5 * ind_protegida/100) %>% 
  mutate(transf_alt3 = montante_disponivel * ind_alt3 / 100,
         transf_alt4 = montante_disponivel * ind_alt4 / 100)



df_municipios <- df_municipios3 %>% 
  mutate(var_transf_alt1 = transf_alt1 - total_recebido_municipio,
         var_perc_transf_alt1 = var_transf_alt1 / total_recebido_municipio,
         var_transf_alt2 = transf_alt2 - total_recebido_municipio,
         var_perc_transf_alt2 = var_transf_alt2 / total_recebido_municipio) %>% 
  mutate(var_transf_alt3 = transf_alt3 - total_recebido_municipio,
         var_perc_transf_alt3 = var_transf_alt3 / total_recebido_municipio,
         var_transf_alt4 = transf_alt4 - total_recebido_municipio,
         var_perc_transf_alt4 = var_transf_alt4 / total_recebido_municipio)

df_municipios %>% summary()
