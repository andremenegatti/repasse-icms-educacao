library(tidyverse)
theme_set(theme_bw())

glimpse(alunos_sp_p_brasil)

df_alunos <- alunos_sp_p_brasil %>% 
  filter(!is.na(municipio),
         IN_PREENCHIMENTO_PROVA == 1,
         IN_PROVA_BRASIL == 1,
         IN_SITUACAO_CENSO == 1,
         ID_DEPENDENCIA_ADM == 3) %>% 
  mutate(distancia_minimo_mt = PROFICIENCIA_MT_SAEB - 175,
         distancia_minimo_lp = PROFICIENCIA_LP_SAEB - 150) %>% 
  mutate(abaixo_minimo_mt = ifelse(PROFICIENCIA_MT_SAEB < 175, 1, 0),
         abaixo_minimo_lp = ifelse(PROFICIENCIA_LP_SAEB < 150, 1, 0))

df_pob_ed <- df_alunos %>% 
  group_by(ID_PROVA_BRASIL, municipio) %>% 
  summarise(pob_ed_mt = mean(abaixo_minimo_mt, na.rm = TRUE),
            pob_ed_lp = mean(abaixo_minimo_lp, na.rm = TRUE),
            inten_pob_ed_mt = ifelse(distancia_minimo_mt < 0,
                                     distancia_minimo_mt,
                                     0) %>% mean(na.rm = TRUE) * -1,
            inten_pob_ed_lp = ifelse(distancia_minimo_lp < 0,
                                     distancia_minimo_lp,
                                     0) %>% mean(na.rm = TRUE) * -1,
            participantes_regulares = n(),
            # participantes_regulares_min3questoes = sum(IN_PROFICIENCIA),
            total_abaixo_minimo_mt = sum(abaixo_minimo_mt, na.rm = TRUE),
            total_abaixo_minimo_lp = sum(abaixo_minimo_lp, na.rm = TRUE)
            ) %>% 
  ungroup()

df_pob_ed %>% 
  rename(Matemática = pob_ed_mt, Português = pob_ed_lp) %>% 
  gather(Matemática:Português, key = 'Prova', value = 'pob_ed') %>% 
  ggplot(aes(x = pob_ed, fill = Prova)) +
  geom_histogram(alpha = 0.6, bins = 40, position = 'identity') +
  facet_grid(. ~ ID_PROVA_BRASIL) +
  ggtitle('Pobreza educacional',
          subtitle = 'Proporção de alunos abaixo do mínimo adequado') +
  xlab('Proporção de alunos abaixo do mínimo adequado') +
  ylab('Número de municípios')

df_pob_ed %>% 
  rename(Matemática = inten_pob_ed_mt, Português = inten_pob_ed_lp) %>% 
  gather(Matemática:Português, key = 'Prova', value = 'inten_pob_ed') %>% 
  ggplot(aes(x = inten_pob_ed, fill = Prova)) +
  geom_histogram(alpha = 0.6, bins = 40, position = 'identity') +
  facet_grid(. ~ ID_PROVA_BRASIL) +
  ggtitle('Intensidade de pobreza educacional',
          subtitle = 'Distância média do mínimo adequado (apenas notas inferiores)') +
  xlab('Distância média do mínimo adequado') +
  ylab('Número de municípios')


saveRDS(df_pob_ed, 'df_pob_ed_p_brasil.rds')
