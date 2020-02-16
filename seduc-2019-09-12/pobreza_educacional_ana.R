library(tidyverse)
theme_set(theme_bw())

glimpse(alunos_sp_ana)

df_alunos_ana <- alunos_sp_ana %>% 
  filter(!is.na(municipio),
         IN_PREENCHIMENTO_LP == 1,
         IN_PRESENCA_LP == 1,
         IN_SITUACAO_CENSO == 1,
         ID_DEPENDENCIA_ADM == 3) %>% 
  mutate(distancia_minimo_lpo = PROFICIENCIA_LPO_ANA - 525,
         abaixo_minimo_lpo = ifelse(PROFICIENCIA_LPO_ANA < 525, 1, 0)) 

df_pob_ed_ana <- df_alunos_ana %>% 
  group_by(ID_EXAME_ANA, municipio) %>% 
  summarise(pob_ed_lpo = mean(abaixo_minimo_lpo, na.rm = TRUE),
            inten_pob_ed_lpo = ifelse(distancia_minimo_lpo < 0,
                                     distancia_minimo_lpo,
                                     0) %>% mean(na.rm = TRUE) * -1,
            participantes_regulares = n(),
            total_abaixo_minimo_lpo = sum(abaixo_minimo_lpo, na.rm = TRUE)
  ) %>% 
  ungroup()

ggplot(df_pob_ed_ana, aes(x = pob_ed_lpo)) +
  geom_histogram(bins = 25, alpha = 0.8, fill = 'steelblue') +
  facet_grid(. ~ ID_EXAME_ANA) +
  ggtitle('ANA - Pobreza Educacional')

ggplot(df_pob_ed_ana, aes(x = inten_pob_ed_lpo)) +
  geom_histogram(bins = 25, alpha = 0.8, fill = 'steelblue') +
  facet_grid(. ~ ID_EXAME_ANA) +
  ggtitle('ANA - Intensidade da pobreza Educacional')

saveRDS(df_pob_ed_ana, 'df_pob_ed_ana.rds')

# 
# municipios_imputados
# 
# nomes_municipios[!nomes_municipios %in% df_alunos_ana$municipio[df_alunos_ana$ID_EXAME_ANA == 2014]]
# nomes_municipios[!nomes_municipios %in% df_alunos_ana$municipio] %>% length()
