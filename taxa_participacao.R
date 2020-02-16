library(tidyverse)

glimpse(escolas_sp_p_brasil)

df_taxa_part_p_brasil <- escolas_sp_p_brasil %>% 
  filter(!is.na(municipio),
         ID_DEPENDENCIA_ADM != 4) %>% 
  group_by(ID_PROVA_BRASIL, Codmun7) %>% 
  summarise(alunos_presentes = sum(NU_PRESENTES_5EF, na.rm = TRUE),
            alunos_matriculados = sum(NU_MATRICULADOS_CENSO_5EF, na.rm = TRUE),
            taxa_participacao = alunos_presentes / alunos_matriculados) %>% 
  ungroup()

summary(df_taxa_part_p_brasil)

saveRDS(df_taxa_part_p_brasil, 'df_taxa_part_p_brasil.rds')

glimpse(escolas_sp_ana)

df_taxa_part_ana <- escolas_sp_ana %>% 
  filter(!is.na(municipio),
         ID_DEPENDENCIA_ADM != 4) %>% 
  group_by(ID_EXAME_ANA, Codmun7) %>% 
  summarise(alunos_presentes = sum(NU_PRESENTES_LP, na.rm = TRUE),
            alunos_matriculados = sum(NU_MATRICULADOS_CENSO, na.rm = TRUE),
            taxa_participacao = alunos_presentes / alunos_matriculados) %>% 
  ungroup()

summary(df_taxa_part_ana)

saveRDS(df_taxa_part_ana, 'df_taxa_part_ana.rds')
