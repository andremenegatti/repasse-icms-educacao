library(tidyverse)

setwd("C:/Users/Dell/Desktop/Estudo ICMS")


# Abrindo base com relacao nome-codigo dos municipios paulistas
codigos_sp <- readRDS('Dados/codigos_sp.rds') %>% 
  select(-Município)


#### ALUNOS PROVA BRASIL ####
dados_alunos_p_brasil <- list()
for (i in c("2015", "2017")) {
  filename <- str_c('Microdados/SAEB_', i, '/DADOS/TS_ALUNO_5EF.csv')
  dados_alunos_p_brasil[[i]] <- data.table::fread(input = filename, sep = 'auto', sep2 = 'auto', integer64 = 'double') %>% 
    filter(ID_UF == 35) %>% 
    select(Codmun7 = ID_MUNICIPIO, ID_PROVA_BRASIL, ID_AREA, ID_ESCOLA, ID_DEPENDENCIA_ADM, ID_LOCALIZACAO, ID_ALUNO,
           IN_SITUACAO_CENSO, IN_PREENCHIMENTO_PROVA, IN_PROFICIENCIA, IN_PROVA_BRASIL, PROFICIENCIA_LP_SAEB, PROFICIENCIA_MT_SAEB) %>% 
    as_tibble() %>%
    left_join(codigos_sp, by = 'Codmun7') %>% 
    select(municipio, Codmun7:PROFICIENCIA_MT_SAEB)
}

# Juntando
alunos_sp_p_brasil <- bind_rows(dados_alunos_p_brasil)


#### ESCOLAS PROVA BRASIL ####
dados_escolas_p_brasil <- list()
for (i in c("2015", "2017")) {
  filename <- str_c('Microdados/SAEB_', i, '/DADOS/TS_ESCOLA.csv')
  dados_escolas_p_brasil[[i]] <- data.table::fread(input = filename, sep = 'auto', sep2 = 'auto', integer64 = 'double') %>% 
    filter(ID_UF == 35) %>% 
    select(Codmun7 = ID_MUNICIPIO, ID_PROVA_BRASIL, ID_ESCOLA, ID_DEPENDENCIA_ADM, ID_LOCALIZACAO, NU_MATRICULADOS_CENSO_5EF, NU_PRESENTES_5EF, TAXA_PARTICIPACAO_5EF) %>% 
    as_tibble() %>% 
    left_join(codigos_sp, by = 'Codmun7') %>% 
    select(municipio, Codmun7:TAXA_PARTICIPACAO_5EF)
}

escolas_sp_p_brasil <- bind_rows(dados_escolas_p_brasil)



#### ALUNOS ANA ####
dados_alunos_ana <- list()
for (i in c("2014", "2016")) {
  filename <- str_c('Microdados/ANA_', i, '/DADOS/TS_ALUNO.csv')
  dados_alunos_ana[[i]] <- data.table::fread(input = filename, sep = 'auto', sep2 = 'auto', integer64 = 'double') %>% 
    filter(ID_UF == 35) %>% 
    select(Codmun7 = ID_MUNICIPIO, ID_EXAME_ANA, ID_AREA, ID_ESCOLA, ID_DEPENDENCIA_ADM, ID_LOCALIZACAO, ID_ALUNO,
           IN_SITUACAO_CENSO, IN_PRESENCA_LP, IN_PREENCHIMENTO_LP, PROFICIENCIA_LPO_ANA) %>%
    as_tibble() %>% 
    left_join(codigos_sp, by = "Codmun7") %>% 
    select(municipio, Codmun7:PROFICIENCIA_LPO_ANA)
}

# Juntando
alunos_sp_ana <- bind_rows(dados_alunos_ana)


#### ESCOLAS ANA ####
dados_escolas_ana <- list()
for (i in c("2014", "2016")) {
  filename <- str_c('Microdados/ANA_', i, '/DADOS/TS_ESCOLA.csv')
  dados_escolas_ana[[i]] <- data.table::fread(input = filename, sep = 'auto', sep2 = 'auto', integer64 = 'double') %>% 
    filter(ID_UF == 35) %>% 
    select(Codmun7 = ID_MUNICIPIO, ID_EXAME_ANA, ID_ESCOLA, ID_DEPENDENCIA_ADM, ID_LOCALIZACAO,
           NU_MATRICULADOS_CENSO, NU_PRESENTES_LP, NU_VALIDOS_LP, TAXA_PARTICIPACAO_LP, MEDIA_LPO,
           Nivel_1_LPO, Nivel_2_LPO, Nivel_3_LPO, Nivel_4_LPO) %>% 
    as_tibble() %>% 
    left_join(codigos_sp, by = 'Codmun7') %>% 
    select(municipio, Codmun7:Nivel_4_LPO)
}

escolas_sp_ana <- bind_rows(dados_escolas_ana)

saveRDS(escolas_sp_p_brasil, 'escolas_sp_p_brasil.rds')
saveRDS(escolas_sp_ana, 'escolas_sp_ana.rds')
saveRDS(alunos_sp_p_brasil, 'alunos_sp_p_brasil.rds')
saveRDS(alunos_sp_ana, 'alunos_sp_ana.rds')
