library(tidyverse)

# Abrindo base Atlas Municipios, com dados de todos os municipios do BRasil
atlas_mun <- readRDS('atlas_mun.rds')

# Selecionando apenas municipios de SP
atlas_sp <- atlas_mun %>% 
  filter(UF.x == 35,
         ANO == '2010')

# Removendo caracteres especiais
codigos_sp <- atlas_sp %>% 
  select(Codmun7, Município) %>% 
  mutate(municipio = Município) %>% 
  mutate(municipio = str_replace_all(municipio, 'Ã', 'A') %>% 
           str_replace_all('Õ', 'O') %>% 
           str_replace_all('É', 'E') %>% 
           str_replace_all('Á', 'A') %>% 
           str_replace_all('Ó', 'O') %>% 
           str_replace_all('Ç', 'C') %>% 
           str_replace_all('Í', 'I') %>% 
           str_replace_all('Â', 'A') %>% 
           str_replace_all('Ê', 'E') %>% 
           str_replace_all('Ô', 'O') %>% 
           str_replace_all('Ú', 'U'))

# Corrigindo erros
codigos_sp <- codigos_sp %>% 
  mutate(municipio = str_replace(municipio, 'SAO LUIS DO PARAITINGA', 'SAO LUIZ DO PARAITINGA') %>% 
           str_replace('FLORINIA', 'FLORINEA') %>% 
           str_replace('EMBU$', 'EMBU DAS ARTES') %>% 
           str_replace('MOJI MIRIM', 'MOGI MIRIM') %>% 
           str_replace('PACAEMBU DAS ARTES', 'PACAEMBU'))

# Salvando
saveRDS(codigos_sp, 'codigos_sp.rds')
