library(tidyverse)

# Abrindo base Atlas Municipios, com dados de todos os municipios do BRasil
atlas_mun <- readRDS('atlas_mun.rds')

# Selecionando apenas municipios de SP
atlas_sp <- atlas_mun %>% 
  filter(UF.x == 35,
         ANO == '2010')

# Removendo caracteres especiais
codigos_sp <- atlas_sp %>% 
  select(Codmun7, Munic�pio) %>% 
  mutate(municipio = Munic�pio) %>% 
  mutate(municipio = str_replace_all(municipio, '�', 'A') %>% 
           str_replace_all('�', 'O') %>% 
           str_replace_all('�', 'E') %>% 
           str_replace_all('�', 'A') %>% 
           str_replace_all('�', 'O') %>% 
           str_replace_all('�', 'C') %>% 
           str_replace_all('�', 'I') %>% 
           str_replace_all('�', 'A') %>% 
           str_replace_all('�', 'E') %>% 
           str_replace_all('�', 'O') %>% 
           str_replace_all('�', 'U'))

# Corrigindo erros
codigos_sp <- codigos_sp %>% 
  mutate(municipio = str_replace(municipio, 'SAO LUIS DO PARAITINGA', 'SAO LUIZ DO PARAITINGA') %>% 
           str_replace('FLORINIA', 'FLORINEA') %>% 
           str_replace('EMBU$', 'EMBU DAS ARTES') %>% 
           str_replace('MOJI MIRIM', 'MOGI MIRIM') %>% 
           str_replace('PACAEMBU DAS ARTES', 'PACAEMBU'))

# Salvando
saveRDS(codigos_sp, 'codigos_sp.rds')
