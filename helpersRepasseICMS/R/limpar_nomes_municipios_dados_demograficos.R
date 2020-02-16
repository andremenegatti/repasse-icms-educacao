limpar_nomes_municipios_dados_demograficos <- function(x) {
  str_remove(x, '\\(SP\\)') %>% 
    substituir_caracteres_especiais() %>% 
    str_trim(side = 'both') %>% 
    str_replace('BIRITIBA MIRIM', 'BIRITIBA-MIRIM')
  }
