substituir_caracteres_especiais <- function(x) {
  toupper(x) %>% 
    str_replace_all('�', 'A') %>% 
    str_replace_all('�', 'O') %>% 
    str_replace_all('�', 'E') %>% 
    str_replace_all('�', 'A') %>% 
    str_replace_all('�', 'O') %>% 
    str_replace_all('�', 'C') %>% 
    str_replace_all('�', 'I') %>% 
    str_replace_all('�', 'A') %>% 
    str_replace_all('�', 'E') %>% 
    str_replace_all('�', 'O') %>% 
    str_replace_all('�', 'U')
}