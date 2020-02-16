selecionar_municipios_maior_variacao <- function(df, valor_inicial, valor_final, size = 25, variacao_percentual = TRUE, sentido_variacao) {
  
  if (!sentido_variacao %in% c('positiva', 'negativa', 'absoluta')) {
    stop(" 'sentido_variacao' deve ser 'positiva', 'negativa' ou 'absoluta' ")
  }
  
  valor_inicial <- enquo(valor_inicial)
  valor_final <- enquo(valor_final)
  
  # Calculando variacoes de interesse
  df <- df %>% 
    select(municipio, !!valor_inicial, !!valor_final) %>% 
    filter(!is.na(!!valor_final)) %>% 
    group_by(municipio) %>% 
    mutate(variacao = !!valor_final - !!valor_inicial,
           variacao_absoluta = abs(variacao),
           variacao_percentual = variacao / !!valor_inicial,
           variacao_absoluta_percentual = variacao_absoluta / !!valor_inicial) %>% 
    ungroup() %>% 
    distinct()
  
  # DEFAULT: VARIACAO PERCENTUAL
  if (variacao_percentual) {
    
    if (sentido_variacao == 'positiva') {
      df <- df %>% filter(variacao_percentual > 0) %>% arrange(desc(variacao_percentual))
    } else if (sentido_variacao == 'negativa') {
      df <- df %>% filter(variacao_percentual < 0) %>% arrange(variacao_percentual)
    } else {
      df <- df %>% arrange(desc(variacao_absoluta_percentual))
    }
    
  # Caso se deseje VARIACAO EM NIVEL
  } else {
    
    if (sentido_variacao == 'positiva') {
      df <- df %>% filter(variacao > 0) %>% arrange(desc(variacao))
    } else if (sentido_variacao == 'negativa') {
      df <- df %>% filter(variacao < 0) %>% arrange(variacao)
    } else {
      df <- df %>% arrange(desc(variacao_absoluta))
    }
    
  }
  
  if (size > length(df$municipio)) {
    size <- length(df$municipio)
    message(str_c('Definindo size = ', as.character(size)))
  } 
  
  df$municipio[1:size]
    
}
