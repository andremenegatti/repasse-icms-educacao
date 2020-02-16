# Funcao para contar ocorrencias dentro de uma faixa de valores
contar_ocorrencias_na_faixa <- function(var, lower, upper) {
  sum(if_else(var > lower & var <= upper, 1, 0))
  }