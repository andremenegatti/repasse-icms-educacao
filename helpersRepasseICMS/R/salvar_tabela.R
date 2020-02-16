# Funcao para salvar tabelas de resultados
salvar_tabela <- function(x, file) {
  write.table(x = x,
              file = file,
              sep = ';',
              quote = FALSE,
              col.names = TRUE,
              row.names = FALSE)
  }