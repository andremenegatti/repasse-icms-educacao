normalizar_variavel <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}