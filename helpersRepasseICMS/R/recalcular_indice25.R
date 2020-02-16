recalcular_indice25 <- function(share_new_var) {
  df_municipios$share_valor_adicionado_2anos * 75 + share_new_var * 25
  }