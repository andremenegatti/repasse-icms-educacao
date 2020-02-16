recalcular_indice10 <- function(share_new_var) {
  75 * df_municipios$share_valor_adicionado_2anos + 12.5 * df_municipios$share_pop + 2.5 * df_municipios$share_receita_tributaria + 10 * share_new_var
  }
