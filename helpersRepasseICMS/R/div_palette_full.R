div_palette_full <- function() {
  reds <- RColorBrewer::brewer.pal(11, 'RdBu')[1:4] %>% rev()
  blues <- RColorBrewer::brewer.pal(9, 'Blues')[3:8] %>% rev()
  div_palette <- c(blues, reds)
  div_palette
}