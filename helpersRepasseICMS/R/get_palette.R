get_palette <- function(breaks) {
  
  reds_full <- RColorBrewer::brewer.pal(11, 'RdBu')[1:4]
  blues_full <- RColorBrewer::brewer.pal(9, 'Blues')[3:8]
  
  negative <- breaks[breaks < 0]
  positive <- breaks[breaks > 0]
  
  reds <- reds_full[(5 - length(negative)):4]
  blues <- blues_full[1:length(positive)]
  
  div_palette <- c(reds, blues)
  
  div_palette
  
}