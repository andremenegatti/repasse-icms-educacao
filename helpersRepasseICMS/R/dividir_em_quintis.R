dividir_em_quintis <- function(x) {
  
  breaks <- classInt::classIntervals(x, n = 5, style = 'quantile')$brks
  
  case_when(x >= breaks[1] & x < breaks[2] ~ '5',
            x >= breaks[2] & x < breaks[3] ~ '4',
            x >= breaks[3] & x < breaks[4] ~ '3',
            x >= breaks[4] & x < breaks[5] ~ '2',
            x >= breaks[5] & x <= breaks[6] ~ '1')
}