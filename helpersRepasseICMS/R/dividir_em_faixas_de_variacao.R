dividir_em_faixas_de_variacao <- function(x) {

  breaks <- c(-Inf, -0.5, -0.3, -0.15, 0, 0.15, 0.3, 0.5, 1, 3, Inf)

  case_when(x > breaks[1] & x < breaks[2] ~ 'Redução superior a 50%',
            x >= breaks[2] & x < breaks[3] ~ 'Redução de 30% a 50%',
            x >= breaks[3] & x < breaks[4] ~ 'Redução de 15% a 30%',
            x >= breaks[4] & x < breaks[5] ~ 'Redução de até 15%',
            x >= breaks[5] & x < breaks[6] ~ 'Aumento de até 15%',
            x >= breaks[6] & x < breaks[7] ~ 'Aumento de 15% a 30%',
            x >= breaks[7] & x < breaks[8] ~ 'Aumento de 30% a 50%',
            x >= breaks[8] & x < breaks[9] ~ 'Aumento de 50% a 100%',
            x >= breaks[9] & x < breaks[10] ~ 'Aumento de 100% a 300%',
            x >= breaks[10] & x < breaks[11] ~ 'Aumento superior a 300%')
}
