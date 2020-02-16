library(tidyverse)
theme_set(theme_bw())

source("salvar_tabela.R")

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

# Abrindo base
df_municipios <- readRDS('Dados/df_municipios_simplificada.rds')


df_var_perc_gathered <- df_municipios %>% 
  gather(var_perc_transf25_nota:var_perc_transf10_evolucao_e_nota_X_matriculas_municipal, key = 'simulacao', value = 'var_perc_transf')


# Funcao para contar ocorrencias dentro de uma faixa de valores
source("contar_ocorrencias_na_faixa.R")


# Contagem de municipios em faixas de ganhos e perdas
df_contagem_faixas <- df_municipios %>% 
  select(municipio, starts_with("var_perc_transf")) %>% 
  gather(-municipio, key = 'simulacao', value = 'var_perc') %>% 
  mutate(simulacao = str_remove(simulacao, 'var_perc_sim_')) %>% 
  group_by(simulacao) %>% 
  summarise(
    ganha_mais_de_300 = contar_ocorrencias_na_faixa(var_perc, 3, Inf),
    # ganha_entre_200_e_500 = contar_ocorrencias_na_faixa(var_perc, 2, 5),
    ganha_entre_100_e_300 = contar_ocorrencias_na_faixa(var_perc, 1, 3),
    ganha_entre_50_e_100 = contar_ocorrencias_na_faixa(var_perc, 0.5, 1),
    ganha_entre_30_e_50 = contar_ocorrencias_na_faixa(var_perc, 0.3, 0.5),
    ganha_entre_15_e_30 = contar_ocorrencias_na_faixa(var_perc, 0.15, 0.3),
    ganha_entre_0_e_15 = contar_ocorrencias_na_faixa(var_perc, 0, 0.15),
    perde_entre_0_e_15 = contar_ocorrencias_na_faixa(var_perc, -0.15, 0),
    perde_entre_15_e_30 = contar_ocorrencias_na_faixa(var_perc, -0.3, -0.15),
    perde_entre_30_e_50 = contar_ocorrencias_na_faixa(var_perc, -0.5, -0.3),
    perde_mais_de_50 = contar_ocorrencias_na_faixa(var_perc, -Inf, -0.5)
  ) %>% 
  mutate(legenda = c('Nota + Evolução, 10%',
                     'Nota + Evolução, pond. por matrículas, 10%',
                     'Nota, 10%',
                     'Nota + Evolução, 25%',
                     'Nota + Evolução, pond. por matrículas, 25%',
                     'Nota, 25%'
  ))

df_contagem_faixas # %>% salvar_tabela(file = 'contagem_ganha_perde_faixas.csv')



# Graficos de barras mostrando contagem de municipios em cada faixa de ganho/perda
source("div_palette_full.R")
div_palette <- div_palette_full()

my_labels <- c('Aumento superior a 300%',
               'Aumento de 100% a 300%',
               'Aumento de 50% a 100%',
               'Aumento de 30% a 50%',
               'Aumento de 15% a 30%',
               'Aumento de até 15%',
               'Redução de até 15%',
               'Redução de 15% a 30%',
               'Redução de 30% a 50%',
               'Redução superior a 50%')





# Proporção
df_contagem_faixas %>% 
  select(simulacao, legenda, ganha_mais_de_300:perde_mais_de_50) %>% 
  gather(-simulacao, -legenda, key = 'Faixa', value = 'numero_municipios') %>% 
  mutate(Faixa = factor(Faixa, levels = names(df_contagem_faixas)[2:length(df_contagem_faixas)])) %>%
  mutate(simulacao = str_replace(simulacao, 'var_perc_transf', 'sim_')) %>% 
  filter(simulacao %in% c("sim_10_nota",
                          "sim_10_evolucao_e_nota",
                          "sim_10_evolucao_e_nota_X_matriculas_municipal",
                          "sim_25_nota",
                          "sim_25_evolucao_e_nota",
                          "sim_25_evolucao_e_nota_X_matriculas_municipal"
                          )
  ) %>% 
  mutate(legenda = factor(legenda, levels = c('Nota, 10%',
                                              'Nota + Evolução, 10%',
                                              'Nota + Evolução, pond. por matrículas, 10%',
                                              'Nota, 25%',
                                              'Nota + Evolução, 25%',
                                              'Nota + Evolução, pond. por matrículas, 25%'
                                              ),
                          labels = c('Nota, 10%',
                                     'Nota + Evolução, 10%',
                                     'Nota + Evolução, pond., 10%',
                                     'Nota, 25%',
                                     'Nota + Evolução, 25%',
                                     'Nota + Evolução, pond., 25%'
                          )
                          )
  ) %>%
  ggplot() +
  geom_col(aes(x = legenda, y = numero_municipios, fill = Faixa)) +
  scale_fill_manual(values = div_palette, labels = my_labels) +
  scale_y_continuous(breaks = c(seq(0, 600, by = 50), 645)) +
  ylab('Número de municípios') +
  xlab('Regime de distribuição') +
  ggtitle('Distribuição dos municípios em faixas de variação da transferência', subtitle = 'Comparação entre diferentes regimes de distribuição') +
  theme(axis.text = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        title = element_text(size = 15))
