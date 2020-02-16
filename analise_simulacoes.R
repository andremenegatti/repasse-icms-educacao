library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

# Funcao para salvar tabelas de resultados
source("salvar_tabela.R")

#### ABRINDO BASES ####
df_municipios <- readRDS('Dados/df_municipios_etapa5.rds')

#### CONTAGEM DE MUNICIPIOS EM CADA SITUCAO ####

# Quantos municipios ganham e quantos perdem em cada modelo
df_municipios %>% 
  select(municipio, starts_with("var_perc_transf")) %>% 
  gather(-municipio, key = 'simulacao', value = 'var_perc') %>% 
  mutate(simulacao = str_remove(simulacao, 'var_perc_sim_')) %>% 
  group_by(simulacao) %>% 
  summarise(n_aumenta = sum(if_else(var_perc > 0, 1, 0)),
            n_diminui = sum(if_else(var_perc < 0, 1, 0))) %>% salvar_tabela(file = 'contagem_ganha_perde.csv')

# Funcao para contar ocorrencias dentro de uma faixa de valores
source("contar_ocorrencias_na_faixa.R")
# contar_ocorrencias_na_faixa <- function(var, lower, upper) {sum(if_else(var > lower & var <= upper, 1, 0))}

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
  mutate(legenda = c('Evolução IDEB - 10%',
                     'Evolução IDEB + Nota IDEB - 10%',
                     'Evolução + Nota, pond. pelo log. de matrículas (rede municipal) - 10%',
                     'Evolução + Nota, pond. pelo log. de matrículas (rede pública) - 10%',
                     'Evolução + Nota, pond. por matrículas (rede municipal) - 10%',
                     'Evolução + Nota, pond. por matrículas (rede pública) - 10%',
                     'Evolução IDEB pond. pelo log. de matrículas (rede municipal) - 10%',
                     'Evolução IDEB pond. pelo log. de matrículas (rede pública) - 10%',
                     'Evolução IDEB pond. por matrículas (rede municipal) - 10%',
                     'Evolução IDEB pond. por matrículas (rede pública) - 10%',
                     'Nota IDEB - 10%',
                     'Nota IDEB pond. pelo log. de matrículas (rede municipal) - 10%',
                     'Nota IDEB pond. pelo log. de matrículas (rede pública) - 10%',
                     'Nota IDEB pond. por matrículas (rede municipal) - 10%',
                     'Nota IDEB pond. por matrículas (rede pública) - 10%',
                     'Evolução IDEB - 25%',
                     'Evolução IDEB + Nota IDEB - 25%',
                     'Evolução + Nota, pond. pelo log. de matrículas (rede municipal) - 25%',
                     'Evolução + Nota, pond. pelo log. de matrículas (rede pública) - 25%',
                     'Evolução + Nota, pond. pelo número de matrículas (rede municipal) - 25%',
                     'Evolução + Nota, pond. pelo número de matrículas (rede pública) - 25%',
                     'Evolução IDEB pond. pelo log. de matrículas (rede municipal) - 25%',
                     'Evolução IDEB pond. pelo log. de matrículas (rede pública) - 25%',
                     'Evolução IDEB pond. por matrículas (rede municipal) - 25%',
                     'Evolução IDEB pond. por matrículas (rede pública) - 25%',
                     'Nota IDEB - 25%',
                     'Nota IDEB pond. pelo log. de matrículas (rede municipal) - 25%',
                     'Nota IDEB pond. pelo log. de matrículas (rede pública) - 25%',
                     'Nota IDEB pond. por matrículas (rede municipal) - 25%',
                     'Nota IDEB pond. por matrículas (rede pública) - 25%'
  ))

df_contagem_faixas # %>% salvar_tabela(file = 'contagem_ganha_perde_faixas.csv')

# df_contagem_faixas %>% select(simulacao, legenda) %>% rename(variavel = simulacao) %>% salvar_tabela(file = 'legendas.csv')
# df_contagem_faixas %>% select(simulacao, legenda) %>% rename(variavel = simulacao) %>% saveRDS('legendas.rds')

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


# Contagem
df_nested_graficos <- df_contagem_faixas %>% 
  nest(-simulacao, -legenda) %>% 
  mutate(plots = map2(.x = data, .y = legenda,
                       .f = ~ .x %>% 
                         gather(key = 'Faixa', value = 'numero_municipios') %>%
                         mutate(Faixa = factor(Faixa, levels = names(df_contagem_faixas)[2:length(df_contagem_faixas)])) %>% 
                         ggplot() + 
                         geom_col(aes(x = Faixa, y = numero_municipios, fill = Faixa)) +
                         scale_fill_manual(values = div_palette, labels = my_labels) +
                         ylab('Número de municípios') +
                         geom_text(aes(label = numero_municipios, y = numero_municipios + 4.5, x = Faixa)) +
                         theme(axis.text.x = element_blank(),
                               panel.grid.major.x = element_blank()) +
                         ggtitle('Municípios por faixas de variação da transferência', subtitle = .y)))

# walk2(.x = df_nested_graficos$plots, .y = df_nested_graficos$simulacao, .f = ~ ggsave(plot = .x, filename = str_c('Resultados2/plot_', .y, '.png')))


# Proporção
df_contagem_faixas %>% 
  select(simulacao, legenda, ganha_mais_de_300:perde_mais_de_50) %>% 
  gather(-simulacao, -legenda, key = 'Faixa', value = 'numero_municipios') %>% 
  mutate(Faixa = factor(Faixa, levels = names(df_contagem_faixas)[2:length(df_contagem_faixas)])) %>%
  mutate(simulacao = str_replace(simulacao, 'var_perc_transf', 'sim_')) %>% 
  filter(simulacao %in% c("sim_10_evolucao",
                          "sim_10_nota",
                          "sim_10_nota_X_matriculas_municipal",
                          "sim_25_evolucao",
                          "sim_25_nota",
                          "sim_25_nota_X_matriculas_municipal"
                          )
         ) %>% 
  mutate(legenda = factor(legenda, levels = c('Evolução IDEB - 10%',
                                              'Nota IDEB - 10%',
                                              'Evolução IDEB - 25%',
                                              'Nota IDEB - 25%',
                                              'Nota IDEB pond. por matrículas (rede municipal) - 10%',
                                              'Nota IDEB pond. por matrículas (rede municipal) - 25%'
                                              ),
                          labels = c("Evolução 10%",
                                     "Nota 10%",
                                     "Evolução 25%",
                                     "Nota 25%",
                                     "Nota pond. 10%",
                                     "Nota pond. 25%")
                          )
         ) %>%
  ggplot() +
  geom_col(aes(x = legenda, y = numero_municipios, fill = Faixa)) +
  scale_fill_manual(values = div_palette, labels = my_labels) +
  ylab('Número de municípios') +
  xlab('Modelo de distribuição') +
  ggtitle('Distribuição dos municípios em faixas de variação da transferência', subtitle = 'Comparação entre diferentes modelos de distribuição')