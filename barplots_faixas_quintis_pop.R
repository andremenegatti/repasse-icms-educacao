library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

# Abrindo base
df_municipios <- readRDS('df_municipios_etapa5.rds')

# Carregando funcao para dividir variacoes percentuais em faixas
source("dividir_em_faixas_de_variacao.R")

# Vetor com identificacao dos modelos que sera usado no subtitulo dos graficos
vetor_legendas <- c('Evolu��o IDEB - 10%',
                    'Evolu��o IDEB + Nota IDEB - 10%',
                    'Evolu��o + Nota, pond. pelo log. de matr�culas (rede municipal) - 10%',
                    'Evolu��o + Nota, pond. pelo log. de matr�culas (rede p�blica) - 10%',
                    'Evolu��o + Nota, pond. por matr�culas (rede municipal) - 10%',
                    'Evolu��o + Nota, pond. por matr�culas (rede p�blica) - 10%',
                    'Evolu��o IDEB pond. pelo log. de matr�culas (rede municipal) - 10%',
                    'Evolu��o IDEB pond. pelo log. de matr�culas (rede p�blica) - 10%',
                    'Evolu��o IDEB pond. por matr�culas (rede municipal) - 10%',
                    'Evolu��o IDEB pond. por matr�culas (rede p�blica) - 10%',
                    'Nota IDEB - 10%',
                    'Nota IDEB pond. pelo log. de matr�culas (rede municipal) - 10%',
                    'Nota IDEB pond. pelo log. de matr�culas (rede p�blica) - 10%',
                    'Nota IDEB pond. por matr�culas (rede municipal) - 10%',
                    'Nota IDEB pond. por matr�culas (rede p�blica) - 10%',
                    'Evolu��o IDEB - 25%',
                    'Evolu��o IDEB + Nota IDEB - 25%',
                    'Evolu��o + Nota, pond. pelo log. de matr�culas (rede municipal) - 25%',
                    'Evolu��o + Nota, pond. pelo log. de matr�culas (rede p�blica) - 25%',
                    'Evolu��o + Nota, pond. pelo n�mero de matr�culas (rede municipal) - 25%',
                    'Evolu��o + Nota, pond. pelo n�mero de matr�culas (rede p�blica) - 25%',
                    'Evolu��o IDEB pond. pelo log. de matr�culas (rede municipal) - 25%',
                    'Evolu��o IDEB pond. pelo log. de matr�culas (rede p�blica) - 25%',
                    'Evolu��o IDEB pond. por matr�culas (rede municipal) - 25%',
                    'Evolu��o IDEB pond. por matr�culas (rede p�blica) - 25%',
                    'Nota IDEB - 25%',
                    'Nota IDEB pond. pelo log. de matr�culas (rede municipal) - 25%',
                    'Nota IDEB pond. pelo log. de matr�culas (rede p�blica) - 25%',
                    'Nota IDEB pond. por matr�culas (rede municipal) - 25%',
                    'Nota IDEB pond. por matr�culas (rede p�blica) - 25%'
                    )

# Definindo vetor de cores
source("div_palette_full.R")
div_palette <- div_palette_full()

# Vetor com legenda das cores
my_labels <- c('Aumento superior a 300%',
               'Aumento de 100% a 300%',
               'Aumento de 50% a 100%',
               'Aumento de 30% a 50%',
               'Aumento de 15% a 30%',
               'Aumento de at� 15%',
               'Redu��o de at� 15%',
               'Redu��o de 15% a 30%',
               'Redu��o de 30% a 50%',
               'Redu��o superior a 50%')

# Dataframe com zero para preencher casos faltantes no 'summarise'
zero_tibble <- tibble(Faixa = my_labels, numero_municipios = 0)

# Preparando dados para os graficos
df_nested <- df_municipios %>% 
  gather(var_perc_transf10_evolucao:var_perc_transf25_nota_X_matriculas_publica, key = 'criterio', value = 'var_perc_transf') %>% 
  nest(-criterio) %>% 
  mutate(legenda = vetor_legendas) %>% 
  mutate(data = map(.x = data,
                    .f = ~ .x %>% 
                      mutate(Faixa = dividir_em_faixas_de_variacao(var_perc_transf)) %>% 
                      nest(-quintil_pop))
         ) %>% 
  mutate(data = map(.x = data,
                    .f = ~ .x %>% mutate(counts = map(.x = data,
                                                      .f = ~ .x %>% 
                                                        group_by(Faixa) %>% 
                                                        summarise(numero_municipios = n())
                                                      )
                                         )
                    )
         ) %>% 
  unnest() %>% 
  mutate(counts = map(.x = counts,
                      .f = ~ zero_tibble %>% 
                        filter(!Faixa %in% .x$Faixa) %>% 
                        bind_rows(.x))) %>% 
  mutate(legenda = str_c(legenda, '- Quintil pop. ', quintil_pop)) 


# Criando graficos
df_nested_graficos <- df_nested %>%
  mutate(plots = map2(.x = counts, .y = legenda,
                      .f = ~ .x %>% 
                        mutate(Faixa = factor(Faixa, levels = my_labels)) %>% 
                        ggplot() + 
                        geom_col(aes(x = Faixa, y = numero_municipios, fill = Faixa)) +
                        scale_fill_manual(values = div_palette,
                                          labels = my_labels) +
                        ylab('N�mero de munic�pios') +
                        geom_text(aes(label = numero_municipios, y = numero_municipios + 4.5, x = Faixa)) +
                        theme(axis.text.x = element_blank(),
                              panel.grid.major.x = element_blank()) +
                        ggtitle('Munic�pios por faixas de varia��o da transfer�ncia', subtitle = .y)))

# Salvando graficos
df_nested_graficos <- df_nested_graficos %>% 
  mutate(filenames = str_c('Resultados2/Quintis_pop/plot_', criterio, '__quintil_pop', quintil_pop, '.png'))

walk2(.x = df_nested_graficos$plots, .y = df_nested_graficos$filenames,
      .f = ~ ggsave(plot = .x, filename = .y ))
