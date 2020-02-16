library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

source("salvar_tabela.R")

# Abrindo base
df_municipios <- readRDS('Dados/df_municipios_simplificada.rds')



df_var_transf_gathered <- df_municipios %>% 
  gather(var_transf25_nota:var_transf10_evolucao_e_nota_X_matriculas_municipal, key = 'simulacao', value = 'var_transf') %>% 
  group_by(simulacao) %>% 
  mutate(total_var_transf_positiva = sum(if_else(var_transf >= 0, var_transf, 0)),
         total_var_transf_negativa = sum(if_else(var_transf < 0, var_transf, 0))) %>% 
  ungroup() %>% 
  mutate(simulacao = str_replace(simulacao, 'var_transf', 'sim_'))


df_var_perc_gathered <- df_municipios %>% 
  select(municipio, var_perc_transf25_nota:var_perc_transf10_evolucao_e_nota_X_matriculas_municipal) %>% 
  gather(-municipio, key = 'simulacao', value = 'var_perc') %>% 
  mutate(simulacao = str_replace(simulacao, 'var_perc_transf', 'sim_'))


df_plot <- df_var_transf_gathered %>% 
  left_join(df_var_perc_gathered, by = c('municipio', 'simulacao'))


df_plot <- df_plot %>% 
  mutate(simulacao = factor(simulacao, levels = c('sim_10_nota',
                                                  'sim_10_evolucao_e_nota',
                                                  'sim_10_evolucao_e_nota_X_matriculas_municipal',
                                                  'sim_25_nota',
                                                  'sim_25_evolucao_e_nota',
                                                  'sim_25_evolucao_e_nota_X_matriculas_municipal'),
                            labels = c('Nota, 10%',
                                       'Nota + Evol., 10%',
                                       'Nota + Evol., pond., 10%',
                                       'Nota, 25%',
                                       'Nota + Evol., 25%',
                                       'Nota + Evol., pond., 25%')
  )
  ) %>% 
  mutate(faixa_pop = factor(faixa_pop, levels = c('Mais de 500 mil',
                                                  '250 mil a 500 mil',
                                                  '150 mil a 250 mil',
                                                  '100 mil a 150 mil',
                                                  '50 mil a 100 mil',
                                                  '20 mil a 50 mil',
                                                  'Até 20 mil')))

ggplot(df_plot, aes(x = log(valor_adicionado_2anos), y = var_perc * 100, color = faixa_pop, size = pop)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = 'black', linetype = 'dashed', alpha = 0.5) +
  facet_wrap(~simulacao) +
  scale_color_manual(values = RColorBrewer::brewer.pal(7, 'Spectral'),
                     name = 'Faixa (hab.)') +
  ylab('Variação percentual em relação à cota-parte (%)') +
  xlab('Log. do valor adicionado (biênio 2015-2016)') +
  scale_size_continuous(name = 'População (hab.)', breaks = c(50000, 500000, 10000000), labels = c('50 mil', '500 mil', '10 milhões')) +
  # scale_y_continuous(breaks = c(-100, 0, 100, 200, 300, 400, 500, 600, 700)) +
  # theme(legend.background = element_rect(color = 'gray'))
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  ggtitle('Variação da transferência vs. Log. do valor adicionado')

ggplot(df_plot %>% 
         filter(municipio != "SAO PAULO")
       ,
       aes(x = log(valor_adicionado_2anos), y = var_transf, color = faixa_pop, size = pop)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = 'black', linetype = 'dashed', alpha = 0.5) +
  facet_wrap(~simulacao) +
  scale_color_manual(values = RColorBrewer::brewer.pal(7, 'Spectral'),
                     name = 'Faixa (hab.)') +
  ylab('Variação percentual em relação à cota-parte (%)') +
  xlab('Log. do valor adicionado (biênio 2015-2016)') +
  scale_size_continuous(name = 'População (hab.)', breaks = c(50000, 500000, 10000000), labels = c('50 mil', '500 mil', '10 milhões')) +
  # scale_y_continuous(breaks = c(-100, 0, 100, 200, 300, 400, 500, 600, 700)) +
  # theme(legend.background = element_rect(color = 'gray'))
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  ggtitle('Variação da transferência vs. Log. do valor adicionado')
