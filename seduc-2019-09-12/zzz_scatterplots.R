library(tidyverse)
theme_set(theme_classic())
# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

source("salvar_tabela.R")

# Abrindo base
df_municipios <- readRDS("2019_08_14_simulacao_secretaria/df_municipios_etapa5.rds")


df_municipios <- df_municipios %>% 
  mutate(total_var_transf_positiva = sum(if_else(var_transf_alt3 >= 0, var_transf_alt3, 0)),
         total_var_transf_negativa = sum(if_else(var_transf_alt3 < 0, var_transf_alt3, 0)))


df_plot <- df_municipios %>% 
  mutate(faixa_pop1 = case_when(pop <= 10e+3 ~ 'Até 10 mil',
                               pop > 10e+3 & pop <= 20e+3 ~ '10 mil a 20 mil',
                               pop > 20e+3 & pop <= 50e+3 ~ '20 mil a 50 mil',
                               pop > 50e+3 & pop <= 150e+3 ~ '50 mil a 150 mil',
                               pop > 150e+3 & pop <= 250e+3 ~ '150 mil a 250 mil',
                               pop > 250e+3 & pop <= 500e+3 ~ '250 mil a 500 mil',
                               pop > 500e+3 ~ 'Mais de 500 mil')) %>% 
  mutate(faixa_pop = factor(faixa_pop1, levels = c('Mais de 500 mil',
                                                  '250 mil a 500 mil',
                                                  '150 mil a 250 mil',
                                                  '50 mil a 150 mil',
                                                  '20 mil a 50 mil',
                                                  '10 mil a 20 mil',
                                                  'Até 10 mil')))

# Definicoes dos graficos
scatter_plot <- ggplot(data = df_plot,
                       aes(x = valor_adicionado_2anos / 1e+6, color = faixa_pop, size = pop)) +
  geom_hline(yintercept = 0, color = 'black', linetype = 'dashed', alpha = 0.5) +
  scale_color_manual(values = RColorBrewer::brewer.pal(7, 'Spectral'),
                     name = 'Faixa (hab.)') +
  xlab('Valor adicionado no biênio 2015-2016 (R$, em escala logarítmica)') +
  scale_size_continuous(name = 'População (hab.)', breaks = c(20000, 100000, 500000, 10000000), labels = c('20 mil', '100 mil', '500 mil', '10 milhões')) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000), labels = c('10 milhões', '100 milhões', '1 bilhão', '10 bilhões', '100 bilhões')) +
  theme_classic()
# theme(panel.background = element_rect(fill = 'gray96'))

# Percentual cota-parte 2018
scatter_plot +
  geom_point(aes(y = var_perc_transf_alt3 * 100), alpha = 0.75) +
  ggtitle('Variação percentual da transferência vs. Valor adicionado',
          subtitle = 'Valor adicionado (75%), Educação (18,5%), Receitas Trib. (6%), Área Protegida (0,5%)') +
  scale_y_continuous(name = 'Variação percentual em relação à cota-parte (%)',
                     breaks = seq(-75, 375, by = 50)) +
  coord_cartesian(ylim = c(-100, 400))

ggsave('alt3_var_perc_VS_log_valor_adicionado.png')

# Total
scatter_plot +
  geom_point(aes(y = var_transf_alt3), alpha = 0.5) +
  ggtitle('Variação da transferência vs. Valor adicionado',
          subtitle = 'Valor adicionado (75%), Educação (18,5%), Receitas Trib. (6%), Área Protegida (0,5%)') +
  scale_y_continuous(name = 'Variação da transferência (milhões de R$)')

ggsave('alt3_var_transf_VS_log_valor_adicionado.png')

# Total, sem SP
scatter_plot +
  geom_point(data = df_plot %>%  filter(municipio != 'SAO PAULO'), aes(y = var_transf_alt3), alpha = 0.4) +
  ggtitle('Variação da transferência vs. Valor adicionado - Excluindo São Paulo',
          subtitle = 'Valor adicionado (75%), Educação (18,5%), Receitas Trib. (6%), Área Protegida (0,5%)') +
  scale_y_continuous(name = 'Variação da transferência (milhões de R$)')

ggsave('alt3_var_transf_VS_log_valor_adicionado_SEM_SP.png')

# Perc. RCL 2017
scatter_plot +
  geom_point(data = df_plot %>% filter(municipio != 'POLONI'), aes(y = var_transf_perc_rc2017_secretaria * 100), alpha = 0.75) +
  ggtitle('Variação da transferência (perc. RCL) vs. Valor adicionado',
          subtitle = 'Valor adicionado (75%), Educação (18,5%), Receitas Correntes Líquidas (6%), Área Inundada (0,5%)') +
  scale_y_continuous(name = 'Variação da transferência como perc. da RCL do mun. em 2017 (%)',
                     breaks = seq(-20, 5, by = 5))
ggsave('2019_08_14_simulacao_secretaria/var_perc_rcl2017_VS_log_valor_adicionado_SEM_POLONI.png')
