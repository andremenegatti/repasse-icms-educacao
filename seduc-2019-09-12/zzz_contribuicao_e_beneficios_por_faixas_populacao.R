library(tidyverse)
theme_set(theme_classic())
# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

source("salvar_tabela.R")

# Abrindo base
df_municipios <- readRDS("2019_08_14_simulacao_secretaria/df_municipios_etapa5.rds")


df_municipios <- df_municipios %>% 
  mutate(total_var_transf_positiva = sum(if_else(var_transf_alt1 >= 0, var_transf_alt1, 0)),
         total_var_transf_negativa = sum(if_else(var_transf_alt1 < 0, var_transf_alt1, 0)))


df_plot <- df_municipios %>% 
  mutate(faixa_pop = factor(faixa_pop, levels = c('Mais de 500 mil',
                                                  '250 mil a 500 mil',
                                                  '150 mil a 250 mil',
                                                  '50 mil a 150 mil',
                                                  '20 mil a 50 mil',
                                                  'Até 20 mil')))


#### DISTRIBUICAO CONTRIBUICAO ENTRE FAIXAS POPULACIONAIS #####

# DF com contribuicao bruta e percentual, por faixas populacionais
df2 <- df_plot %>% 
  filter(var_transf_alt1 < 0) %>% 
  group_by(faixa_pop) %>% 
  summarise(total_var_transf_negativa = mean(total_var_transf_negativa),
            contribuicao = sum(var_transf_alt1),
            contrib_perc = contribuicao / total_var_transf_negativa) %>% 
  ungroup()



# Desenhando o grafico
df2 %>% 
  ggplot(aes(x = faixa_pop, fill = faixa_pop, y = -contribuicao)) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(6, 'Spectral'), name = 'Faixa (habitantes)') +
  scale_x_discrete(labels = c('Mais de 500mil', '200mil - 500mil', '150mil - 250mil', '50mil - 150mil', '20mil - 50mil', 'Até 20mil')) +
  ylab('Contribuição para a redistribuição (milhões de R$)') +
  xlab('Faixas populacionais (habitantes)') +
  ggtitle('Contrib. de municípios de diferentes tamanhos para o total redistribuído',
          subtitle = 'Valor adic. (75%), Educ. (18,5%), Receitas Trib. (6%), Área Protegida (0,5%)') +
  theme(axis.text = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        title = element_text(size = 15),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, size = 9, hjust = 1), legend.position = 'none') +
  scale_y_continuous(breaks = seq(0, 700, by = 100)) +
  geom_text(aes(x = faixa_pop, y = -contribuicao + 25, label =  round(contrib_perc * 100, digits = 2) %>% as.character() %>% str_replace('\\.', ',') %>% str_c('%')))

ggsave('alt1_contribuicao_municipios_por_tamanho.png', width = 9, height = 8)


##### REPARTICAO BENEFICIOS #######

# DF com contribuicao bruta e percentual, por faixas populacionais
df3 <- df_plot %>% 
  filter(var_transf_alt1 >= 0) %>% 
  group_by(faixa_pop) %>% 
  summarise(total_var_transf_positiva = mean(total_var_transf_positiva),
            contribuicao = sum(var_transf_alt1),
            contrib_perc = contribuicao / total_var_transf_positiva) %>% 
  ungroup()



# Desenhando o grafico
df3 %>% 
  ggplot(aes(x = faixa_pop, fill = faixa_pop, y = contribuicao)) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(6, 'Spectral'), name = 'Faixa (habitantes)') +
  scale_x_discrete(labels = c('Mais de 500mil', '200mil - 500mil', '150mil - 250mil', '50mil - 150mil', '20mil - 50mil', 'Até 20mil')) +
  ylab('Benefício com a redistribuição (milhões de R$)') +
  xlab('Faixas populacionais (habitantes)') +
  ggtitle('Benefícios da redistribuição - Municípios de diferentes tamanhos',
          subtitle = 'Valor adic. (75%), Educ. (18,5%), Receitas Trib. (6%), Área Protegida (0,5%)') +
  theme(axis.text = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        title = element_text(size = 15),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, size = 9, hjust = 1), legend.position = 'none') +
  scale_y_continuous(breaks = seq(0, 700, by = 100)) +
  geom_text(aes(x = faixa_pop, y = contribuicao + 25, label =  round(contrib_perc * 100, digits = 2) %>% as.character() %>% str_replace('\\.', ',') %>% str_c('%')))

ggsave('alt1_beneficios_municipios_por_tamanho.png', width = 9, height = 10)

####### SP, PEQUENOS E OUTROS ############

# DF com contribuicao bruta e percentual: SP, pequenos e outros
df_grafico_sp_e_pequenos_contrib <-  df_municipios %>% 
  filter(var_transf_alt1 < 0) %>% 
  mutate(sp_pequenos_outros = case_when(municipio == 'SAO PAULO' ~ 'São Paulo',
                                        pop <= 20e+3 ~ 'Menos de 20 mil hab.')) %>% 
  mutate(sp_pequenos_outros = if_else(is.na(sp_pequenos_outros), 'Outros', sp_pequenos_outros)) %>% 
  mutate(sp_pequenos_outros = factor(sp_pequenos_outros, levels = c('São Paulo', 'Menos de 20 mil hab.', 'Outros'))) %>% 
  group_by(sp_pequenos_outros) %>% 
  summarise(total_var_transf_negativa = mean(total_var_transf_negativa),
            contribuicao = sum(var_transf_alt1),
            contrib_perc = contribuicao / total_var_transf_negativa)

df_grafico_sp_e_pequenos_benef <-  df_municipios %>% 
  filter(var_transf_alt1 >= 0) %>% 
  mutate(sp_pequenos_outros = case_when(municipio == 'SAO PAULO' ~ 'São Paulo',
                                        pop <= 20e+3 ~ 'Menos de 20 mil hab.')) %>% 
  mutate(sp_pequenos_outros = if_else(is.na(sp_pequenos_outros), 'Outros', sp_pequenos_outros)) %>%
  mutate(sp_pequenos_outros = factor(sp_pequenos_outros, levels = c('São Paulo', 'Menos de 20 mil hab.', 'Outros'))) %>% 
  group_by(sp_pequenos_outros) %>% 
  summarise(total_var_transf_positiva = mean(total_var_transf_positiva),
            contribuicao = sum(var_transf_alt1),
            contrib_perc = contribuicao / total_var_transf_positiva) %>% 
  ungroup()



df_grafico_sp_e_pequenos_contrib %>% 
  ggplot(aes(x = sp_pequenos_outros, fill = sp_pequenos_outros, y = -contribuicao)) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Spectral'), name = '') +
  ylab('Contribuição para a redistribuição (milhões de R$)') +
  xlab('Regime de distribuição') +
  ggtitle('Contribuição de municípios de diferentes tamanhos para o total redistribuído', subtitle = 'Comparação entre diferentes regimes de distribuição') +
  theme(axis.text = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        title = element_text(size = 15),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(breaks = seq(0, 3500, by = 500))
# geom_text(aes(x = simulacao, y = -total_var_transf_negativa + 90, label =  -round(total_var_transf_negativa)))



df_grafico_sp_e_pequenos_benef %>% 
  ggplot(aes(x = sp_pequenos_outros, fill = sp_pequenos_outros, y = contribuicao)) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Spectral'), name = '') +
  ylab('Contribuição para a redistribuição (milhões de R$)') +
  xlab('Regime de distribuição') +
  ggtitle('Contribuição de municípios de diferentes tamanhos para o total redistribuído', subtitle = 'Comparação entre diferentes regimes de distribuição') +
  theme(axis.text = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        title = element_text(size = 15),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank())
# scale_y_continuous(breaks = seq(0, 3500, by = 500)) 
# geom_text(aes(x = simulacao, y = -total_var_transf_negativa + 90, label =  -round(total_var_transf_negativa)))

