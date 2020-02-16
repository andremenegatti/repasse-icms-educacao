library(tidyverse)
theme_set(theme_bw())

# Mudando diretorio de trabalho
setwd('C:/Users/Dell/Desktop/Estudo ICMS')

source("salvar_tabela.R")

# Abrindo base
df_municipios <- readRDS('Dados/df_municipios_simplificada.rds')

# Montante disponível, em milhões de reais
df_municipios$montante_disponivel %>% mean()

# Checando se em todos os regimes de distribuicao o total distribuido foi identico
df_municipios %>% 
  select(total_recebido_municipio,
         transf25_nota, transf25_evolucao_e_nota, transf25_evolucao_e_nota_X_matriculas_municipal,
         transf10_nota, transf10_evolucao_e_nota, transf10_evolucao_e_nota_X_matriculas_municipal) %>% 
  as.matrix() %>% 
  apply(FUN = sum, MARGIN = 2)

# Gather na variavel de transferencia bruta e criando variavel com soma dos beneficios e contribuicoes (devem ser iguais)
df_var_transf_gathered <- df_municipios %>% 
  gather(var_transf25_nota:var_transf10_evolucao_e_nota_X_matriculas_municipal, key = 'simulacao', value = 'var_transf') %>% 
  group_by(simulacao) %>% 
  mutate(total_var_transf_positiva = sum(if_else(var_transf >= 0, var_transf, 0)),
         total_var_transf_negativa = sum(if_else(var_transf < 0, var_transf, 0))) %>% 
  ungroup() %>% 
  mutate(simulacao = factor(simulacao, levels = c('var_transf10_nota',
                                                  'var_transf10_evolucao_e_nota',
                                                  'var_transf10_evolucao_e_nota_X_matriculas_municipal',
                                                  'var_transf25_nota',
                                                  'var_transf25_evolucao_e_nota',
                                                  'var_transf25_evolucao_e_nota_X_matriculas_municipal'),
                            labels = c('Nota, 10%',
                                       'Nota + Evol., 10%',
                                       'Nota + Evol., pond., 10%',
                                       'Nota, 25%',
                                       'Nota + Evol., 25%',
                                       'Nota + Evol., pond. 25%')
  )
  ) %>% 
  mutate(faixa_pop = factor(faixa_pop, levels = c('Mais de 500 mil',
                                                  '250 mil a 500 mil',
                                                  '150 mil a 250 mil',
                                                  '100 mil a 150 mil',
                                                  '50 mil a 100 mil',
                                                  '20 mil a 50 mil',
                                                  'Até 20 mil')))

# Checando se 'perdas' e 'ganhos' brutos sao iguais em todos os regimes
df_var_transf_gathered %>% 
  group_by(simulacao) %>% 
  summarise(total_var_transf_positiva = sum(if_else(var_transf >= 0, var_transf, 0)),
            total_var_transf_negativa = sum(if_else(var_transf < 0, var_transf, 0)))


# DF com contribuicao bruta e percentual, por faixas populacionais
df2 <- df_var_transf_gathered %>% 
  filter(var_transf < 0) %>% 
  group_by(simulacao, faixa_pop) %>% 
  summarise(total_var_transf_negativa = mean(total_var_transf_negativa),
            contribuicao = sum(var_transf),
            contrib_perc = contribuicao / total_var_transf_negativa) %>% 
  ungroup()

# Ajustes de nomes de variaveis para inclusao no grafico
df_grafico <- df2 %>% 
  mutate(simulacao = factor(simulacao, levels = c('var_transf10_nota',
                                                  'var_transf10_evolucao_e_nota',
                                                  'var_transf10_evolucao_e_nota_X_matriculas_municipal',
                                                  'var_transf25_nota',
                                                  'var_transf25_evolucao_e_nota',
                                                  'var_transf25_evolucao_e_nota_X_matriculas_municipal'),
                            labels = c('Nota, 10%',
                                       'Nota + Evol., 10%',
                                       'Nota + Evol., pond., 10%',
                                       'Nota, 25%',
                                       'Nota + Evol., 25%',
                                       'Nota + Evol., pond. 25%')
  )
  ) %>% 
  mutate(faixa_pop = factor(faixa_pop, levels = c('Mais de 500 mil',
                                                  '250 mil a 500 mil',
                                                  '150 mil a 250 mil',
                                                  '100 mil a 150 mil',
                                                  '50 mil a 100 mil',
                                                  '20 mil a 50 mil',
                                                  'Até 20 mil')))


# Desenhando o grafico
df_grafico %>% 
ggplot(aes(x = simulacao, fill = faixa_pop, y = -contribuicao)) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(7, 'Spectral'), name = 'Faixa (habitantes)') +
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
  scale_y_continuous(breaks = seq(0, 3500, by = 500)) +
  geom_text(aes(x = simulacao, y = -total_var_transf_negativa + 90, label =  -round(total_var_transf_negativa)))



# DF com contribuicao bruta e percentual: SP, pequenos e outros
df_grafico_sp_e_pequenos_contrib <-  df_var_transf_gathered %>% 
  filter(var_transf < 0) %>% 
  mutate(sp_pequenos_outros = case_when(municipio == 'SAO PAULO' ~ 'São Paulo',
                                        pop <= 20e+3 ~ 'Menos de 20 mil hab.')) %>% 
  mutate(sp_pequenos_outros = if_else(is.na(sp_pequenos_outros), 'Outros', sp_pequenos_outros)) %>% 
  mutate(sp_pequenos_outros = factor(sp_pequenos_outros, levels = c('São Paulo', 'Menos de 20 mil hab.', 'Outros'))) %>% 
  group_by(simulacao, sp_pequenos_outros) %>% 
  summarise(total_var_transf_negativa = mean(total_var_transf_negativa),
            contribuicao = sum(var_transf),
            contrib_perc = contribuicao / total_var_transf_negativa) %>% 
  ungroup()

df_grafico_sp_e_pequenos_benef <-  df_var_transf_gathered %>% 
  filter(var_transf >= 0) %>% 
  mutate(sp_pequenos_outros = case_when(municipio == 'SAO PAULO' ~ 'São Paulo',
                                        pop <= 20e+3 ~ 'Menos de 20 mil hab.')) %>% 
  mutate(sp_pequenos_outros = if_else(is.na(sp_pequenos_outros), 'Outros', sp_pequenos_outros)) %>%
  mutate(sp_pequenos_outros = factor(sp_pequenos_outros, levels = c('São Paulo', 'Menos de 20 mil hab.', 'Outros'))) %>% 
  group_by(simulacao, sp_pequenos_outros) %>% 
  summarise(total_var_transf_negativa = mean(total_var_transf_negativa),
            contribuicao = sum(var_transf),
            contrib_perc = contribuicao / total_var_transf_negativa) %>% 
  ungroup()



df_grafico_sp_e_pequenos_contrib %>% 
  ggplot(aes(x = simulacao, fill = sp_pequenos_outros, y = -contribuicao)) +
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
  scale_y_continuous(breaks = seq(0, 3500, by = 500)) +
  geom_text(aes(x = simulacao, y = -total_var_transf_negativa + 90, label =  -round(total_var_transf_negativa)))



df_grafico_sp_e_pequenos_benef %>% 
  ggplot(aes(x = simulacao, fill = sp_pequenos_outros, y = contribuicao)) +
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
  scale_y_continuous(breaks = seq(0, 3500, by = 500)) +
  geom_text(aes(x = simulacao, y = -total_var_transf_negativa + 90, label =  -round(total_var_transf_negativa)))

