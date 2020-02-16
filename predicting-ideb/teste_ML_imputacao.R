df_ml <- df_municipios %>% 
  mutate(idosos_sobre_jovens = (faixa_60_a_69 + faixa_70_ou_mais) / (faixa_5_a_9 + faixa_10_a_14)) %>% 
  mutate(pop = pop / 1000,
         taxa_municip = matriculas_rede_municipal / matriculas_rede_publica,
         regiao_metrop = ifelse(is.na(regiao_metrop), 'Nao', regiao_metrop),
         valor_adicionado = valor_adicionado / 1e+6,
         receita_tributaria = receita_tributaria / 1e+6,
         area_cultivada = area_cultivada / 100)

df_ml2 <- df_ml %>% 
  select(-ano_distribuicao, -codigo, -ind_participacao, -total_icms_estado, -montante_disponivel,
         -total_recebido_municipio, -valor_adicionado_2anos,
         -ind_participacao_calculado, - Codmun7, - media_ideb_ano, nota_ideb_imp, -mediana_ideb_ano,
         -nota_ideb_imp_mediana) %>% 
  select(-starts_with("share_"))

df_ml2 %>% glimpse()

df_ml3 <- df_ml2 %>% 
  filter(ano_calculo %in% c("2015", "2017"))


df_ml4 <- df_ml3 %>% 
  select(-nota_ideb_2015, -nota_ideb_2017,
         -receitas_correntes_2018) %>% 
  drop_na() %>% 
  mutate_if(.predicate = is.character, .funs = as.factor)

df_ml4 %>% glimpse()


df_shuffle <- df_ml4[sample(nrow(df_ml4)), ]                 

cut <- round(nrow(df_shuffle) * 0.75)

train_data <- df_shuffle[1:cut, ]
test_data <- df_shuffle[(cut + 1):nrow(df_shuffle),]

library(ranger)

tuneGrid <- data.frame(
  .mtry = c(2, 3, 7, 10, 15, 20),
  .splitrule = "variance",
  .min.node.size = 5
)

m_rf <- train(
  nota_ideb ~ .,
  tuneLength = 3,
  data = train_data,
  tuneGrid = tuneGrid,
  method = 'ranger',
  trControl = trainControl(
    method = 'cv',
    number = 10,
    verboseIter = TRUE
  )
)

m_rf

predicted <- predict(m_rf, newdata = test_data)

library(Metrics)

Metrics::mae(actual = test_data$nota_ideb, predicted = predicted)

test_data %>% 
  mutate(fitted = predicted) %>% 
  select(nota_ideb, nota_ideb_imp, fitted) %>% View()

df_fitted <- df_shuffle %>% 
  mutate(fitted = predict(m_rf, df_shuffle)) %>% 
  filter(ano_calculo == "2017") %>% 
  mutate(municipio = as.character(municipio)) %>% 
  arrange(municipio)

df_municipios2017 <- df_municipios2017 %>%
  arrange(municipio)

df_municipios2017$municipio
df_fitted$municipio %>% as.character()

