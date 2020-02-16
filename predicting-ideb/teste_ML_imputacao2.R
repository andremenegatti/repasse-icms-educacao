df_imput <- df_municipios %>% 
  filter(ano_calculo %in% c("2015", "2017")) %>% 
  mutate(idosos_sobre_jovens = (faixa_60_a_69 + faixa_70_ou_mais) / (faixa_5_a_9 + faixa_10_a_14)) %>% 
  mutate(pop = pop / 1000,
         taxa_municip = matriculas_rede_municipal / matriculas_rede_publica,
         regiao_metrop = ifelse(is.na(regiao_metrop), 'Nao', regiao_metrop),
         valor_adicionado = valor_adicionado / 1e+6,
         receita_tributaria = receita_tributaria / 1e+6)


df_imput %>% glimpse()


df_imput %>% 
  group_by(ano_calculo) %>% 
  summarise(m = mean(media_ideb_ano),
            m2 = mean(mediana_ideb_ano))

df_imput %>% select(nota_ideb_2015, nota_ideb_2017) %>% summary()

df_imput %>% filter(ano_calculo == "2015") %>%   select(nota_ideb) %>% summary()
df_imput %>% filter(ano_calculo == "2017") %>%   select(nota_ideb) %>% summary()

df_imput_2017 <- df_imput %>% 
  filter(ano_calculo == "2017")

glimpse(df_imput)

df_imput

form <- "nota_ideb_2017 ~ nota_ideb_2015 + matriculas_rede_publica + taxa_municip + IDHM_renda + IDHM_longevidade + IDHM_educacao + 
regiao_metrop + pop_urbana + valor_adicionado + pop + receita_tributaria + receitas_correntes_2017 + idosos_sobre_jovens" 

m_lin <- lm(form, data = df_imput)

summary(m_lin)

library(caret)

set.seed(42)

df_caret <- df_imput_2017 %>% 
  select(nota_ideb_2017, nota_ideb_2015, matriculas_rede_publica, taxa_municip, IDHM_renda, IDHM_longevidade,
         IDHM_educacao, regiao_metrop, pop_urbana, valor_adicionado, pop, receita_tributaria, receitas_correntes_2017, idosos_sobre_jovens) %>% 
  drop_na()


m1 <- train(
  nota_ideb_2017 ~ nota_ideb_2015 + matriculas_rede_publica + taxa_municip + IDHM_renda + IDHM_longevidade + IDHM_educacao + regiao_metrop + pop_urbana + valor_adicionado + pop + receita_tributaria + receitas_correntes_2017 + idosos_sobre_jovens,
  data = df_caret,
  method = 'lm',
  trControl = trainControl(
    method = 'cv',
    number = 15,
    verboseIter = TRUE
  )
)

df_caret2 <- df_imput_2017 %>% 
  select(nota_ideb_2017, nota_ideb_2015, matriculas_rede_publica, taxa_municip, IDHM_renda, IDHM_longevidade,
         IDHM_educacao, regiao_metrop, pop_urbana, valor_adicionado, pop, receita_tributaria, receitas_correntes_2017, idosos_sobre_jovens) %>% 
  drop_na(receitas_correntes_2017, nota_ideb_2017)

X <- df_caret2 %>% 
  select(-nota_ideb_2017) %>% 
  as.data.frame()

m2 <- train(
  x = X,
  y = df_caret2[["nota_ideb_2017"]],
  method = 'lm',
  preProcess = 'medianImpute',
  trControl = trainControl(
    method = 'cv',
    number = 15,
    verboseIter = TRUE
  )
)
m1
m2

m3 <- train(
  
)