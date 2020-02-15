
# Fonte: https://towardsdatascience.com/an-r-package-to-explore-the-novel-coronavirus-590055738ad6

# Instalar o pacote nCov2019

remotes::install_github("GuangchuangYu/nCov2019")


# 1. Carregar as bibliotecas ----
library(tidyverse)
library(lubridate)

# Carregar a biblioteca com os dados atualizados do coronavírus
library(nCov2019)


# Carregar o resumo do histórico de casos na China 
historico <- load_nCov2019(lang = "en")


# 2. Examinar os dados ----
summary(historico) %>% 
    glimpse()

# Somente as colunas de 1:5 e 8:10 estão traduzidas
summary(historico)[ , c(1:5, 8:10)]

# Checando os valores das colunas 
summary(historico)[ , c(1:5, 8:10)] %>% 
    count(confirm, sort = TRUE)

# Checando as classes
summary(historico)[ , c(1:5, 8:10)] %>% 
    glimpse()


# 3. Manipulação dos dados ----

# Convertendo as classes, renomeando e organizando as colunas, e substituindo os NAs por 0 
historico_tbl <- summary(historico)[ , c(1:5, 8:10)] %>% 
    as_tibble() %>% 
    mutate(acum_morte = as.integer(cum_dead),
           confirmado = as.integer(confirm),
           curado = as.integer(heal)) %>% 
    rename(provincia = province, 
           data = time,
           acum_confirmado = cum_confirm,
           acum_curado = cum_heal,
           morte = dead) %>%
    select(provincia, data, starts_with("acum"), starts_with("c"),everything()) %>% 
    select(-cum_dead, -confirm, -heal)

# Separando dia e mês  
historico_transp_tbl <- historico_tbl %>%
    as_tibble() %>% 
    mutate(mes = month(data, label = TRUE)) %>% 
    mutate(dia = day(data) %>% as.character()) %>% 
    mutate(dia = case_when(nchar(dia) == 1 ~ str_c("0", dia), TRUE ~ dia)) %>% 
    unite(mes_dia, c(mes, dia), sep = "-", remove = TRUE) %>%
    arrange(mes_dia) %>% 
    
# Transpondo o dataframe para usar os dados no Flourish
    select(provincia, mes_dia, acum_confirmado) %>% 
    spread(mes_dia, acum_confirmado) %>% 
    replace(is.na(.), 0) %>% 
    select(provincia, starts_with("jan"), everything())
   
# 4. Salvar ----  
historico_transp_tbl %>% write_csv("data/historico_transp.csv")
