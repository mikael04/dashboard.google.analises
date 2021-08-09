## code to prepare `dimension` dataset goes here
library(dplyr)

debug <- F
## Lendo arquivo de banco de dados  ---------------------------------
## Setup, lendo a base de dados (pode ser usado qualquer outro formato)
df_dimensions <- fst::read_fst("dados/dimensions_compressed.fst")
df_dimensions <- tibble::as_tibble(df_dimensions)

## Tabelas ---------------------------------

#Selecionando campos que serão usados nos filtros e gráficos
df_dimensions_filter <- df_dimensions  %>%
  dplyr::select(id, authors_fn = authors,  authors_ln =  `authors/lastname`, metrics.times_cited, altmetrics.score, date_normal, type, title.preferred, abstract.preferred,
                research_org_country_names, categories.for_v1.first_level.codes)

df_dimensions_filter <- tibble::as_tibble(df_dimensions_filter)

## Por enquanto apenas data e tipo
df_dimensions_filter_type_date <- df_dimensions_filter %>%
  dplyr::filter(date_normal < lubridate::ymd("2021-05-24")) %>% ##data da última extração
  dplyr::filter(date_normal > "2020-01-01") %>% ##não pegar arquivos que possuem apenas ano para não distorcer gráfico
  dplyr::mutate(date = lubridate::floor_date(date_normal, "month")) %>%
  dplyr::select(-date_normal) %>%
  dplyr::group_by(type, date) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup()