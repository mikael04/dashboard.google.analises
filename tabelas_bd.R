## code to prepare `dimension` dataset goes here
library(dplyr)

debug <- F
## Lendo arquivo de banco de dados  ---------------------------------
## Setup, lendo a base de dados (pode ser usado qualquer outro formato)
df_dimensions <- fst::read_fst("dados/dimensions_compressed.fst")
df_dimensions <- tibble::as_tibble(df_dimensions)

## Variáveis selecionadas ---------------------------------
#Selecionando campos que serão usados nos filtros e gráficos
df_dimensions_filter <- df_dimensions  %>%
  dplyr::select(id, authors_fn = authors,  authors_ln =  `authors/lastname`, metrics.times_cited, altmetrics.score, date_normal, type, title.preferred, abstract.preferred,
                research_org_country_names, categories.for_v1.first_level.codes)

df_dimensions_filter <- tibble::as_tibble(df_dimensions_filter)

## Samples para testes  ---------------------------------
# parametro usado para o seed
set.seed(424242)
df_dimensions_filter_sample <- df_dimensions_filter %>%
  dplyr::slice_sample(n = 40000)
set.seed(424242)
df_dimensions_filter_sample_min <- df_dimensions_filter_sample %>%
  dplyr::slice_sample(n = 40)

## Tabelas de data e tipo  ---------------------------------
df_dimensions_filter_type_date <- df_dimensions_filter %>%
  dplyr::filter(date_normal < lubridate::ymd("2021-05-24")) %>% ##data da última extração
  dplyr::filter(date_normal > "2020-01-01") %>% ##não pegar arquivos que possuem apenas ano para não distorcer gráfico
  dplyr::mutate(date = lubridate::floor_date(date_normal, "month")) %>%
  dplyr::select(-date_normal) %>%
  dplyr::group_by(type, date) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup()


## Outra forma de separar a primeira palavra
# authors_ln <- df_dimensions_filter_sample$authors_ln
# authors_ln_mod <- lapply(authors_ln, function(x) stringr::str_split_fixed(x, pattern = "\\|", 2))
# ## aqui temos o nome do primeiro autor
# authors_ln_clean <- purrr::map(authors_ln_mod, 1)



