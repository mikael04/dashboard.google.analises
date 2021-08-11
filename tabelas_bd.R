## 1. Criação de tabelas  ---------------------------------
library(dplyr)
## quais campos preciso? id +
## artigos_autores -> 
## artigos_cit -> 
## evol_pub_tipo -> date_normal, type
## wordcloud -> title.preferred, abstract.preferred
## paises_pub -> research_org_country_names
## categ_pub -> categories.for_v1.first_level.codes
## 
df_dimensions_filter <- fst::read_fst("dados/dimensions_compressed.fst")%>%
  dplyr::select(id, authors_fn = authors,  authors_ln =  `authors/lastname`, metrics.times_cited, altmetrics.score, date_normal, type, title.preferred, abstract.preferred,
                research_org_country_names, categories.for_v1.first_level.codes)
df_dimensions_filter <- tibble::as_tibble(df_dimensions_filter)

df_dimensions_filter_sample <- df_dimensions_filter %>% 
  dplyr::sample_frac(0.01)

### 1.1 Manipulando países  ---------------------------------
## Forma antiga, antes precisava de todos os países, agora,
## tanto para países quanto autores pegarei apenas primeiro e último
# ## Recebe apenas a coluna de paises
# df_paises <- df_dimensions_filter %>%
#   # df_paises <- df_dimensions_sample %>%
#   dplyr::select(id, research_org_country_names) %>%
#   dplyr::filter(research_org_country_names != "")
# paises <- df_paises$research_org_country_names
# ## Separa em uma lista de mais de um elemento quando possui mais de um país
# paises_split <- paises %>%
#   stringr::str_split(., ';')
# 
# ## remove todos os caractéres menos letras e números
# #list <- lapply(paises, stringr::str_replace_all, ";", "0")
# ## Apenas valores únicos, para listar todos os países (sem repetição)
# unique_values <- unique(rapply(paises_split, function(x) head(x, 30)))
# 
# ## Transforma em dataframe para manipulação
# dt_list <- purrr::map(paises_split, data.table::as.data.table)
# dt <- data.table::rbindlist(dt_list, fill = TRUE, idcol = T)
library(stringr)

## Funções para pegar primeiro e último pais
fct_first_country <- function (x){
  str_extract(x, '[^;]+') 
}
fct_last_country <- function (x){
  sub(".*;", "", x)
}

df_dimensions_filter_country <- df_dimensions_filter %>%
  dplyr::select(id, research_org_country_names) %>%
  dplyr::filter(research_org_country_names != "")  %>%
  dplyr::mutate(first_country = fct_first_country(research_org_country_names)) %>%
  dplyr::mutate(last_country = fct_last_country(research_org_country_names)) %>%
  dplyr::select(-research_org_country_names) %>%
  dplyr::mutate(last_country = if_else(first_country == last_country, "NoCountry",
                                       last_country))

### 1.2 Tabela tipo e data   ---------------------------------

## arredondando para mês (que é como vai ser exibido nos gráficos)
df_dimensions_filter_type_date <- df_dimensions_filter %>%
  dplyr::filter(date_normal < lubridate::ymd("2021-05-24")) %>% ##data da última extração
  dplyr::filter(date_normal > "2020-01-01") %>% ##não pegar arquivos que possuem apenas ano para não distorcer gráfico
  dplyr::mutate(date = lubridate::floor_date(date_normal, "month")) %>%
  dplyr::select(id, date, type)



df_dimensions_filter_type_date_db <- df_dimensions_filter_type_date %>%
  dplyr::group_by(type, date) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup()

## 1.3 Tabelas de países com tipo e data ---------------------

df_dim_filter_type_date_country <- full_join(df_dimensions_filter_type_date, df_dimensions_filter_country,
                                             by=c("id")) %>%
  dplyr::mutate(first_country = tidyr::replace_na(first_country, "NoCountry")) %>%
  dplyr::mutate(last_country = tidyr::replace_na(last_country, "NoCountry")) %>%
  dplyr::mutate(type = tidyr::replace_na(type, "NoType"))

df_dim_filter_type_date_country_f_db <- df_dim_filter_type_date_country %>%
  dplyr::group_by(type, date, first_country) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup()

df_dim_filter_type_date_country_l_db <- df_dim_filter_type_date_country %>%
  dplyr::group_by(type, date, last_country) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup()

df_dim_filter_type_date_country_db <- full_join(df_dim_filter_type_date_country_f_db, 
                                                df_dim_filter_type_date_country_l_db,
                                                by=c("type" = "type", "date" = "date",
                                                     "first_country" = "last_country")) %>%
  dplyr::mutate(count.x = tidyr::replace_na(count.x, 0)) %>%
  dplyr::mutate(count.y = tidyr::replace_na(count.y, 0)) %>%
  dplyr::mutate(count_sum = count.x + count.y) %>%
  dplyr::select(-count.x, -count.y)

# ##Tabela com resultado de agregações
# df_dim_filter_type_date_country_db

## 1.5 Manipulando (todos) países ---------------------------------


df_coun <- data.table::fread("dados/df_paises_wider.RDS")

col_names <- colnames(df_coun)
## A partir do segundo, o primeiro é o "id"
for(i in 2:ncol(df_coun)){
  df_coun[[i]] <- sapply(df_coun[[i]], function(x){
    if(x == "1"){
      x = col_names[i]
    }else{
      x = "no"
    }
  })
}