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

library(stringr)

df_country <- df_dimensions_filter %>%
  dplyr::select(id, research_org_country_names) %>%
  dplyr::filter(research_org_country_names != "") %>%
  dplyr::mutate_at(.vars = c(2) , function (x) stringr::str_split(x, pattern = ";"))

## Verificando o maior número de países nas linhas
max_lenght = 1
for(i in 1:nrow(df_country)){
  col_length <- length(df_country[[2]][[i]])
  #print(col_length)
  if(col_length > max_lenght){
    col_name <- i
    max_lenght <- col_length
  }
}

## artigo com maior número de países na base
# df_country[[3]][[183424]]

##Adicionando coluna de ID para agrupar com a contagem depois
df_country <- tibble::rowid_to_column(df_country, "id_pub")

## Transforma lista para manipulação
countries <- purrr::map(df_country$research_org_country_names , data.table::as.data.table)
## Transforma em dataframe, mas criando novas linhas para mesmo id (separa países por linha)
df_countries_count <- data.table::rbindlist(countries, fill = TRUE, idcol = T) %>%
  dplyr::rename(id = .id, paises = V1)

df_paises_long <- dplyr::inner_join(df_country, df_countries_count, by=c("id_pub" = "id")) %>%
  dplyr::select(-id_pub, -research_org_country_names)

rm(countries, df_countries_count, df_country)
### 1.2 Tabela tipo e data   ---------------------------------

## arredondando para mês (que é como vai ser exibido nos gráficos)
df_dimensions_filter_type_date <- df_dimensions_filter %>%
  dplyr::filter(date_normal < lubridate::ymd("2021-05-24")) %>% ##data da última extração
  dplyr::filter(date_normal > "2020-01-01") %>% ##não pegar arquivos que possuem apenas ano para não distorcer gráfico
  dplyr::mutate(date = lubridate::floor_date(date_normal, "month")) %>%
  dplyr::select(id, date, type)

## 1.3 Tabelas de países com tipo e data ---------------------

df_dim_filter_type_date_country <- full_join(df_dimensions_filter_type_date, df_paises_long,
                                             by=c("id"))

## Tabela com resultado de agregações
df_dim_filter_type_date_country_db <- df_dim_filter_type_date_country %>%
  dplyr::group_by(type, date, paises) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup()

# Foram mantidos os NA, para caso não seja selecionado algum filtro

