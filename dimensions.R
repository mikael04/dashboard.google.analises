## code to prepare `dimension` dataset goes here
library(dplyr)
# usethis::use_data(dimension, overwrite = TRUE)

df_dimensions <- data.table::fread("data-raw/dimensionscompletoTOTAL.csv")

df_dimensions <- as_tibble(df_dimensions)

## df_dim_sort_alph <- toda a base com campos ordenados em ordem alfabética
df_dim_sort_alph <- df_dimensions %>% 
  dplyr::select(sort(tidyselect::peek_vars()))

## df_dim_cut <- base com campos selecionados (provavelmente nem todos são importantes, esse campo tem 22 variáveis)
## diferente do geral que tem 93
df_dim_cut <- df_dimensions %>%
  dplyr::select(id, year, doi, type, date, date_normal, date_inserted, clinical_trial_ids, altmetrics.score, open_access_categories,
                open_access_categories_v2, research_org_city_names, research_org_country_names, reference_ids, funding_details,
                title.preferred, abstract.preferred, publisher.name, publisher.name, metrics.times_cited, metrics.recent_citations,
                citations_count, journal.id, journal.title)


typeof(df_dimensions$date)
typeof(df_dimensions$date_print)


df_dimensions_sample <- head(df_dimensions, 50)

## Checar se existe alguma linha que não esteja no padrão ("ano", "ano-mes", "ano-mes-dia")
count(df_dimensions_sample[(stringr::str_count(df_dimensions_sample) != 10) && (stringr::str_count(df_dimensions_sample) != 7) && (stringr::str_count(df_dimensions_sample) != 4)])

df_dimensions[(stringr::str_count(df_dimensions) == 10)]

df_dimensions_sample <- df_dimensions_sample %>% 
  mutate(date_ = case_when(
    stringr::str_count(date) < 7 ~ "aaaa",
    stringr::str_count(date) < 10 ~ "aaaa-mm",
    TRUE ~ "aaaa-mm-dd"
    )) %>%
  dplyr::select(id, year, date, date_)

## Já existem diferentes datas, qual é a correta?
df_date <- df_dimensions %>%
  select(id, year, doi, date, date_print, date_normal, date_inserted, date_imported_gbq, date_online)

## Países
sum(is.na(df_dimensions$research_org_countries))
sum(df_dimensions$research_org_countries)
sum(df_dimensions$research_org_cities != "")
sum(df_dimensions$research_org_city_names != "")
# 29771 linhas com cidades (cities id) faltantes / 29780 (city_name) - Total 46154 linhas
sum(df_dimensions$research_org_countries != "")
sum(df_dimensions$research_org_country_names != "")
# 30081 linhas com países faltantes - Total 46154 linhas

stringr::str_split(df_dimensions[df_dimensions$doi == "10.3760/cma.j.cn112138-20200328-00310"]$research_org_country_names, ",")

skimr::skim(df_dimensions)


unique_countrys <- unique(df_dim_cut$research_org_country_names)
unique_countrys <- unique_countrys %>%
  stringr::str_split(',')
# unique_countrys <- stringr::str_replace_all(unique_countrys, "[^[:alnum:]]", "")
list <- lapply(unique_countrys, stringr::str_replace_all, "[^[:alnum:]]", "")
unique_values <- unique(rapply(list, function(x) head(x, 30)))

dt_list <- purrr:map(list, data.table::as.data.table)
dt <- data.table::rbindlist(dt_list, fill = TRUE, idcol = T)
dt_count <- dt %>%
  dplyr::filter(V1 != '' & V1 != ' ') %>% 
  dplyr::group_by(V1) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::rename(Paises = V1) %>%
  dplyr::ungroup()
  