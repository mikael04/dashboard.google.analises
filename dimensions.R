## code to prepare `dimension` dataset goes here
library(dplyr)

plot <- F
dashboard <- F
#########################################################################################
## Lendo arquivo de banco de dados
#########################################################################################
df_dimensions <- fst::read_fst("dados/dimensions_compressed.fst")
df_dimensions <- tibble::as_tibble(df_dimensions)

df_dimensions_sample <- df_dimensions %>% 
  dplyr::sample_frac(0.1)

## df_dim_sort_alph <- toda a base com campos ordenados em ordem alfabética
df_dim_sort_alph <- df_dimensions %>% 
  dplyr::select(sort(tidyselect::peek_vars()))


df_dim_sort_alph_sample <- df_dimensions_sample %>% 
  dplyr::select(sort(tidyselect::peek_vars()))


#########################################################################################
## Manipulando datas
#########################################################################################

df_date_count <- df_dimensions  %>%
  dplyr::filter(date_normal < lubridate::ymd("2021-05-23")) %>%
  dplyr::filter(date_normal > "2020-01-01") %>%
  dplyr::select(id, date_normal) %>%
  dplyr::mutate(date_normal = lubridate::floor_date(date_normal, "month")) %>%
  dplyr::group_by(date_normal) %>%
  dplyr::summarise(count = n()) %>%
  #dplyr::mutate(date_normal = lubridate::format_ISO8601(date_normal, precision = "ym")) %>%
  dplyr::ungroup()

# data.table::fwrite(df_date_count, "/ dados/df_date_count.csv")
if(plot){
  p <- plotly::plot_ly(df_date_count, x = ~date_normal, y = ~count, mode = 'line', type = 'scatter') %>%
    plotly::layout(title = "Evolução de publicações de COVID19 no tempo",
                   xaxis = list(title = ""),
                   yaxis = list(title = "Número de publicações"))
  if(dashboard){
    p
  }
}
#########################################################################################
## Contando países
#########################################################################################
## Recebe apenas a coluna de paises
df_paises <- df_dimensions %>%
# df_paises <- df_dimensions_sample %>%
  dplyr::select(id, research_org_country_names) %>%
  dplyr::filter(research_org_country_names != "")
paises <- df_paises$research_org_country_names
## Separa em uma lista de mais de um elemento quando possui mais de um país
paises_split <- paises %>%
  stringr::str_split(., ';')
## remove todos os caractéres menos letras e números
#list <- lapply(paises, stringr::str_replace_all, ";", "0")
## Apenas valores únicos, para listar todos os países (sem repetição)
unique_values <- unique(rapply(paises_split, function(x) head(x, 30)))
# 
## Transforma em dataframe para manipulação
dt_list <- purrr::map(paises_split, data.table::as.data.table)
dt <- data.table::rbindlist(dt_list, fill = TRUE, idcol = T)
## Agrupa por país e conta quantas vezes aparece
df_count <- dt %>%
  dplyr::filter(V1 != '' & V1 != ' ') %>%
  dplyr::group_by(V1) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::rename(Paises = V1) %>%
  dplyr::ungroup()

df_count_ordered <- df_count %>%
  dplyr::arrange(desc(count))

## Plota gráfico
if(plot){
  if(dashboard){
    p
  }
}
data.table::fwrite(df_count_ordered, "dados/df_paises_count_ordered.csv")
# caso queira trabalhar com a sample
# data.table::fwrite(df_count_ordered, "dados/df_sample_count_ordered.csv")

#########################################################################################
## Publicações por categoria
#########################################################################################
df_dim_categories <- df_dimensions %>%
  dplyr::select(id, categories.for_v1.first_level.codes) %>%
  dplyr::filter(length(categories.for_v1.first_level.codes) > 2) %>%
  dplyr::filter(stringr::str_detect(categories.for_v1.first_level.codes,"[0123456789]"))

## Pega apenas a variável que quero para fazer contagem
categoria <- df_dim_categories$categories.for_v1.first_level.codes
  #dplyr::filter(stringr::str_detect(., "[[:digit:]]"))
list_categ <- lapply(categoria, stringr::str_replace_all, "'", "")
list_categ <- lapply(list_categ, stringr::str_replace_all, " ", "")
list_categ <- lapply(list_categ, stringr::str_replace_all, "\\[", "")
list_categ <- lapply(list_categ, stringr::str_replace_all, "\\]", "")
#list_categ <- lapply(df_dim_categories, stringr::str_replace_all, "\\[", "")
## Separa em uma lista de mais de um elemento quando possui mais de um país
list_categ_split <- list_categ %>%
  stringr::str_split(., ',')

## remove todos os caractéres menos letras e números
#list_categ <- lapply(paises, stringr::str_replace_all, ";", "0")
## Apenas valores únicos, para listar todos os países (sem repetição)
unique_values <- unique(rapply(list_categ_split, function(x) head(x, 5)))

## Transforma em dataframe para manipulação
dt_list <- purrr::map(list_categ_split, data.table::as.data.table)
dt <- data.table::rbindlist(dt_list, fill = TRUE, idcol = T)
## Agrupa por país e conta quantas vezes aparece
df_categ_count <- dt %>%
  dplyr::filter(V1 != '' & V1 != ' ') %>%
  dplyr::group_by(V1) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::rename(ID = V1) %>%
  dplyr::mutate(ID = as.numeric(ID)) %>%
  dplyr::ungroup()

## Lê arquivo com nome das categorias
df_nome_categ <- data.table::fread("dados/ANZSRC_FoR.csv") %>%
  dplyr::rename(Category = FoR)

## Agrupa para pegar nome, ajusta tabela
df_categ <- dplyr::inner_join(df_categ_count, df_nome_categ, by='ID') %>%
  dplyr::mutate(Category = paste0(ID, " - ", Category)) %>%
  dplyr::select(Category, count) %>%
  dplyr::rename(Count = count)

df_categ_count_ordered <- df_categ %>%
  dplyr::arrange(desc(Count))

## Se quiser mostrar total na última linha, para uma tabela
# df_categ_withTotal <- df_categ %>%
#   ## Adicionando linha de total
#   dplyr::bind_rows(summarise(.,
#                              across(where(is.numeric), sum),
#                              across(where(is.character), ~"TOTAL")))

## Escrevendo tabela
data.table::fwrite(df_categ_count_ordered, "dados/df_categ_count_ordered.csv")

## Plota gráfico
if(plot){
  layout_axis_y <- list(
    title = "",
    showline = F,
    showticklabels = T,
    showgrid = F
  )
  layout_axis_x <- list(
    title = "",
    showline = F,
    showticklabels = T,
    showgrid = F,
    tickformat = "digit"
  )
  m <- list(l=350, r=50, b=50, t=30, pad=4)
  p <- plotly::plot_ly(df_categ, x = ~Count, y = ~reorder(Category, Count), type = 'bar')
  p <- p %>%
    plotly::layout(title = "Publicações por categoria", xaxis = layout_axis_x, yaxis = layout_axis_y, dragmode='pan', margin = m) %>%
    plotly::config(modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d", "toggleSpikelines"), displaylogo = FALSE)
  if(dashboard){
    p
  }
}
#########################################################################################
## Análise exploratória funders e organização
#########################################################################################
df_dimensions_fund_org <- df_dimensions %>%
  dplyr::select(funder_orgs, raw_affiliations, research_org_cities, research_org_city_names,
                research_org_countries, research_org_country_names, research_org_state_codes,
                research_org_state_names, research_orgs)


skim_funder_orgs <- skimr::skim(df_dimensions_fund_org)

data.table::fwrite(skim_funder_orgs, "dados/df_skim_funder_orgs.csv")

#########################################################################################
df_perguntas <- data.table::fread("dados/buscaCompleta2305.csv")

df_dim_sort_alph_sample_orgs <- df_dim_sort_alph_sample %>%
  dplyr::select(funder_orgs, research_orgs)
