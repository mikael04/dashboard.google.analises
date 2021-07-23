## code to prepare `dimension` dataset goes here
library(dplyr)

plot <- F
dashboard <- F
## Lendo arquivo de banco de dados  ---------------------------------

df_dimensions <- fst::read_fst("dados/dimensions_compressed.fst")
df_dimensions <- tibble::as_tibble(df_dimensions)

df_dimensions_sample <- df_dimensions %>% 
  dplyr::sample_frac(0.1)

## df_dim_sort_alph <- toda a base com campos ordenados em ordem alfabética
df_dim_sort_alph <- df_dimensions %>% 
  dplyr::select(sort(tidyselect::peek_vars()))


df_dim_sort_alph_sample <- df_dimensions_sample %>% 
  dplyr::select(sort(tidyselect::peek_vars()))


## Manipulando datas ---------------------------------


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

rm(df_date_count, p)

## Contando países ---------------------------------

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

rm(df_paises, paises, paises_split, unique_values, dt_list, dt,
   df_count, df_count_ordered, p)


## Publicações por categoria  ---------------------------------

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

rm(df_dim_categories, categoria, list_categ, list_categ_split,
   unique_values, dt_list, dt,
   df_categ_count, df_nome_categ, df_categ, df_categ_count_ordered,
   layout_axis_x, layout_axis_y, m, p)


## Análise exploratória funders e organização ---------------------------------

df_dimensions_fund_org <- df_dimensions %>%
  dplyr::select(funder_orgs, raw_affiliations, research_org_cities, research_org_city_names,
                research_org_countries, research_org_country_names, research_org_state_codes,
                research_org_state_names, research_orgs)


skim_funder_orgs <- skimr::skim(df_dimensions_fund_org)

data.table::fwrite(skim_funder_orgs, "dados/df_skim_funder_orgs.csv")
stringr::str_detect(df_dimensions_fund_org_sample$raw_affiliations, "vazio")
sum(stringr::str_detect(df_dimensions_fund_org$raw_affiliations, "vazio"))
sum(stringr::str_detect(df_dimensions_fund_org$raw_affiliations, "\\|"))
sum(stringr::str_detect(df_dimensions_fund_org_sample$raw_affiliations, " "))
df_fund_org_sample_n_vazio <- df_dimensions_fund_org_sample %>%
  dplyr::filter(!stringr::str_detect(df_dimensions_fund_org_sample$raw_affiliations, "vazio"))

rm(df_dimensions_fund_org, skim_funder_orgs, df_fund_org_sample_n_vazio)

## Mapa de palavras ---------------------------------

df_word_cloud <- df_dimensions_sample %>%
  dplyr::select(title.preferred, abstract.preferred)

df_word_cloud$title_clean <- gsub("[[:punct:]]", "", df_word_cloud$title.preferred)
df_word_cloud$abstract_clean <- gsub("[[:punct:]]", "", df_word_cloud$abstract.preferred)
df_word_cloud <- df_word_cloud %>%
  dplyr::select(title_clean, abstract_clean)
library(tm)
my_corpus <- VCorpus(VectorSource(df_word_cloud))
# my_corpus <- tm_map(my_corpus, removeWords, c(stopwords("english")))
# myStopwords <- c(stopwords("english"), "the", "this", "can")
my_corpus <- tm_map(my_corpus, removeWords, Stopwords)
dtm <- TermDocumentMatrix(my_corpus)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)
words <- c("the", "this", "can")
df_r <- df %>%
  filter(!word %in% words)

data.table::fwrite(df_r, "dados/df_word_cloud.csv")

rm(df_word_cloud, my_corpus, myStopwords)


## Perguntas e artigos resposta - Inconsistências  ---------------------------------

df_perguntas <- data.table::fread("dados/buscaCompleta2305.csv")

df_dimensions_perguntas_anti <- dplyr::anti_join(df_dimensions, df_perguntas, by="id")

df_perguntas_dupli_doi <- df_perguntas %>%
  dplyr::select(doi) %>%
  dplyr::group_by(doi) %>% 
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count > 1) %>%
  ## Se quiser ver todas as linhas que são repetidas, sem contagem e sem agrupamento,
  ## só comentar a linha abaixo
  # dplyr::distinct(.keep_all = T) %>%
  dplyr::ungroup()

df_dimensions_dupli_doi <- df_dimensions %>% 
  dplyr::select(doi) %>%
  dplyr::group_by(doi) %>% 
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count > 1) %>%
  # dplyr::distinct(.keep_all = T) %>%
  dplyr::ungroup()

df_perguntas_dupli_id <- df_perguntas %>% 
  dplyr::select(id) %>%
  dplyr::group_by(id) %>% 
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count > 1) %>%
  # dplyr::distinct(.keep_all = T) %>%
  dplyr::ungroup()

df_dimensions_dupli_id <- df_dimensions %>% 
  dplyr::select(id) %>%
  dplyr::group_by(id) %>% 
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count > 1) %>%
  # dplyr::distinct(.keep_all = T) %>%
  dplyr::ungroup()

df_valid_doi_perguntas <- nrow(df_perguntas) - nrow(df_perguntas_dupli_doi)
df_valid_doi_dimensions <- nrow(df_dimensions) - nrow(df_dimensions_dupli_doi)

df_valid_id_perguntas <- nrow(df_perguntas) - nrow(df_perguntas_dupli_id)
df_valid_id_dimensions <- nrow(df_dimensions) - nrow(df_dimensions_dupli_id)

rm(df_dimensions_perguntas_anti, df_perguntas_dupli_doi, df_perguntas_dupli_id,
   df_dimensions_dupli_doi, df_dimensions_dupli_id, df_valid_doi_perguntas, 
   df_valid_doi_dimensions, df_valid_id_perguntas, df_valid_id_dimensions)

## Perguntas e artigos resposta - Tabela resposta ---------------------------------
# df_dimensions_cut <- df_dimensions %>%
#   dplyr::select(id, title.preferred, `authors/lastname`, abstract.preferred, date_normal,
#                 subtitles, type, research_org_country_names)
# df_perguntas <- data.table::fread("dados/buscaCompleta2305.csv") %>%
#   dplyr::select(-abstract.preferred, -title.preferred)
# ###
# ## Feito apenas uma vez, depois usar o "Relacao_clean.csv"
# ## manipulando para ter mesmo nome de coluna do df_perguntas
# # df_perguntas_dict <- data.table::fread("dados/Relacao.csv")
# # df_perguntas_dict[,1] <- lapply(df_perguntas_dict[,1], gsub, pattern = " ", replacement = "", fixed = T)
# # df_perguntas_dict[,1] <- lapply(df_perguntas_dict[,1], gsub, pattern = "B", replacement = "b", fixed = T)
# # ## Escrevendo nova tabela
# # data.table::fwrite(df_perguntas_dict, "dados/Relacao_clean.csv")
# ###
# df_perguntas_dict <- data.table::fread("dados/Relacao_clean.csv")
# 
# df_dimensions_ij_perguntas <- dplyr::inner_join(df_dimensions_cut,
#                                                 df_perguntas, by="id")
####
## Como é feito no app
# arvore_no_sel = 'What are the prodromal symptoms in COVID-19?'
# col_name <- df_perguntas_dict %>%
#   dplyr::filter(Pergunta == arvore_no_sel) %>%
#   dplyr::select(Busca)
# df_dimensions_ij_perguntas_search <- df_dimensions_ij_perguntas %>%
#     dplyr::filter(!!as.name(col_name$Busca) == '1')
# ## Escrevendo uma tabela exemplo
# data.table::fwrite(df_dimensions_ij_perguntas_search, "dados/df_dimensions_ij_perguntas_search.csv")
####
## Para o app_tabela
# col_name <- df_perguntas_dict[df_perguntas_dict$Pergunta == input$question]$Busca
# df_dimensions_ij_perguntas_search <- df_dimensions_ij_perguntas %>%
#   dplyr::filter(!!as.name(col_name) == '1')
####

rm(df_dimensions_cut, df_perguntas, df_dimensions_ij_perguntas)

## Top autores ---------------------------------

## Recebe apenas a coluna de paises
df_autores <- df_dimensions %>%
  # df_paises <- df_dimensions_sample %>%
  dplyr::select(id, authors = `authors/lastname`) %>%
  dplyr::filter(authors != "", authors != "vazio")
autores <- df_autores$authors
## Separa em uma lista de mais de um elemento quando possui mais de um país
autores_split <- autores %>%
  stringr::str_split(., '\\|')
## remove todos os caractéres menos letras e números
#list <- lapply(paises, stringr::str_replace_all, ";", "0")
## Apenas valores únicos, para listar todos os países (sem repetição)
unique_values <- unique(rapply(autores_split, function(x) head(x, 30)))

## Transforma em dataframe para manipulação
df_list <- purrr::map(autores_split, data.table::as.data.table)
df <- data.table::rbindlist(df_list, fill = TRUE, idcol = T)
## Agrupa por país e conta quantas vezes aparece
df_count_autores <- df %>%
  dplyr::filter(V1 != '' & V1 != ' ') %>%
  dplyr::group_by(V1) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::rename(Autores = V1) %>%
  dplyr::ungroup()

df_autores_ordered_cit <- df_count_autores %>%
  dplyr::arrange(desc(count))
data.table::fwrite(df_autores_ordered_cit, "dados/df_autores_cit_ordered.csv")
## Apenas o top20
# df_count_autores <- data.table::fread("dados/df_autores_cit_ordered.csv")
df_autores_cit_top20 <- df_count_autores %>%
  dplyr::slice_head(n = 20)
data.table::fwrite(df_autores_cit_top20, "dados/df_autores_cit_top20.csv")

# ## Adicionando primeiro nome de autores
# df_autores_f <- df_dimensions %>%
#   # df_paises <- df_dimensions_sample %>%
#   dplyr::select(id, authors_f = authors) %>%
#   dplyr::filter(authors_f != "", authors_f != "vazio")
# ## Limpar memória
# rm(df_dimensions)
# 
# autores_fn <- df_autores_f$authors_f
# ## Separa em uma lista de mais de um elemento quando possui mais de um país
# autores_fn_split <- autores_fn %>%
#   stringr::str_split(., '\\|')
# rm(autores_fn, df_autores)
# autores_fn_split_first <- lapply(autores_fn_split, substring, 1, 1)
# ##  Aqui tenho o primeiro nome do autor, com um "." após a primeira letra, porém,
# ## ainda falta adicionar os outros nomes, pego apenas o primeiro
# autores_fn_split_first_c <- lapply(autores_fn_split_first, function(x) paste0(x, "."))

## Top citações ---------------------------------

df_citacoes_ordered <- df_dimensions %>%
  dplyr::select(doi, metrics.times_cited, altmetrics.score) %>%
  dplyr::arrange(desc(metrics.times_cited))

df_count_autores_ordered_top20 <- df_citacoes_ordered %>%
  dplyr::slice_head(n = 20)
  
data.table::fwrite(df_citacoes_ordered, "dados/df_citacoes_ordered.csv")
data.table::fwrite(df_count_autores_ordered_top20, "dados/df_citacoes_ordered_top20.csv")
