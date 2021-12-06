library(dplyr)

## Caso queira testar alguns gráficos e tabelas gerados, dois parâmetros abaixo
plot <- F
dashboard <- F
debug <- F
write <- F
## 1. Lendo arquivo de banco de dados  ---------------------------------
## Setup, lendo a base de dados (pode ser usado qualquer outro formato)
# df_dimensions <- fst::read_fst("dados/dimensions_compressed.fst") %>%
  # select(id, doi, date_normal, type, research_org_country_names, raw_affiliations,
  #        categories.for_v1.first_level.codes, title.preferred, abstract.preferred,
  #        authors, `authors/lastname`, metrics.times_cited, altmetrics.score, journal_lists)

# fst::write_fst(df_dimensions, "dados/dimensions_compressed_selected.fst")

df_dimensions <- fst::read_fst("dados/dimensions_compressed_selected.fst")
df_dimensions <- tibble::as_tibble(df_dimensions)


df_dimensions_sample <- df_dimensions %>% 
  dplyr::sample_frac(0.01)

# if(write)
#    data.table::fwrite(df_dimensions_sample, "dados/df_sample.csv")

## não precisam rodar, só rodo para organizar melhor o banco (variáveis ficam em ordem alfabética,
## assim fica mais fácil de procurar por elas)
# ##Sample da base com campos ordenados em ordem alfabética
# df_dim_sort_alph_sample <- df_dimensions_sample %>% 
#   dplyr::select(sort(tidyselect::peek_vars()))

# ## df_dim_sort_alph <- toda a base com campos ordenados em ordem alfabética
# df_dim_sort_alph <- df_dimensions %>% 
#   dplyr::select(sort(tidyselect::peek_vars()))


## Evolução de publicações no tempo ---------------------------------

df_date_count <- df_dimensions  %>%
  dplyr::filter(date_normal < lubridate::ymd("2021-05-24")) %>% ##data da última extração
  dplyr::filter(date_normal > "2020-01-01") %>% ##não pegar arquivos que possuem apenas ano para não distorcer gráfico
  dplyr::select(id, date_normal) %>%
  dplyr::mutate(date_normal = lubridate::floor_date(date_normal, "month")) %>%
  dplyr::group_by(date_normal) %>%
  dplyr::summarise(count = n()) %>%
  #dplyr::mutate(date_normal = lubridate::format_ISO8601(date_normal, precision = "ym")) %>%
  dplyr::ungroup()

if(write)
  data.table::fwrite(df_date_count, "/dados/df_date_count.csv")
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

## Evolução de publicações por tipo, no tempo ---------------------------------

df_dimensions_type_date <- df_dimensions %>%
  dplyr::filter(date_normal > "2020-01-01") %>%
  dplyr::mutate(date_normal = lubridate::floor_date(date_normal, "month")) %>%
  dplyr::select(id, date_normal, type) %>%
  dplyr::group_by(date_normal, type) %>%
  dplyr::filter(date_normal <= lubridate::ymd("2021-05-23")) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup()

if(write)
  data.table::fwrite(df_dimensions_type_date, "dados/df_dimensions_type_date.csv")
rm(df_dimensions_type_date)
## Publicações por país ---------------------------------

# df_dimensions <- df_dimensions %>%
#   dplyr::select(id, date_normal, type, title.preferred, abstract.preferred,
#                 research_org_country_names, categories.for_v1.first_level.codes)
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
unique_values_ordered <- as.data.frame(stringr::str_sort(unique_values)) %>%
  dplyr::rename(pais = `stringr::str_sort(unique_values)`)

if(write)
  data.table:: fwrite(unique_values_ordered, "dados/paises.csv")

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
if(write)
  data.table::fwrite(df_count_ordered, "dados/df_paises_count_ordered.csv")
# caso queira trabalhar com a sample
# data.table::fwrite(df_count_ordered, "dados/df_sample_paises_count_ordered.csv")

rm(df_paises, paises, paises_split, unique_values, dt_list, dt,
   df_count, df_count_ordered, unique_values_ordered, p)


## Publicações por categoria (ANZSRC_FoR)  ---------------------------------

df_dim_categories <- df_dimensions %>%
  dplyr::select(id, categories.for_v1.first_level.codes) %>%
  dplyr::filter(length(categories.for_v1.first_level.codes) > 2) %>%
  dplyr::filter(stringr::str_detect(categories.for_v1.first_level.codes,"[0123456789]"))

df_dim_categories_clean <- df_dim_categories |> 
  dplyr::rename(categ = categories.for_v1.first_level.codes) |> 
  dplyr::mutate(categ = sapply(categ, stringr::str_replace_all, "'", "")) |> 
  dplyr::mutate(categ = sapply(categ, stringr::str_replace_all, " ", "")) |> 
  dplyr::mutate(categ = sapply(categ, stringr::str_replace_all, "\\[", "")) |> 
  dplyr::mutate(categ = sapply(categ, stringr::str_replace_all, "\\]", ""))
  
## calculando o número máximo de categorias
nmax <- max(stringr::str_count(df_dim_categories_clean$categ, ",")) + 1
  
df_dim_categories_manip <- df_dim_categories_clean |> 
    tidyr::separate(categ, paste0("categ_", seq_len(nmax)), ",")
df_dim_categories_manip <- df_dim_categories_manip |> 
  tidyr::pivot_longer(!id, names_to = "categs_", values_to = "categ") %>%
  dplyr::select(-categs_) %>%
  dplyr::filter(!is.na(categ))

# ## Pega apenas a variável que quero para fazer contagem
# categoria <- df_dim_categories$categories.for_v1.first_level.codes
# #dplyr::filter(stringr::str_detect(., "[[:digit:]]"))
# list_categ <- lapply(categoria, stringr::str_replace_all, "'", "")
# list_categ <- lapply(list_categ, stringr::str_replace_all, " ", "")
# list_categ <- lapply(list_categ, stringr::str_replace_all, "\\[", "")
# list_categ <- lapply(list_categ, stringr::str_replace_all, "\\]", "")
# #list_categ <- lapply(df_dim_categories, stringr::str_replace_all, "\\[", "")
# ## Separa em uma lista de mais de um elemento quando possui mais de um país
# list_categ_split <- list_categ %>%
#   stringr::str_split(., ',')
# 
# ## remove todos os caractéres menos letras e números
# #list_categ <- lapply(paises, stringr::str_replace_all, ";", "0")
# ## Apenas valores únicos, para listar todos os países (sem repetição)
# unique_values <- unique(rapply(list_categ_split, function(x) head(x, 5)))
# 
# ## Transforma em dataframe para manipulação
# dt_list <- purrr::map(list_categ_split, data.table::as.data.table)
# dt <- data.table::rbindlist(dt_list, fill = TRUE, idcol = T)
## Agrupa por país e conta quantas vezes aparece
df_categ_count <- df_dim_categories_manip %>%
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
if(write)
  data.table::fwrite(df_categ_count_ordered, "dados/df_categ_count_ordered.csv")
df_categ <- data.table::fread("dados/df_categ_count_ordered.csv")

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

df_dimensions_fund_org <- fst::read_fst("dados/dimensions_compressed.fst") %>%
  dplyr::select(funder_orgs, raw_affiliations, research_org_cities, research_org_city_names,
                research_org_countries, research_org_country_names, research_org_state_codes,
                research_org_state_names, research_orgs)

df_dimensions_fund_org_sample <- df_dimensions_fund_org %>%
  dplyr::slice_sample(n = 0.1)

skim_funder_orgs <- skimr::skim(df_dimensions_fund_org)

if(write)
  data.table::fwrite(skim_funder_orgs, "dados/df_skim_funder_orgs.csv")
stringr::str_detect(df_dimensions_fund_org_sample$raw_affiliations, "vazio")
sum(stringr::str_detect(df_dimensions_fund_org$raw_affiliations, "vazio"))
sum(stringr::str_detect(df_dimensions_fund_org$raw_affiliations, "\\|"))
sum(stringr::str_detect(df_dimensions_fund_org_sample$raw_affiliations, " "))
df_fund_org_sample_n_vazio <- df_dimensions_fund_org_sample %>%
  dplyr::filter(!stringr::str_detect(df_dimensions_fund_org_sample$raw_affiliations, "vazio"))

rm(df_dimensions_fund_org, skim_funder_orgs,
   df_fund_org_sample_n_vazio, df_dimensions_fund_org_sample)

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
myStopwords <- c(stopwords("english"), "the", "this", "can")
my_corpus <- tm_map(my_corpus, removeWords, myStopwords)
dtm <- TermDocumentMatrix(my_corpus)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)
words <- c("the", "this", "can")
df_r <- df %>%
  filter(!word %in% words)

if(write)
  data.table::fwrite(df_r, "dados/df_word_cloud.csv")

rm(df, df_r, dtm, matrix, words,
  df_word_cloud, my_corpus, myStopwords)


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
   df_valid_doi_dimensions, df_valid_id_perguntas, df_valid_id_dimensions,
   df_perguntas)



## Top autores ---------------------------------

### Nomes   ---------------------------------

df_dimensions_authors <- df_dimensions %>%
  dplyr::select(id, authors_fn, authors_ln) %>%
  dplyr::mutate(authors_fn = if_else(authors_fn != "", authors_fn, "vazio")) |> 
  dplyr::mutate(authors_ln = if_else(authors_ln != "", authors_ln, "vazio"))

## calculando o número máximo de primeiros nomes
nmax <- max(stringr::str_count(df_dimensions_authors$authors_fn, "\\|"), na.rm = T) + 1
j = 1
for(i in 1:nrow(df_dimensions_authors)){
  max_names <- max(stringr::str_count(df_dimensions_authors[[2]][[i]], "\\|"), na.rm = T)
  if(j < max_names){
    j = max_names
    max_authors <- i
  }
}

df_dimensions_authors_top_names <- df_dimensions_authors %>%
  dplyr::mutate(n_autores = stringr::str_count(authors_fn, "\\|"))

## Recebe apenas a coluna de paises
df_autores <- df_dimensions %>%
  # df_paises <- df_dimensions_sample %>%
  dplyr::select(id, authors = authors_last_name) %>%
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

if(write)
  data.table::fwrite(df_autores_ordered_cit, "dados/df_autores_cit_ordered.csv")
## Apenas o top20
# df_count_autores <- data.table::fread("dados/df_autores_cit_ordered.csv")
df_autores_cit_top20 <- df_count_autores %>%
  dplyr::slice_head(n = 20)
if(write)
  data.table::fwrite(df_autores_cit_top20, "dados/df_autores_cit_top20.csv")



# df_dimensions_sample_gist <- df_dimensions_sample %>%
#   dplyr::select(id, authors, `authors/lastname`)
# if(write)
#   data.table::fwrite(df_dimensions_sample_gist, "dados/df_gist.csv")
## Adicionando primeiro nome de autores
df_autores_f_l <- df_dimensions_sample %>%
  # df_paises <- df_dimensions_sample %>%
  dplyr::select(id, authors_f = authors, authors_l = `authors/lastname`) %>%
  dplyr::filter(authors_f != "", authors_f != "vazio")
## Limpar memória
#rm(df_autores_f)

autores_fn <- df_autores_f_l$authors_f
## Separa em uma lista de mais de um elemento quando possui mais de um país
autores_fn_split <- autores_fn %>%
  stringr::str_split(., '\\|')
rm(autores_fn, df_autores)
# autores_fn_split_first <- lapply(autores_fn_split, substring, 1, 1)
# autores_mais_um_nome <- lapply(autores_fn_split, function(x) stringr::str_detect(x, " "))
autores_fn_split_ <- lapply(autores_fn_split, function(x) stringr::str_to_title(x))
autores_fn_split__ <- lapply(autores_fn_split_, function (x) stringr::str_extract_all(x, stringr::regex("[:upper:]")))
## Agora temos uma lista de listas, ao invés de uma lista de caractéres,
## mas acho que está ok, só teria que agrupar a lista em caractéres pra depois fazer a união

##Seguir aqui
autores_fn_split___ <- lapply(autores_fn_split__, function (x) paste0(x, "."))
length(autores_fn_split__[[3]][[2]])
i=1
j=1
debug = T
for(i in 1:length(autores_fn_split__)){
  for(j in 1:length(autores_fn_split__[[i]])){
    if(autores_fn_split__[[i]][[j]] > 1){
      if(debug){
        print(paste0("i = ", i))
        print(paste0("j = ", j))
        print(autores_fn_split__[[i]][[j]])
      }
      
    }
  }
}

# length(autores_fn_split__[[i]])
# autores_fn_split__[[137]]
# autores_fn_split_[[93]]
##  Aqui tenho o primeiro nome do autor, com um "." após a primeira letra, porém,
## ainda falta adicionar os outros nomes, pego apenas o primeiro
# autores_fn_split_first_c <- lapply(autores_fn_split_first, function(x) paste0(x, "."))
rm(autores_split, df_autores_f_l, autores_fn, autores, unique_values, df_list,
   df, df_count_autores, df_autores_ordered_cit, df_autores_cit_top20,
  autores_fn_split, autores_fn_split_, autores_fn_split__, autores_fn_split___)
## Top citações ---------------------------------

df_citacoes_ordered <- df_dimensions %>%
  dplyr::select(doi, authors, `authors/lastname`, metrics.times_cited, altmetrics.score) %>%
  dplyr::arrange(desc(metrics.times_cited))

df_count_autores_ordered_top20 <- df_citacoes_ordered %>%
  dplyr::slice_head(n = 20)

if(write){
  data.table::fwrite(df_citacoes_ordered, "dados/df_citacoes_ordered.csv")
  data.table::fwrite(df_count_autores_ordered_top20, "dados/df_citacoes_ordered_top20.csv")
}
## BD autores e afiliações ---------------------------------


df_raw_affiliation <- data.table::fread("dados/mikael_raw_affiliation.csv", sep = "/")
df_raw_affiliation_clean <- df_raw_affiliation %>%
  dplyr::group_by(doi) %>%
  dplyr::summarise(last_t = last(raw_affiliation))

# problemas nas linhas (número estranho, pessoal da extração vai me retornar)

## Altimetria  ---------------------------------

df_alt <- df_dimensions %>%
  dplyr::select(doi, altmetrics.score) %>%
  dplyr::arrange(desc(altmetrics.score)) %>%
  dplyr::filter(altmetrics.score > 0)

df_alt_top20 <- df_alt %>%
  dplyr::slice_head(n = 20)

if(write){
  data.table::fwrite(df_alt, "dados/df_alt.csv")
  data.table::fwrite(df_alt_top20, "dados/df_alt_top20.csv")
}
# rm(df_dimensions, df_dimensions_sa)
## Países Parquet  ---------------------------------

df_country <- arrow::read_parquet("dados/country_code-Copy1") %>%
  dplyr::select(doi) %>%
  dplyr::group_by(doi) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::distinct(doi, .keep_all = T) %>%
  dplyr::ungroup()

glimpse(df_country)

rm(df_country)

## Título ----
max_lenght = 1
for(i in 1:nrow(df_dimensions)){
  # stringr::str_count(df_dimensions[[8]][i])
  col_length <- stringr::str_count(df_dimensions[[8]][i])
  #print(col_length)
  if(col_length > max_lenght){
    col_name <- i
    max_lenght <- col_length
  }
}
## 1. Criação de tabelas  ---------------------------------
## quais campos preciso? id +
## artigos_autores -> 
## artigos_cit -> 
## evol_pub_tipo -> date_normal, type
## wordcloud -> title.preferred, abstract.preferred
## paises_pub -> research_org_country_names
## categ_pub -> categories.for_v1.first_level.codes
## 
library(dplyr)

# df_dimensions <- fst::read_fst("dados/dimensions_compressed.fst") %>%
#   dplyr::select(id, doi, authors_fn = authors,  authors_ln =  `authors/lastname`, metrics.times_cited, altmetrics.score, date_normal, type, title.preferred, abstract.preferred,
#                 research_org_country_names, categories.for_v1.first_level.codes)
df_dimensions <- tibble::as_tibble(df_dimensions) %>%
  dplyr::rename(authors_fn = authors,  authors_ln =  `authors/lastname`)

set.seed(424242)
df_dimensions_sample <- df_dimensions %>% 
  dplyr::sample_frac(0.01)

### 1.1 Países  ---------------------------------
## Forma antiga, antes precisava de todos os países, agora,
## tanto para países quanto autores pegarei apenas primeiro e último
# ## Recebe apenas a coluna de paises
# df_paises <- df_dimensions %>%
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

df_dimensions_country <- df_dimensions %>%
  dplyr::select(id, research_org_country_names) %>%
  dplyr::filter(research_org_country_names != "")  %>%
  dplyr::mutate(first_country = fct_first_country(research_org_country_names)) %>%
  dplyr::mutate(last_country = fct_last_country(research_org_country_names)) %>%
  dplyr::select(-research_org_country_names) %>%
  dplyr::mutate(last_country = if_else(first_country == last_country, "NoCountry",
                                       last_country))

### 1.2 Tipo e data   ---------------------------------

## arredondando para mês (que é como vai ser exibido nos gráficos)
df_dimensions_type_date <- df_dimensions %>%
  dplyr::filter(date_normal < lubridate::ymd("2021-05-24")) %>% ##data da última extração
  dplyr::filter(date_normal > "2020-01-01") %>% ##não pegar arquivos que possuem apenas ano para não distorcer gráfico
  dplyr::mutate(date = lubridate::floor_date(date_normal, "month")) %>%
  dplyr::select(id, date, type)



df_dimensions_type_date_db <- df_dimensions_type_date %>%
  dplyr::group_by(type, date) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup()
### 1.3 Nomes, separando primeiro e último   ---------------------------------
library(stringr)

## If NA, significa que tem apenas um autor, posso puxar de last author
# first_author <- str_extract(df_dimensions_sample_min$authors_ln, '[^|]+') 
# 
# last_author <- sub(".*\\|", "", df_dimensions_sample_min$authors_ln)
# 
## Funções para pegar primeiro autor e último autor
func_first_author <- function (x){
  str_extract(x, '[^|]+')
}
func_last_author <- function (x){
  sub(".*\\|", "", x)
}
func_trans_names <- function(x){
  stringi::stri_trans_general(x, "Latin-ASCII")
}

df_dimensions_authors <- df_dimensions %>%
  dplyr::select(id, authors_fn, authors_ln) %>%
  dplyr::filter((authors_ln != "vazio") | (authors_fn != "vazio")) %>%
  ## transliteração de nomes, funciona para caractéres próximos do nosso alfabeto
  ## não vai funcionar pra outras línguas (ex: árabe e chinês)
  dplyr::mutate(authors_fn = func_trans_names(authors_fn)) %>%
  dplyr::mutate(authors_ln = func_trans_names(authors_ln)) %>%
  ## extraindo apenas nome (nome e sobrenome) do primeiro autor
  dplyr::mutate(first_author_fn = func_first_author(authors_fn)) %>%
  dplyr::mutate(first_author_ln = func_first_author(authors_ln)) %>%
  ## extraindo apenas nome (nome e sobrenome) do último autor
  dplyr::mutate(last_author_fn = func_last_author(authors_fn)) %>%
  dplyr::mutate(last_author_ln = func_last_author(authors_ln)) %>%
  dplyr::select(-authors_fn, -authors_ln) %>%
  ## removendo duplicatas, quando tem apenas um autor
  dplyr::mutate(last_author_ln = if_else(first_author_ln == last_author_ln, "vazio",
                                         last_author_ln)) %>%
  dplyr::mutate(last_author_fn = if_else(first_author_fn == last_author_fn, "vazio",
                                         last_author_fn)) %>%
  ## Adicionando identificador de letra, para futura tradução de nome (nomes asiáticos,
  ## russos e derivados, e em idioma arábico)
  dplyr::mutate(first_author_fn_typeofchar = stringi::stri_enc_mark(first_author_fn)) %>%
  dplyr::mutate(first_author_ln_typeofchar = stringi::stri_enc_mark(first_author_ln)) %>%
  dplyr::mutate(last_author_fn_typeofchar = stringi::stri_enc_mark(last_author_fn)) %>%
  dplyr::mutate(last_author_ln_typeofchar = stringi::stri_enc_mark(last_author_ln))
  ## Precisa testar as quatro variáveis, primeiro e último autor combinados com primeiro e último nome (2x2)

## Apenas os que precisarão de tradução
df_utf8 <- df_dimensions_authors %>%
  dplyr::filter(first_author_ln_typeofchar == "UTF-8")

## Tradução
library(googleLanguageR)

## Verificar quais campos deverão ser traduzidos, por enqaunto apenas um
# df_dimensions_authors <- df_dimensions_authors %>%
#   dplyr::mutate(language = gl_translate_detect(first_author_ln))

# stringi::stri_trans_general("Zażółć gęślą jaźń", "Latin-ASCII")


## 1.4 Países com tipo e data (tabela) ---------------------

df_dim_filter_type_date_country <- full_join(df_dimensions_type_date, df_dimensions_country,
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


## 1.5 Países (manipulando todos) ---------------------------------

library(stringr)

df_country <- df_dimensions %>%
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

if(write)
  data.table::fwrite(df_paises_wider, "dados/df_paises_long.RDS")
# df_paises_wider <- data.table::fread("dados/df_wider_paises.RDS")

## 1.6 Doi - analisando -----
df_dimensions_doi_count <- fst::read_fst("dados/dimensions_compressed.fst") %>%
  dplyr::select(doi) %>%
  dplyr::group_by(doi) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::distinct(doi, .keep_all = T) %>%
  dplyr::ungroup()

df_dimensions_doi_count_1 <- df_dimensions_doi_count %>%
  dplyr::filter(count > 1, doi != "")

sum(df_dimensions_doi_count_1$count)

df_dimensions_doi_dup <- inner_join(df_dimensions, df_dimensions_doi_count_1, by=c("doi"))
# if(write)
#   data.table::fwrite(df_dimensions_doi_dup, "dados/df_dimensions_doi_dup.csv")


# sum(df_dimensions_doi_count$count)

# if(write)
#   data.table::fwrite(df_dimensions_doi_count, "dados/df_dimensions_doi_count.csv")

## 1.7 Tabela - Perguntas e artigos resposta -------------------------
library(dplyr)
source("fct_manip_string_nome_pais_journal.R")
df_dimensions_authors_countries_journal <- df_dimensions |>
  # df_dimensions_authors_countries_journal <- df_dimensions_sample |>
  dplyr::select(id, doi, authors_fn, authors_ln, research_org_country_names,
                journal_lists) |>
  ## nomes
  ## transliteração de nomes, funciona para caractéres próximos do nosso alfabeto
  ## não vai funcionar pra outras línguas (ex: árabe e chinês)
  dplyr::mutate(authors_fn = func_trans_names(authors_fn)) |>
  dplyr::mutate(authors_ln = func_trans_names(authors_ln)) |>
  ## Criando variável para saber quantos autores temos
  dplyr::mutate(count_authors = func_count_numbers(authors_ln, "\\|")) |>
  ## extraindo apenas nome (nome e sobrenome) do primeiro autor
  dplyr::mutate(first_author_fn = func_first(authors_fn, "|")) |>
  dplyr::mutate(first_author_ln = func_first(authors_ln, "|")) |>
  ## extraindo apenas nome (nome e sobrenome) do último autor
  dplyr::mutate(last_author_fn = func_last(authors_fn, "|")) |>
  dplyr::mutate(last_author_ln = func_last(authors_ln, "|")) |>
  ## removendo duplicatas, quando tem apenas um autor
  dplyr::mutate(last_author_ln = if_else(first_author_ln == last_author_ln, "",
                                         paste0(if_else(count_authors > 2, "... ; ", ""),last_author_ln))) |>
  dplyr::mutate(last_author_fn = if_else(first_author_fn == last_author_fn, "",
                                         last_author_fn)) |>
  # paste0(last_author_fn, "..."))) |> -> Se quiser adicionar os ... ao final
  ## paises
  ## Criando variável para saber quantos autores temos
  dplyr::mutate(count_countries = func_count_numbers(research_org_country_names, ";")) |>
  ## Separando países e organizando
  dplyr::mutate(first_country = func_first(research_org_country_names, ";")) |>
  dplyr::mutate(last_country = func_last(research_org_country_names, ";")) |>
  dplyr::mutate(last_country = if_else(first_country == last_country, "",
                                       paste0(if_else(count_countries > 2, "... ; ", ""), last_country))) |>
  dplyr::mutate(first_country = if_else(is.na(first_country), "vazio", first_country)) |>
  dplyr::mutate(last_country = if_else(is.na(last_country), "", last_country)) |>
  ## journal
  ## Criando variável para saber quantos autores temos
  dplyr::mutate(count_journal = func_count_numbers(journal_lists, ";")) |>
  ## Separando países e organizando
  dplyr::mutate(first_journal = func_first(journal_lists, ";")) |>
  dplyr::mutate(last_journal = func_last(journal_lists, ";")) |>
  dplyr::mutate(last_journal = if_else(first_journal == last_journal, "",
                                       paste0(if_else(count_journal > 2, "... ; ", ""), last_journal))) |>
  dplyr::mutate(first_journal = if_else(is.na(first_journal), "vazio", first_journal)) |>
  dplyr::mutate(last_journal = if_else(is.na(last_journal), "", last_journal)) |>
  
  
  dplyr::select(-doi, -research_org_country_names, -journal_lists)


## manipulando para ter mesmo nome de coluna do df_perguntas
# df_perguntas_dict[,1] <- lapply(df_perguntas_dict[,1], gsub, pattern = " ", replacement = "", fixed = T)
# df_perguntas_dict[,1] <- lapply(df_perguntas_dict[,1], gsub, pattern = "B", replacement = "b", fixed = T)

# for(i in 1:68){
#   ##nesse caso, eu sei que as colunas estão ordenadas
#   colnames(df_perguntas)[i+3] <- newnames[i]
# }
# df_dimensions <- dplyr::inner_join(df_dimensions_authors_countries_journal, df_dimensions, by="id") |>
df_tabela_authors_countries_journal <- dplyr::inner_join(df_dimensions_authors_countries_journal, df_dimensions, by="id") |>
  dplyr::mutate(authors_last_name = paste0(first_author_ln, " ; ", last_author_ln))  |> 
  dplyr::mutate(countries = paste0(first_country, " ; ", last_country)) |>
  dplyr::mutate(journals = paste0(first_journal, " ; ", last_journal)) |>
  dplyr::select(id, doi, title.preferred, type, authors_last_name, countries, journals,
                metrics.times_cited, altmetrics.score, abstract.preferred,
                authors_ln = authors_ln.x, research_org_country_names, journal_lists) |>
  dplyr::rowwise() |>
  dplyr::mutate(title_n_char = dplyr::case_when(nchar(title.preferred) > 50 ~
                                                  paste(stringr::str_sub(title.preferred, 1, 50), "..."),
                                                nchar(title.preferred) <= 50 ~ title.preferred))  |>
  dplyr::mutate(abstract_50char = dplyr::case_when(nchar(abstract.preferred) > 50 ~
                                                     paste(stringr::str_sub(abstract.preferred, 1, 50), "..."),
                                                   nchar(abstract.preferred) <= 50 ~ abstract.preferred)) |>
  dplyr::mutate(altmetrics.score = if_else(is.na(altmetrics.score), 0, as.double(altmetrics.score))) |> 
  dplyr::ungroup() 

df_dimensions_authors_countries_journal <- df_tabela_authors_countries_journal
## Organizando para ficar melhor apresentável
df_dimensions_authors_countries_journal$authors_ln <- stringr::str_replace_all(df_dimensions_authors_countries_journal$authors_ln, "vazio", "-")
df_dimensions_authors_countries_journal$authors_ln <- stringr::str_replace_all(df_dimensions_authors_countries_journal$authors_ln, "\\|", "; ")
df_dimensions_authors_countries_journal$authors_last_name <- stringr::str_replace(df_dimensions_authors_countries_journal$authors_last_name, "vazio ;", "-")
df_dimensions_authors_countries_journal$countries <- stringr::str_replace(df_dimensions_authors_countries_journal$countries, "vazio ;", "-")
df_dimensions_authors_countries_journal$journals <- stringr::str_replace(df_dimensions_authors_countries_journal$journals, "vazio ;", "-")

## Organizando para impressão
df_dimensions_authors_countries_journal <- df_dimensions_authors_countries_journal |> 
  dplyr::select(authors_last_name, title_n_char, abstract_50char, journals, countries, type,
                doi, citations = metrics.times_cited, altmetrics = altmetrics.score,
                authors_ln, title = title.preferred, abstract = abstract.preferred,
                journal_lists, research_org_country_names, id)

if(write)
  data.table::fwrite(df_dimensions_authors_countries_journal, "dados/df_dimensions_tabelas_clean.csv")

df_dimensions_authors_countries_journal <- data.table::fread("dados/df_dimensions_tabelas_clean.csv") %>%
  dplyr::select(id, doi, title_n_char, type, authors_last_name, countries, metrics.times_cited,
                altmetrics.score, abstract_50char, title.preferred, abstract.preferred, authors_ln,
                research_org_country_names, journal_lists)

df_perguntas <- data.table::fread("dados/buscaCompleta2305.csv") %>%
  dplyr::select(-abstract.preferred, -title.preferred)
df_perguntas_dict <- data.table::fread("dados/Relacao_clean.csv")

newnames = df_perguntas_dict$Pergunta

df_dimensions_ij_perguntas <- dplyr::inner_join(df_dimensions_authors_countries_journal, df_perguntas, by="id") %>%
  dplyr::select(-V1, -id, doi = doi.x)

###
# Como é feito no app
arvore_no_sel = 'What are the prodromal symptoms in COVID-19?'
col_name <- df_perguntas_dict %>%
  dplyr::filter(Pergunta == arvore_no_sel) %>%
  dplyr::select(Busca)
df_dimensions_ij_perguntas_search <- df_dimensions_ij_perguntas %>%
    dplyr::filter(!!as.name(col_name$Busca) == '1')
## Escrevendo uma tabela exemplo
if(write)
  data.table::fwrite(df_dimensions_ij_perguntas_search, "dados/df_dimensions_ij_perguntas_search.csv")
# ###
# # Para o app_tabela
# col_name <- df_perguntas_dict[df_perguntas_dict$Pergunta == input$question]$Busca
# df_dimensions_ij_perguntas_search <- df_dimensions_ij_perguntas %>%
#   dplyr::filter(!!as.name(col_name) == '1')
# ###
# 
rm(df_perguntas, df_dimensions_ij_perguntas,
   df_perguntas_dict, newnames, df_dimensions_authors_countries_journal,
   df_dimensions_authors_countries)

### 1.7.1 Tabela - Analisando periódico -------------------------

df_dimensions_journal <- df_dimensions %>%
  dplyr::select(id, journal_lists, journal.title, journal.id, journal.issn, journal.eissn,
                publisher.id, publisher.name)

skimr::skim(df_dimensions_journal)

## 1.8 Tabela citações ---------------------------------
# Vou pegar a tabela gerada anteriormente e apenas selecionar os campos desejados
df_citacoes_ordered <- df_dimensions_authors_countries_journal %>%
  dplyr::select(doi, authors_last_name, metrics.times_cited, altmetrics.score, title.preferred, authors_ln) %>%
  dplyr::arrange(desc(metrics.times_cited))

df_count_autores_ordered_top20 <- df_citacoes_ordered %>%
  dplyr::slice_head(n = 20)
if(write){
  data.table::fwrite(df_citacoes_ordered, "dados/df_citacoes_ordered.csv")
  data.table::fwrite(df_count_autores_ordered_top20, "dados/df_citacoes_ordered_top20.csv")
}
if(plot){
  DT::datatable(df_citacoes_ordered,
                options = list(columnDefs = list(list(visible=FALSE, targets=c(5, 6))),
                               rowCallback = DT::JS(
                                 "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                                 "var full_text_author = aData[6]",
                                 "var full_text_title = aData[5]",
                                 "$('td:eq(1)', nRow).attr('title', full_text_title);",
                                 "$('td:eq(2)', nRow).attr('title', full_text_author);",
                                 "}"
                                 # "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                                 # "var full_text_title = aData[9]",
                                 # "$('td:eq(7)', nRow).attr('title', full_text_title);",
                                 # "}"
                               ))
  )
}

## 1.9 Publicações por categoria (ANZSRC_FoR)  ---------------------------------

df_dim_categories <- df_dimensions %>%
  dplyr::select(id, categories.for_v1.first_level.codes) %>%
  dplyr::filter(length(categories.for_v1.first_level.codes) > 2) %>%
  dplyr::filter(stringr::str_detect(categories.for_v1.first_level.codes,"[0123456789]"))

df_dim_categories_clean <- df_dim_categories |> 
  dplyr::rename(categ = categories.for_v1.first_level.codes) |> 
  dplyr::mutate(categ = sapply(categ, stringr::str_replace_all, "'", "")) |> 
  dplyr::mutate(categ = sapply(categ, stringr::str_replace_all, " ", "")) |> 
  dplyr::mutate(categ = sapply(categ, stringr::str_replace_all, "\\[", "")) |> 
  dplyr::mutate(categ = sapply(categ, stringr::str_replace_all, "\\]", ""))

## calculando o número máximo de categorias
nmax <- max(stringr::str_count(df_dim_categories_clean$categ, ",")) + 1

df_dim_categories_manip <- df_dim_categories_clean |> 
  tidyr::separate(categ, paste0("categ_", seq_len(nmax)), ",")
df_dim_categories_manip <- df_dim_categories_manip |> 
  tidyr::pivot_longer(!id, names_to = "categs_", values_to = "categ") %>%
  dplyr::select(-categs_) %>%
  dplyr::filter(!is.na(categ))

## 2.0 Novas tabelas de busca ----

df_buscas <- arrow::read_parquet("dados/Banco1909PerguntasAmostra.parquet")
df_buscas_relacao <- data.table::fread("dados/relacaoColunaPergunta.csv") |> 
  dplyr::select(col_name = `Nome da Coluna`, col_name_plus_abs = `Nome da Coluna Abs`, perg = Pergunta)

# perg_sel <- "Child and Adolescent Health and Education" ##Não existe no df tree
# perg_sel <- "Does pregnancy increase the risk for severe COVID-19?"
# "Can breast milk transmit SARS-CoV-2 to newborns?"
# "Can mode of delivery transmit SARS-CoV-2 to neonates?"
# "What are the precautions for breastfeeding for a SARS-CoV-2 positive mother?"
# "What are the safest methods for neonatal feeding for a SARS-CoV-2 positive mother?"
# "Women's Health and Gender" <- QUERIE

pergs_topics_sel <- c("Does pregnancy increase the risk for severe COVID-19?",
                      "Can breast milk transmit SARS-CoV-2 to newborns?",
                      "Can mode of delivery transmit SARS-CoV-2 to neonates?",
                      "What are the precautions for breastfeeding for a SARS-CoV-2 positive mother?",
                      "What are the safest methods for neonatal feeding for a SARS-CoV-2 positive mother?",
                      "Women's Health and Gender")

df_buscas_relacao_filtered <- df_buscas_relacao |> 
  dplyr::filter(perg %in% pergs_topics_sel)

df_buscas_relacao_filtered[[2]][[1]]
buscas_tit <- NULL
df_buscas_filtered <- NULL
df_merge <- data.frame()
df_merge <- df_buscas |> 
  dplyr::filter(!!as.name(buscas_tit[1]) == 1) |> 
  dplyr::select(id, doi_busc = doi, !!as.name(buscas_tit[1]))
# df_merge <- colnames(df_merge)
for(i in 2:nrow(df_buscas_relacao_filtered)){
  buscas_tit[i] <- df_buscas_relacao_filtered[[2]][[i]]
  
  df_buscas_filtered <- df_buscas |> 
    dplyr::filter(!!as.name(buscas_tit[i]) == 1) |> 
    dplyr::select(id, doi_busc = doi, !!as.name(buscas_tit[i]))
  
  df_merge <- dplyr::inner_join(df_merge, df_buscas_filtered)
}
df_buscas_relacao_filtered <- df_buscas_relacao |> 
  dplyr::filter(perg == perg_sel)

buscas_tit <-  df_buscas_relacao_filtered$col_name
buscas_tit_abs <-  df_buscas_relacao_filtered$col_name_plus_abs

df_buscas_filtered <- df_buscas |> 
  dplyr::filter(!!as.name(buscas_tit) == 1) |> 
  dplyr::select(id, doi_busc = doi, !!as.name(buscas_tit))

# df_dim_au_co_jo <- data.table::fread("dados/app/df_dimensions_tabelas_clean.csv")
# df_dim_au_co_jo <- fst::write_fst(df_dim_au_co_jo, "dados/app/df_dimensions_tabelas_clean.fst")
df_dim_au_co_jo <- fst::read_fst("dados/app/df_dimensions_tabelas_clean.fst")

df_dim_au_co_jo_filtered <- dplyr::inner_join(df_dim_au_co_jo, df_buscas_filtered, by="id") |> 
  dplyr::select(-!!as.name(buscas_tit), -doi_busc)

## 3.0 Testes de junção perguntas e base ----

# library(data.table)
# library(dtplyr)
# library(dplyr, warn.conflicts = FALSE)

df_tabela_base_filtros_plus_categ <- fst::read_fst("dados/app/df_tabela_base_plus_categ.fst")

df_tabela_perguntas <- fst::read_fst("dados/app/df_dimensions_tabelas_clean.fst")


ano_sel <- "2021"
tipo_sel <- c("article", "preprint", "book")
# unique(df_tabela_base_filtros_plus_categ$country)
pais_sel <- c("Brazil", "Canada", "United Kingdom", "Japan", "United States", "Argentina", "South Africa", "Spain", "Russia", "China", "Germany", "Haiti", "Italy")

## seleção de linhas
ids <- df_tabela_base_filtros_plus_categ |> 
  dplyr::filter(lubridate::year(date) == ano_sel, type %in% tipo_sel, country %in% pais_sel) |> 
  dplyr::pull(id)

## tabela filtrada com filtros
df_tabela_perguntas_filtered <- df_tabela_perguntas |> 
  dplyr::filter(id %in% ids)


df_buscas <- arrow::read_parquet("dados/Banco1909PerguntasAmostra.parquet")
df_buscas_relacao <- as.data.frame(data.table::fread("dados/relacaoColunaPergunta.csv")) |> 
  dplyr::select(col_name = `Nome da Coluna`, col_name_plus_abs = `Nome da Coluna Abs`, perg = Pergunta)
df_perguntas <- data.table::fread("dados/perguntas_full_clean.csv")

perg_sel <- "Does pregnancy increase the risk for severe COVID-19?"

df_buscas_relacao_filtered <- df_buscas_relacao |> 
  dplyr::filter(perg == perg_sel)

buscas_tit <-  df_buscas_relacao_filtered$col_name
buscas_tit_abs <-  df_buscas_relacao_filtered$col_name_plus_abs

df_buscas_filtered <- df_buscas |> 
  dplyr::filter(!!as.name(buscas_tit) == 1) |> 
  dplyr::select(id, doi_busc = doi, !!as.name(buscas_tit))

df_tabela_perg_filt <- dplyr::inner_join(df_tabela_perguntas_filtered, df_buscas_filtered, by="id") |> 
  dplyr::select(-!!as.name(buscas_tit), -doi_busc)

## Escrevendo exemplo pra teste no app (teste em analises ainda)
if(write_test)
  fst::write.fst(df_tabela_perg_filt, "dados/df_tabela_perg_filt.fst")

# df_dimensions_authors_countries_journal <- data.table::fread("dados/df_dimensions_tabelas_clean.csv")

set.seed(424242)

df_count_perg_filt <- df_tabela_perg_filt |> 
  dplyr::select(id, countries, type, date = date_normal)
df_tabela_perg_filt <- df_tabela_perg_filt |>
  # dplyr::select(authors_ln, tittle_50char, journals, countries, type)
  dplyr::select(authors_last_name, title_n_char, abstract_50char, journals, countries,
                type, doi, citations, altmetrics, authors_ln, title, abstract,
                journal_lists, research_org_country_names, date_normal) |> 
  dplyr::rename(`Autor(es)` = authors_last_name, `Título` = title_n_char,
                `Resumo` = abstract_50char, `Revista` = journals,
                `País` = countries, `Tipo` = type, doi = doi,
                `Citações` = citations, `Altmetria` = altmetrics,
                `Autor(es) nome completo` = authors_ln,
                `Título completo` =  title,`Resumo completo` = abstract,
                `Revistas completo` = journal_lists,
                `Países completo` = research_org_country_names,
                `Data` = date_normal)

DT::datatable(df_tabela_perg_filt[,1:15],
              extensions = c("Buttons", "Responsive"),
              options = list(autoWidth = T,
                             columnDefs = list(list(visible=FALSE, targets=c(10, 11, 12, 13, 14, 15)),
                                               list(width = '200px', targets = c("2"))),
                             rowCallback = DT::JS(
                               "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                               "var full_text_author = aData[10]",
                               "var full_text_title = aData[11]",
                               "var full_text_abs = aData[12]",
                               "var full_text_journals = aData[13]",
                               "var full_text_countries = aData[14]",
                               "$('td:eq(1)', nRow).attr('title', full_text_author);",
                               "$('td:eq(2)', nRow).attr('title', full_text_title);",
                               "$('td:eq(3)', nRow).attr('title', full_text_abs);",
                               "$('td:eq(4)', nRow).attr('title', full_text_journals);",
                               "$('td:eq(5)', nRow).attr('title', full_text_countries);",
                               "}"
                               # "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                               # "var full_text_title = aData[9]",
                               # "$('td:eq(7)', nRow).attr('title', full_text_title);",
                               # "}"
                             ),
                             dom = 'Bfrtip',
                             buttons =
                               list(list(
                                 extend = 'collection',
                                 buttons = c('csv', 'excel', 'pdf'),
                                 text = 'Baixar tabela'
                               ))
              )
)


# ## 4.0 Tratar banco de dados de perguntas ----
# library(readxl)
# xl_data <- "dados/arvore_perguntas_versoes.xlsx"
# 
# # Before reading data, we will return the names of the sheets for later use:
# readxl::excel_sheets(path = xl_data)
# 
# # We will now read in just the quakes data. First, specifying by sheet name, then by number
# 
# arvore <- readxl::read_excel(path = xl_data, sheet = "arvore_app_5")
# 
# data.table::fwrite(arvore, "dados/arvore_perguntas_16_11.csv")





# ## 5.0 Contando eixos/tópicos/consultas/perguntas ----

df_perguntas <- data.table::fread("dados/arvore_perguntas_16_11.csv")

df_perguntas_1 <- df_perguntas |> 
  dplyr::filter(EIXO == "Virus and Pathogenesis")

unique(df_perguntas$EIXO)
unique(df_perguntas_1$TOPICO)
unique(df_perguntas_1$CONSULTA)
unique(df_perguntas_1$PERGUNTA)









