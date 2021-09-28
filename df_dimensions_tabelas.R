library(dplyr)

## Caso queira testar alguns gráficos e tabelas gerados, dois parâmetros abaixo
plot <- F
dashboard <- F
debug <- F
write <- F
## 1. Lendo arquivo de banco de dados  ---------------------------------
## Setup, lendo a base de dados (pode ser usado qualquer outro formato)
# df_dimensions <- fst::read_fst("dados/dimensions_compressed.fst") |> 
# select(id, doi, date_normal, type, research_org_country_names, raw_affiliations,
#        categories.for_v1.first_level.codes, title.preferred, abstract.preferred,
#        authors, `authors/lastname`, metrics.times_cited, altmetrics.score, journal_lists)

# fst::write_fst(df_dimensions, "dados/dimensions_compressed_selected.fst")

# df_dimensions <- fst::read_fst("dados/dimensions_compressed_selected.fst")
df_dimensions <- arrow::read_parquet("dados/standard_dimensions_19092021_base_variaveis_selecionadas_19092021.parquet")
# fst::write_fst(df_dimensions, "dados/dimensions_compressed_selected.fst")
# df_dimensions <- fst::read_fst("dados/dimensions_compressed_selected.fst")
# df_dimensions_basemik_parquet <- arrow::read_parquet("dados/standard_dimensions_19092021_base_mikael.parquet")
# df_dimensions_basemerg_parquet <- arrow::read_parquet("dados/standard_dimensions_19092021_base_merge.parquet")

# set.seed(424242)
# df_dimensions_sample <- df_dimensions |>  
#   dplyr::sample_frac(0.01)

## 2. Criação de tabelas  ---------------------------------
## quais campos preciso? id +
## artigos_autores -> 
## artigos_cit -> 
## evol_pub_tipo -> date_normal, type
## wordcloud -> title.preferred, abstract.preferred
## paises_pub -> research_org_country_names
## categ_pub -> categories.for_v1.first_level.codes
## 

# df_dimensions <- fst::read_fst("dados/dimensions_compressed.fst") |> 
#   dplyr::select(id, doi, authors_fn = authors,  authors_ln =  `authors/lastname`, metrics.times_cited, altmetrics.score, date_normal, type, title.preferred, abstract.preferred,
#                 research_org_country_names, categories.for_v1.first_level.codes)
df_dimensions <- tibble::as_tibble(df_dimensions) |> 
  dplyr::rename(authors_fn = authors,  authors_ln =  `authors/lastname`)

set.seed(424242)
df_dimensions_sample <- df_dimensions |>  
  dplyr::sample_frac(0.01)

### 2.1 Países  ---------------------------------
## Forma antiga, antes precisava de todos os países, agora,
## tanto para países quanto autores pegarei apenas primeiro e último
# ## Recebe apenas a coluna de paises
# df_paises <- df_dimensions |> 
#   # df_paises <- df_dimensions_sample |> 
#   dplyr::select(id, research_org_country_names) |> 
#   dplyr::filter(research_org_country_names != "")
# paises <- df_paises$research_org_country_names
# ## Separa em uma lista de mais de um elemento quando possui mais de um país
# paises_split <- paises |> 
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
# fct_first_country <- function (x){
#   str_extract(x, '[^;]+') 
# }
# fct_last_country <- function (x){
#   sub(".*;", "", x)
# }

# ## Antigamente era filtrado, porém agora farei com todo o banco
# df_dimensions_country <- df_dimensions |> 
#   dplyr::select(id, research_org_country_names) |> 
#   dplyr::filter(research_org_country_names != "")

df_dimensions_country <- df_dimensions |> 
  dplyr::select(id, research_org_country_names) |> 
  dplyr::mutate(research_org_country_names = if_else(research_org_country_names != "", research_org_country_names, "NoCountry"))

## calculando o número máximo de países
nmax <- max(stringr::str_count(df_dimensions_country$research_org_country_names, ";"), na.rm = T) + 1
## Formato long
df_dimensions_country <- df_dimensions_country |> 
  tidyr::separate_rows(research_org_country_names, sep = ";") |> 
  dplyr::rename(country = research_org_country_names)


### 2.2 Tipo e data   ---------------------------------

## arredondando para mês (que é como vai ser exibido nos gráficos)
df_dimensions_type_date <- df_dimensions |> 
  # dplyr::filter(date_normal < lubridate::ymd("2021-05-24")) |>  ##data da última extração
  dplyr::filter(as.Date(date_normal) > "2020-01-01") |>  ##não pegar arquivos que possuem apenas ano para não distorcer gráfico
  dplyr::mutate(date = lubridate::floor_date(lubridate::as_date(date_normal), "month")) |> 
  dplyr::select(id, date, type)


## 3 Tabela base - Países, data, tipo de artigo   ---------------------------------

## Criando a tabela base para junções posteriores

### 3.1 Tabela completa -----
# com todas as linhas, sem agrupamento, campos: id, paises, data, tipo
# id, country, date, type
df_tabela_base_filtros <- inner_join(df_dimensions_type_date, df_dimensions_country, by="id") |> 
  # dplyr::mutate(first_country = tidyr::replace_na(first_country, "NoCountry")) |> 
  dplyr::mutate(country = tidyr::replace_na(country, "NoCountry")) |>
  dplyr::mutate(type = tidyr::replace_na(type, "NoType"))

### 3.2 Tabela de contagem ----
df_count_base_filtros <- df_tabela_base_filtros |> 
  dplyr::group_by(type, date, country) |> 
  dplyr::summarise(count = n()) |> 
  dplyr::ungroup()

if(remove)
## 4 Junções com as demais variáveis ---------------------------------

### 4.1 Categorias ---------------------------------

func_remove_non_numbers <- function (x){
  # gsub("[:graph:]", "", x)
  out <- gsub("\\[", "", x)
  out <- gsub("\\]", "", out)
  out <- gsub(" ", "", out)
  out <- gsub("'", "", out)
  out
}

df_dimensions_categ <- df_dimensions |> 
  dplyr::select(id, categ = categories.for_v1.first_level.codes)

df_dimensions_categ <- df_dimensions_categ |> 
  dplyr::mutate(categ_clean = func_remove_non_numbers(categ)) |> 
  dplyr::select(-categ) |> 
  dplyr::rename(categ = categ_clean) |> 
  tidyr::separate_rows(categ, sep = "\\,")
# #list <- lapply(paises, stringr::str_replace_all, ";", "0")
# df_dimensions_categ$categ <- mapply(df_dimensions_categ$categ, stringr::str_extract_all("[0-9]"))

#### 4.1.1 Tabela completa -----
# com todas as linhas, sem agrupamento, campos: id, paises, data, tipo
# id, country, date, type
df_tabela_base_plus_categ <- inner_join(df_tabela_base_filtros, df_dimensions_categ, by="id") |> 
  dplyr::mutate(categ = tidyr::replace_na(categ, "00"))

#### 4.1.2 Tabela de contagem ----
df_count_base_plus_categ <- df_tabela_base_plus_categ |> 
  # dplyr::mutate(first_country = tidyr::replace_na(first_country, "NoCountry")) |> 
  # dplyr::mutate(last_country = tidyr::replace_na(last_country, "NoCountry")) |> 
  dplyr::group_by(type, date, country, categ) |> 
  dplyr::summarise(count = n()) |> 
  dplyr::ungroup()

### 4.2 Nomes (Em espera)  ---------------------------------

df_dimensions_authors <- df_dimensions_sample |> 
  dplyr::select(id, doi, authors_fn, authors_ln) |> 
  dplyr::mutate(authors_fn = if_else(authors_fn != "", authors_fn, "vazio")) |> 
  dplyr::mutate(authors_ln = if_else(authors_ln != "", authors_ln, "vazio"))

## calculando o número máximo de primeiros nomes
# nmax <- max(stringr::str_count(df_dimensions_authors$authors_fn, "\\|"), na.rm = T) + 1
# for(i in 1:nrow(df_dimensions_authors)){
#   max_names <- max(stringr::str_count(df_dimensions_authors[[2]][[i]], "\\|"), na.rm = T)
#   if(j < max_names){
#     j = max_names
#     max_authors <- i
#   }
# }

# df_dimensions_authors_top_names <- df_dimensions_authors |> 
#   dplyr::mutate(n_autores = stringr::str_count(authors_fn, "\\|"))

# max(stringr::str_count(df_dimensions_authors[[2]][[32631]], "\\|"))+1
# df_dimensions_authors[[1]][[32631]]

## Formato wide
df_dimensions_authors_ln <- df_dimensions_authors |> 
  tidyr::separate_rows(authors_ln, sep = "\\|")

## Formato wide
df_dimensions_authors_fn <- df_dimensions_authors |> 
  tidyr::separate_rows(authors_fn, sep = "\\|")

# df_dimensions_authors_join <- full_join(df_dimensions_authors_ln, df_dimensions_authors_fn)

## 5 Tabela de resposta   ---------------------------------

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
  dplyr::mutate(title_50char = dplyr::case_when(nchar(title.preferred) > 50 ~
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
  dplyr::select(authors_last_name, title_50char, abstract_50char, journals, countries, type,
                doi, citations = metrics.times_cited, altmetrics = altmetrics.score,
                authors_ln, title = title.preferred, abstract = abstract.preferred,
                journal_lists, research_org_country_names, id)
# if(write)
  # data.table::fwrite(df_dimensions_authors_countries_journal, "dados/df_dimensions_tabelas_clean.csv")
  # data.table::fwrite(df_dimensions_authors_countries_journal_sample, "dados/df_dimensions_tabelas_clean_sample.csv")

# df_dimensions_authors_countries_journal <- data.table::fread("dados/df_dimensions_tabelas_clean.csv")

set.seed(424242)

df_dimensions_authors_countries_journal_sample <- df_dimensions_authors_countries_journal |> 
  # dplyr::select(-id) |> 
  dplyr::slice_sample(prop = 0.01) |> 
  # dplyr::select(authors_ln, tittle_50char, journals, countries, type)
  dplyr::select(authors_last_name, title_50char, abstract_50char, journals, countries,
                type, doi, citations, altmetrics, authors_ln, title, abstract,
                journal_lists, research_org_country_names)
  
DT::datatable(df_dimensions_authors_countries_journal_sample[,1:14],
              options = list(columnDefs = list(list(visible=FALSE, targets=c(10, 11, 12, 13, 14))),
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
                             ))
)
## Apenas os que precisarão de tradução
# df_utf8 <- df_dimensions_authors |> 
#   dplyr::filter(first_author_ln_typeofchar == "UTF-8")

## Tradução
library(googleLanguageR)

## Verificar quais campos deverão ser traduzidos, por enqaunto apenas um
# df_dimensions_authors <- df_dimensions_authors |> 
#   dplyr::mutate(language = gl_translate_detect(first_author_ln))

# stringi::stri_trans_general("Zażółć gęślą jaźń", "Latin-ASCII")