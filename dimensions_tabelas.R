library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
## 0. Variáveis Globais ----
## Caso queira rodar algum tipo de teste e ver suas saídas
debug <- F
## Caso queira escrever as tabelas que forem geradas
write <- T
## Gerar apenas as que serão usadas pelo app, se F vai gerar todas
## Quando for gerar para o app, vai mandar para pasta dados/app/
write_app <- T
## Escrever os primeiros plots em formato svg
write_first_plots <- F
## Escrever gráficos de visão inicial
write_rds <- F
## Se remove = T, vai removendo bases e variáveis que não serão mais usadas,
## para usar menos RAM durante a execução
remove <- F
## Se teste = T, vai rodar com base de dados de sample (1% da base)
teste <- T
## 1. Lendo arquivo de banco de dados  ---------------------------------
## Setup, lendo a base de dados (pode ser usado qualquer outro formato)
# df_dimensions <- fst::read_fst("dados/dimensions_compressed.fst") |> 
# select(id, doi, date_normal, type, research_org_country_names, raw_affiliations,
#        categories.for_v1.first_level.codes, title.preferred, abstract.preferred,
#        authors, `authors/lastname`, metrics.times_cited, altmetrics.score, journal_lists)

# fst::write_fst(df_dimensions, "dados/dimensions_compressed_selected.fst")

df_dimensions <- fst::read_fst("dados/dimensions_compressed_selected.fst")
# df_dimensions <- arrow::read_parquet("dados/standard_dimensions_19092021_base_variaveis_selecionadas_19092021.parquet")
# df_dimensions <- arrow::read_parquet("dados/dimensions2812_gzip.parquet")
# df_dimensions <- arrow::read_parquet("dados/base_merge_2812.parquet.gzip")
df_dimensions <- tibble::as_tibble(df_dimensions) |> 
  dplyr::rename(authors_fn = authors,  authors_ln =  `authors/lastname`)

set.seed(424242)
df_dimensions_sample <- df_dimensions |>  
  dplyr::sample_frac(0.01)

if(teste){
  df_dimensions <- df_dimensions_sample
}

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

### 2.3 Tabelas para filtros laterais (ano, países, tipo de artigo) ----

#### 2.3.1 Países ----
distinct_countries <- df_dimensions_country |> 
  dplyr::distinct(country) |> 
  dplyr::arrange(country)

distinct_countries <- distinct_countries |> 
  dplyr::mutate(country = dplyr::if_else(is.na(country), "NÃO DEFINIDO", country))

data.table::fwrite(distinct_countries, "dados/app/filtros_paises.csv")

#### 2.3.1 Tipos ----
distinct_tipos <- df_dimensions_type_date |> 
  dplyr::distinct(type) |> 
  as.data.frame()

data.table::fwrite(distinct_tipos, "dados/app/filtros_tipos.csv")

#### 2.3.1 Anos ----
distinct_anos <- df_dimensions_type_date |>
  dplyr::mutate(date = year(lubridate::floor_date(lubridate::as_date(date), "year"))) |> 
  dplyr::distinct(date) |> 
  as.data.frame()

data.table::fwrite(distinct_anos, "dados/app/filtros_anos.csv")

rm(distinct_anos, distinct_countries, distinct_tipos)
## 3. Tabela base - Países, data, tipo de artigo   ---------------------------------
### 3.0 Gráficos ----
  # - Mapa de publicações
  # - Publicações no tempo
  # - Publicações por país
## Criando a tabela base para junções posteriores

### 3.1 Tabela completa -----
# com todas as linhas, sem agrupamento, campos: id, paises, data, tipo
# id, country, date, type
df_tabela_base_filtros <- inner_join(df_dimensions_type_date, df_dimensions_country, by="id") |> 
  # dplyr::mutate(first_country = tidyr::replace_na(first_country, "NoCountry")) |> 
  dplyr::mutate(country = tidyr::replace_na(country, "NoCountry")) |>
  dplyr::mutate(type = tidyr::replace_na(type, "NoType"))

if(write){
  if(write_app){ ## Não precisa ser gerada para o app
    if(write_rds){
      
    }else{
      fst::write.fst(df_tabela_base_filtros, "dados/app/df_tabela_base_filtros.fst") 
      # data.table::fwrite(df_tabela_base_filtros, "dados/app/df_tabela_base_filtros.csv") 
    }
  }else{ ## Não está gerando para o app
    if(write_rds){
      
    }else{
      fst::write.fst(df_tabela_base_filtros, "dados/df_tabela_base_filtros.fst")
    }
  }
}

### 3.2 Tabela de contagem ----
df_count_base_filtros <- df_tabela_base_filtros |> 
  dplyr::group_by(type, date, country) |> 
  dplyr::summarise(count = n()) |> 
  dplyr::ungroup()

if(write){
  if(write_app){ ##As que ficarem "repetidas" no ifelse deverão ser geradas de quaisquer forma
    if(write_rds){
      
    }else{
      fst::write.fst(df_count_base_filtros, "dados/app/df_count_base_filtros.fst")
    }
  }else{ ## Não está gerando para o app
    if(write_rds){
      
    }else{
      fst::write.fst(df_count_base_filtros, "dados/df_count_base_filtros.fst")
    }
  }
}

if(remove){
  rm(df_count_base_filtros, df_dimensions_country, df_dimensions_type_date)
}
## 4. Junções com as demais variáveis ---------------------------------
### 4.0 Gráficos ----
    # - Publicações por categoria;
    # - Autores mais citados;
    # - Tabela de artigos, (doi, autor último nome, citações e altimetria)

### 4.1 Categorias ---------------------------------
  # - Publicações por categoria;
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
  dplyr::mutate(categ = tidyr::replace_na(categ, "00")) |> 
  dplyr::mutate(categ_aux = as.numeric(categ))

## Tabela ANZSRC
df_anzsrc <- data.table::fread("dados/ANZSRC_FoR.csv")
# df_anzsrc$ID
df_tabela_base_plus_categ <- dplyr::left_join(df_tabela_base_plus_categ, df_anzsrc,
                                              by=c("categ_aux" = "ID")) |> 
  dplyr::mutate(categ_name = paste0(categ, " - ", FoR))|> 
  dplyr::mutate(categ_name = dplyr::if_else(is.na(FoR), "NoCateg", categ_name)) |> 
  dplyr::select(-categ_aux, -FoR) 

if(write){
  if(write_app){ ##
    if(write_rds){
      
    }else{
      fst::write.fst(df_tabela_base_plus_categ, "dados/app/df_tabela_base_plus_categ.fst")
    }
  }else{ ## Não está gerando para o app
    if(write_rds){
      
    }else{
      fst::write.fst(df_tabela_base_plus_categ, "dados/df_tabela_base_plus_categ.fst")
    }
  }
}
#### 4.1.2 Tabela de contagem ----
df_count_base_plus_categ <- df_tabela_base_plus_categ |> 
  dplyr::group_by(type, date, country, categ) |> 
  dplyr::mutate(count = n()) |>
  dplyr::distinct(type, date, country, categ, .keep_all = T) |> 
  dplyr::ungroup()

df_count_base_plus_categ_first_plot <- df_count_base_plus_categ |> 
  dplyr::filter(!categ == "00") |> 
  dplyr::group_by(categ) |> 
  dplyr::mutate(count_ = sum(count)) |> 
  dplyr::distinct(count_, .keep_all = T) |> 
  dplyr::ungroup() |> 
  dplyr::select(-count, -id, -date, -type, -country) |> 
  dplyr::rename(count = count_)

if(write){
  if(write_app){ ##As que ficarem "repetidas" no ifelse deverão ser geradas de quaisquer forma
    if(write_rds){
      
    }else{
      fst::write.fst(df_count_base_plus_categ, "dados/app/df_count_base_plus_categ.fst")
      fst::write.fst(df_count_base_plus_categ_first_plot, "dados/app/df_count_base_plus_categ_first_plot.fst")
    }
  }else{ ## Não está gerando para o app
    if(write_rds){
      
    }else{
      fst::write.fst(df_count_base_plus_categ, "dados/df_count_base_plus_categ.fst")
    }
  }
}
if(remove){
  rm(df_count_base_plus_categ, df_dimensions_categ, df_tabela_base_plus_categ,
     func_remove_non_numbers)
}
### 4.2 Nomes  ---------------------------------
  # - Autores mais citados;
  # - Tabela de artigos, (doi, autor último nome, citações e altimetria)
df_dimensions_authors <- df_dimensions |> 
  dplyr::select(id, doi, authors_ln) |> 
  dplyr::mutate(authors_ln = if_else(authors_ln != "", authors_ln, "vazio"))

## calculando o número máximo de últimos nomes
# nmax <- max(stringr::str_count(df_dimensions_authors$authors_ln, "\\|"), na.rm = T) + 1
# for(i in 1:nrow(df_dimensions_authors)){
#   max_names <- max(stringr::str_count(df_dimensions_authors[[2]][[i]], "\\|"), na.rm = T)
#   if(j < max_names){
#     j = max_names
#     max_authors <- i
#   }
# }

# df_dimensions_authors_top_names <- df_dimensions_authors |> 
#   dplyr::mutate(n_autores = stringr::str_count(authors_ln, "\\|"))

# max(stringr::str_count(df_dimensions_authors[[2]][[32631]], "\\|"))+1
# df_dimensions_authors[[1]][[32631]]

## Formato wide
df_dimensions_authors <- df_dimensions_authors |> 
  tidyr::separate_rows(authors_ln, sep = "\\|")

#### 4.2.1 Tabela completa -----
# com todas as linhas, sem agrupamento, campos: id, paises, data, tipo
# id, country, date, type
df_tabela_base_plus_name <- inner_join(df_tabela_base_filtros, df_dimensions_authors, by="id") |> 
  dplyr::mutate(authors_ln = dplyr::if_else(authors_ln == "vazio", " ", authors_ln)) |> 
  dplyr::select(-doi)

if(write){
  if(write_app){ ## Não precisa ser gerada para o app
    if(write_rds){
      
    }else{
      fst::write.fst(df_tabela_base_plus_name, "dados/app/df_tabela_base_plus_name.fst")
    }
  }else{ ## Não está gerando para o app
    if(write_rds){
      
    }else{
      fst::write.fst(df_tabela_base_plus_name, "dados/app/df_tabela_base_plus_name.fst")
    }
  }
}
#### 4.2.2 Tabela de contagem ----
## Removendo "vazio" para plot
df_count_base_plus_name <- df_tabela_base_plus_name |> 
  dplyr::filter(authors_ln != " ") |>
  dplyr::group_by(authors_ln) |> 
  dplyr::mutate(count = n()) |>
  dplyr::distinct(type, date, country, authors_ln, .keep_all = T) |> 
  dplyr::ungroup()

df_count_base_plus_name_first_plot <- df_count_base_plus_name |> 
  dplyr::group_by(authors_ln) |> 
  dplyr::mutate(count_ = sum(count)) |> 
  dplyr::distinct(count_, .keep_all = T) |> 
  dplyr::ungroup() |> 
  dplyr::select(-count, -id, -date, -type, -country) |> 
  dplyr::rename(count = count_)

if(write){
  if(write_app){ ##As que ficarem "repetidas" no ifelse deverão ser geradas de quaisquer forma
    if(write_rds){
      
    }else{
      fst::write.fst(df_count_base_plus_name, "dados/app/df_count_base_plus_name.fst")
      fst::write.fst(df_count_base_plus_name_first_plot, "dados/app/df_count_base_plus_name_first_plot.fst")
    }
  }else{ ## Não está gerando para o app
    if(write_rds){
      
    }else{
      fst::write.fst(df_count_base_plus_name, "dados/df_count_base_plus_name.fst")
    }
  }
}
if(remove){
  rm(df_dimensions_authors, df_tabela_base_plus_name, df_count_base_plus_name)
}
## 5. Tabela de resposta   ---------------------------------
  
source("fct_manip_string_nome_pais_journal.R")

## Arrumando a data
df_dimensions_authors_countries_journal <- df_dimensions |>
  dplyr::filter(as.Date(date_normal) > "2020-01-01") |>  ##não pegar arquivos que possuem apenas ano para não distorcer gráfico
  dplyr::mutate(date = lubridate::floor_date(lubridate::as_date(date_normal), "month")) 
  

df_dimensions_authors_countries_journal <- df_dimensions_authors_countries_journal |>
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
                authors_ln = authors_ln.x, research_org_country_names, journal_lists,
                date_normal) |>
  dplyr::rowwise() |>
  dplyr::mutate(title_n_char = dplyr::case_when(nchar(title.preferred) > 100 ~
                                                  paste(stringr::str_sub(title.preferred, 1, 100), "..."),
                                                nchar(title.preferred) <= 100 ~ title.preferred))  |>
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

## Adicionando coluna de categoria
df_tabela_base_plus_categ <- fst::read.fst("dados/app/df_tabela_base_plus_categ.fst") |> 
  dplyr::select(id, categ, categ_name)
df_dim_aut_coun_jou <- inner_join(df_dimensions_authors_countries_journal, df_tabela_base_plus_categ, by="id")
## Organizando para impressão
df_dim_aut_coun_jou <- df_dim_aut_coun_jou |> 
  dplyr::select(authors_last_name, title_n_char, abstract_50char, journals, countries, type,
                doi, citations = metrics.times_cited, altmetrics = altmetrics.score,
                authors_ln, title = title.preferred, abstract = abstract.preferred,
                journal_lists, research_org_country_names, id, date_normal, categ, categ_name)
if(write){
  if(write_app){ ##As que ficarem "repetidas" no ifelse deverão ser geradas de quaisquer forma
    if(teste){ ##Trabalhando com sample
      if(write_rds){
        saveRDS(df_dim_aut_coun_jou, "dados/app/df_dimensions_tabelas_clean")
        # saveRDS(df_dim_aut_coun_jou, "dados/app/df_dimensions_tabelas_clean_sample")
      }else{
        # data.table::fwrite(df_dim_aut_coun_jou, "dados/app/df_dimensions_tabelas_clean.csv")
        fst::write.fst(df_dim_aut_coun_jou, "dados/app/df_dimensions_tabelas_clean.fst")
      }
    }else{
      if(write_rds){
        saveRDS(df_dim_aut_coun_jou, "dados/app/df_dimensions_tabelas_clean.rds")
      }else{
        fst::write.fst(df_dim_aut_coun_jou, "dados/app/df_dimensions_tabelas_clean.fst")
      }
    }
  }else{ ## Não está gerando para o app
    if(teste){ ##Trabalhando com sample
      if(write_rds){
        
      }else{
        fst::write.fst(df_dim_aut_coun_jou, "dados/app/df_dimensions_tabelas_clean.fst")
        # data.table::fwrite(df_dim_aut_coun_jou, "dados/app/df_dimensions_tabelas_clean_sample.csv")
        
      }
    }else{
      if(write_rds){
        
      }else{
        fst::write.fst(df_dim_aut_coun_jou, "dados/app/df_dimensions_tabelas_clean.fst")
      }
    }
  }
}


if(remove){
  rm(df_dimensions_authors_countries_journal, df_tabela_authors_countries_journal,
     df_dimensions_authors_countries_journal_sample, df_dim_aut_coun_jou,
     func_trans_names, func_count_numbers, func_first, func_first_author,
     func_first_country, func_first_journal, func_last, func_last_author,
     func_last_country, func_last_journal)
}

## Apenas os que precisarão de tradução
# df_utf8 <- df_dimensions_authors |> 
#   dplyr::filter(first_author_ln_typeofchar == "UTF-8")

## Tradução
library(googleLanguageR)

## Verificar quais campos deverão ser traduzidos, por enqaunto apenas um
# df_dimensions_authors <- df_dimensions_authors |> 
#   dplyr::mutate(language = gl_translate_detect(first_author_ln))

# stringi::stri_trans_general("Zażółć gęślą jaźń", "Latin-ASCII")
# 6. Gráficos iniciais ----
library(dplyr)

if(write_first_plots){
  ##***************************************************##
  ### 6.1 Mod paises pub ----
  df_count_base_plus_categ_filtros <- fst::read.fst("dados/app/df_count_base_plus_categ.fst")
  # df_tabela_base_filtros <- fst::read_fst("dados/app/df_tabela_base_filtros.fst")
  
  # df_mod_paises_pub_first_plot <- df_tabela_base_filtros |>
  df_mod_paises_pub_first_plot <- df_count_base_plus_categ_filtros |> 
    dplyr::filter(!is.na(country) & country != "NoCountry") |>
    dplyr::group_by(country) |>
    dplyr::summarise(Count = sum(count)) |>
    dplyr::arrange(desc(Count)) |>
    dplyr::slice_head(n = 20) |>
    dplyr::ungroup() |> 
    dplyr::rename(Paises = country)
  
  data.table::fwrite(df_mod_paises_pub_first_plot, "dados/first_plots/df_mod_paises_pub_first_plot.csv")
  ##***************************************************##
  
  ##***************************************************##
  ### 6.2 Mod map pub ----
  
  df_mod_map_pub_first_plot <- df_count_base_plus_categ_filtros |>
    dplyr::filter(country != "NoCountry") |> 
    dplyr::group_by(country) |> 
    dplyr::mutate(count_paises = sum(count)) |> 
    dplyr::distinct(country, .keep_all = T) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(count_paises) |>
    dplyr::select(NAME = country, count = count_paises)
  
  data.table::fwrite(df_mod_map_pub_first_plot, "dados/first_plots/df_mod_map_pub_first_plot.csv")
  
  ##***************************************************##
  ### 6.3 Mod evol pub tipo ----
  
  df_mod_evol_pub_tipo_first_plot <- df_count_base_plus_categ_filtros |>
    dplyr::filter(!is.na(date)) |>
    # dplyr::mutate(date_normal = format(as.Date(date_normal), "%Y-%m")) |>
    dplyr::group_by(date, type) |>
    dplyr::summarise(count_date_type = sum(count)) |>
    # dplyr::distinct(date, .keep_all = T) |>
    dplyr::ungroup() |> 
    dplyr::rename(count = count_date_type)
  
  data.table::fwrite(df_mod_evol_pub_tipo_first_plot, "dados/first_plots/df_mod_evol_pub_tipo_first_plot.csv")
  
  ##***************************************************##
  
  ##***************************************************##
  ### 6.4 Mod categ pub ----
  
  df_mod_categ_pub_first_plot <- df_count_base_plus_categ_filtros |>
    dplyr::filter(!categ == "00") |> 
    dplyr::group_by(categ) |> 
    dplyr::mutate(count_ = sum(count)) |> 
    dplyr::distinct(count_, .keep_all = T) |> 
    dplyr::ungroup() |> 
    dplyr::select(-count, -id, -date, -type, -country ) |>
    dplyr::rename(Count = count_, Category = categ_name)
  
  data.table::fwrite(df_mod_categ_pub_first_plot, "dados/first_plots/df_mod_categ_pub_first_plot.csv")
  
  ##***************************************************##
  
  ##***************************************************##
  ### 6.5 Mod artigos autores ----
  
  df_count_base_plus_name_filtros <- fst::read.fst("dados/app/df_count_base_plus_name.fst")
  
  df_mod_artg_aut_first_plot <- df_count_base_plus_name_filtros |>
    dplyr::group_by(authors_ln) |> 
    dplyr::summarise(count = sum(count)) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(desc(count)) |>  
    dplyr::rename(Autores = authors_ln, Count = count) |> 
    dplyr::slice_head(n = 20)
  
  data.table::fwrite(df_mod_artg_aut_first_plot, "dados/first_plots/df_mod_artg_aut_first_plot.csv")
  
  ##***************************************************##
}

## 7. Cálculo de publicações por eixo, tópico, subtópico e pergunta ----

filename <-"dados/arvore_busca.xlsx"

sheets <- openxlsx::getSheetNames(filename)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=filename)
names(SheetList) <- sheets
sheets
col <- 4
df_arvore <- as.data.frame(SheetList[col]) |>
  dplyr::select(TOPICO = cli_perguntas.TOPICO,
                SUBTOPICO = cli_perguntas.CONSULTA, 
                PERGUNTA = cli_perguntas.PERGUNTA) |> 
  dplyr::mutate(TOPICO = dplyr::if_else(is.na(TOPICO), "Outras perguntas neste eixo", TOPICO)) |> 
  dplyr::mutate(SUBTOPICO = dplyr::if_else(is.na(SUBTOPICO), "Outras perguntas neste tópico", SUBTOPICO)) 


