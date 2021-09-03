library(shiny)
library(shinydashboard)
library(dplyr)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)

gif = "nyancat.gif"
# df_dimensions <- fst::read_fst("dados/dimensions_compressed.fst")
# 
# df_dimensions_filter <- df_dimensions %>%
#   dplyr::select(id, doi, authors_fn = authors,  authors_ln =  `authors/lastname`, metrics.times_cited, altmetrics.score, date_normal, type, title.preferred, abstract.preferred,
#                 research_org_country_names, categories.for_v1.first_level.codes)
# df_dimensions_filter <- tibble::as_tibble(df_dimensions_filter)
# 
# set.seed(424242)
# df_dimensions_filter_sample <- df_dimensions_filter %>%
#   dplyr::sample_frac(0.01)
# 
# func_first_author <- function (x){
#   stringr::str_extract(x, '[^|]+')
# }
# func_last_author <- function (x){
#   sub(".*\\|", "", x)
# }
# func_first_country <- function (x){
#   stringr::str_extract(x, '[^;]+')
# }
# func_last_country <- function (x){
#   sub(".*\\;", "", x)
# }
# func_trans_names <- function(x){
#   stringi::stri_trans_general(x, "Latin-ASCII")
# }
# func_count_numbers <- function(string, pattern){
#   count <- stringr::str_count(string, pattern) + 1
#   return(count)
# }
# 
# # df_dimensions_filter_authors_countries <- df_dimensions_filter %>%
# df_dimensions_filter_authors_countries <- df_dimensions_filter_sample %>%
#   dplyr::select(id, authors_fn, authors_ln, research_org_country_names) %>%
#   ## nomes
#   ## transliteração de nomes, funciona para caractéres próximos do nosso alfabeto
#   ## não vai funcionar pra outras línguas (ex: árabe e chinês)
#   dplyr::mutate(authors_fn = func_trans_names(authors_fn)) %>%
#   dplyr::mutate(authors_ln = func_trans_names(authors_ln)) %>%
#   ## Criando variável para saber quantos autores temos
#   dplyr::mutate(count_authors = func_count_numbers(authors_ln, "\\|")) %>%
#   ## extraindo apenas nome (nome e sobrenome) do primeiro autor
#   dplyr::mutate(first_author_fn = func_first_author(authors_fn)) %>%
#   dplyr::mutate(first_author_ln = func_first_author(authors_ln)) %>%
#   ## extraindo apenas nome (nome e sobrenome) do último autor
#   dplyr::mutate(last_author_fn = func_last_author(authors_fn)) %>%
#   dplyr::mutate(last_author_ln = func_last_author(authors_ln)) %>%
#   ## removendo duplicatas, quando tem apenas um autor
#   dplyr::mutate(last_author_ln = if_else(first_author_ln == last_author_ln, "",
#                                          paste0(if_else(count_authors > 2, "... ; ", ""),last_author_ln))) %>%
#   dplyr::mutate(last_author_fn = if_else(first_author_fn == last_author_fn, "",
#                                          last_author_fn)) %>%
#                                          # paste0(last_author_fn, "..."))) %>% -> Se quiser adicionar os ... ao final
# 
# 
# 
#   #############
#   ## paises
#   ## Criando variável para saber quantos autores temos
#   dplyr::mutate(count_countries = func_count_numbers(research_org_country_names, ";")) %>%
#   ## Separando países e organizando
#   dplyr::mutate(first_country = func_first_country(research_org_country_names)) %>%
#   dplyr::mutate(last_country = func_last_country(research_org_country_names)) %>%
#   dplyr::mutate(last_country = if_else(first_country == last_country, "",
#                                        paste0(if_else(count_countries > 2, "... ; ", ""), last_country))) %>%
#   dplyr::mutate(first_country = if_else(is.na(first_country), "vazio", first_country)) %>%
#   dplyr::mutate(last_country = if_else(is.na(last_country), "", last_country))
# 
# 
# ## manipulando para ter mesmo nome de coluna do df_perguntas
# # df_perguntas_dict[,1] <- lapply(df_perguntas_dict[,1], gsub, pattern = " ", replacement = "", fixed = T)
# # df_perguntas_dict[,1] <- lapply(df_perguntas_dict[,1], gsub, pattern = "B", replacement = "b", fixed = T)
# 
# # for(i in 1:68){
# #   ##nesse caso, eu sei que as colunas estão ordenadas
# #   colnames(df_perguntas)[i+3] <- newnames[i]
# # }
# # df_dimensions <- dplyr::inner_join(df_dimensions_filter_authors_countries, df_dimensions_filter, by="id") %>%
# df_dimensions <- dplyr::inner_join(df_dimensions_filter_authors_countries, df_dimensions_filter_sample, by="id") %>%
#   dplyr::mutate(authors_last_name = paste0(first_author_ln, " ; ", last_author_ln)) %>%
#   dplyr::mutate(countries = paste0(first_country, " ; ", last_country)) %>%
#   dplyr::select(id, title.preferred, type, authors_last_name, countries, metrics.times_cited, abstract.preferred, authors_ln = authors_ln.x,  research_org_country_names = research_org_country_names.x) %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(title_50char = dplyr::case_when(nchar(title.preferred) > 50 ~
#                                                      paste(stringr::str_sub(title.preferred, 1, 50), "..."),
#                                                    nchar(title.preferred) <= 50 ~ title.preferred))  %>%
#   dplyr::mutate(abstract_50char = dplyr::case_when(nchar(abstract.preferred) > 50 ~
#                                                         paste(stringr::str_sub(abstract.preferred, 1, 50), "..."),
#                                                       nchar(abstract.preferred) <= 50 ~ abstract.preferred))
# 
# ## Organizando para ficar melhor apresentável
# df_dimensions$authors_ln <- stringr::str_replace_all(df_dimensions$authors_ln, "vazio", "-")
# df_dimensions$authors_ln <- stringr::str_replace_all(df_dimensions$authors_ln, "\\|", "; ")
# df_dimensions$authors_last_name <- stringr::str_replace(df_dimensions$authors_last_name, "vazio ;", "-")
# df_dimensions$countries <- stringr::str_replace(df_dimensions$countries, "vazio ;", "-")
# 
# data.table::fwrite(df_dimensions, "dados/df_dimensions_tabelas_clean.csv")
df_dimensions <- data.table::fread("dados/df_dimensions_tabelas_clean.csv") %>%
  dplyr::select(id, doi, title_50char, type, authors_last_name, countries, metrics.times_cited,
                altmetrics.score, abstract_50char, title.preferred, abstract.preferred,
                authors_ln, journal_lists)

df_perguntas <- data.table::fread("dados/buscaCompleta2305.csv") %>%
  dplyr::select(-abstract.preferred, -title.preferred)

df_perguntas_dict <- data.table::fread("dados/Relacao_clean.csv")

newnames = df_perguntas_dict$Pergunta

df_dimensions_ij_perguntas <- dplyr::inner_join(df_dimensions, df_perguntas, by="id") %>%
  dplyr::select(-V1, -id, doi = doi.x, -doi.y)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  # Application title
  titlePanel("Busca tabela"),
  # Show a plot of the generated distribution
  mainPanel(
    fluidRow(
      selectInput(
        inputId = "question",
        label = "Question",
        choices = newnames,
        selected = newnames[1],
        multiple = F
      ),
      h3("Pergunta selecionada:"),
      textOutput("sel_question")
    ),
    fluidRow(
      shinycssloaders::withSpinner(DT::DTOutput("table")))
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table <- DT::renderDataTable({
    DT::datatable(df_dimensions_ij_perguntas[,1:11])
  })
  output$sel_question <- renderText({
    "Selecione uma pergunta"
  })
  observeEvent(input$question, {
    ## Como é feito no app
    arvore_no_sel = 'What are the most common/typical symptoms of the CoV disease?'
    # col_name <- df_perguntas_dict %>%
    #   dplyr::filter(Pergunta == arvore_no_sel) %>%
    #   dplyr::select(Busca)
    # df_dimensions_ij_perguntas_search <- df_dimensions_ij_perguntas %>%
    #     dplyr::filter(!!as.name(col_name$Busca) == '1')
    # ## Escrevendo uma tabela exemplo
    # data.table::fwrite(df_dimensions_ij_perguntas_search, "dados/df_dimensions_ij_perguntas_search.csv")
    ## Para teste
    col_name <- df_perguntas_dict[df_perguntas_dict$Pergunta == arvore_no_sel]$Busca
    
    # col_name <- df_perguntas_dict[df_perguntas_dict$Pergunta == input$question]$Busca
    df_dimensions_ij_perguntas_search <- df_dimensions_ij_perguntas %>%
      dplyr::filter(!!as.name(col_name) == '1')
    output$table <- DT::renderDataTable({
      DT::datatable(df_dimensions_ij_perguntas_search[,1:12],
                    options = list(columnDefs = list(list(visible=FALSE, targets=c(8, 9, 10, 11))),
                                   rowCallback = DT::JS(
        "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
        "var full_text_title = aData[8]",
        "var full_text_abs = aData[9]",
        "var full_text_author = aData[10]",
        "var full_text_countries = aData[11]",
        "$('td:eq(2)', nRow).attr('title', full_text_title);",
        "$('td:eq(7)', nRow).attr('title', full_text_abs);",
        "$('td:eq(4)', nRow).attr('title', full_text_author);",
        "$('td:eq(5)', nRow).attr('title', full_text_countries);",
        "}"
        # "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
        # "var full_text_title = aData[9]",
        # "$('td:eq(7)', nRow).attr('title', full_text_title);",
        # "}"
        ))
      )
    })
    
    output$sel_question <- renderText({
      input$question
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
