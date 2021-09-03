library(shiny)
library(ggplot2)
library(shinyWidgets)
library(dqshiny)
library(rhandsontable)
library(shinipsum)
library(shiny)
library(jsonlite)
library(bslib)
library(plotly)
library(thematic)
library(bigrquery)
library(DBI)
library(dplyr)
source("fct_filtrar_dim.R")
source("R/mod_arvore_busca_nosel.R")
source("R/mod_arvore_busca.R")
library(shinyTree)
library(stringr)
library(shinyjs)



ui <- fluidPage(
  theme = bslib::bs_theme(version = 3,
                          bootswatch = "journal"),
  navbarPage(
    id = "navbar_panel",
    "COVID19",
    tabPanel("Publicações ao longo do tempo e local",
             sidebarLayout( 
               sidebarPanel(
                 width = 2,
                 conditionalPanel(condition="input.conditioned == 0",helpText("This is navnarMenu 0")),
                 conditionalPanel(condition="input.conditioned == 1",helpText("This is navnarMenu 1")),
                 conditionalPanel(condition= "input.Guidelines == 2.1 && input.conditioned == 2",helpText("This is navnarMenu 2")),
                 conditionalPanel(condition= "input.Guidelines == 2.2 && input.conditioned == 2",helpText("This is navnarMenu 3")),
                 selectizeInput(
                   'e5', '5. Max number of items to select', choices = c("Todos os anos", "2020", "2021"),
                   multiple = TRUE, options = list(maxItems = 1)
                 ),
                 selectizeInput(
                   'article_type', 'Selecione o tipo de artigo', choices = character(0),
                   multiple = TRUE, options = list(maxItems = 5)
                 ),
                 selectizeInput(
                   'date', 'Selecione a data', choices = character(0),
                   multiple = TRUE, options = list(maxItems = 5)
                 ),
                 selectizeInput(
                   'countries', 'Selecione os países', choices =  character(0),
                   multiple = TRUE
                 )
               ),
               
               mainPanel(
                 width = 10,
                 fluidRow(
                   column(3,
                          dropdownButton(
                            label = ("Selecione o assunto ou pergunta"),
                            width = "100%",
                            # inputId = ns("dropdown"),
                            inputId = "dropdown",
                            icon = icon("question"),
                            circle = FALSE,
                            tags$div(
                              # actionButton(inputId = ns("toggle2"),
                              actionButton(inputId = "toggle2",
                                           label = ("Selecionar"))
                            ),
                            tags$div(style = "margin:10px",
                                     column(2, align="center",
                                            #mod_arvore_busca_nosel_ui("arvore_busca_nosel_1"),
                                            # mod_arvore_busca_ui(ns("arvore_busca_1")),
                                            mod_arvore_busca_ui("arvore_busca_1"),
                                     )
                            )
                          )
                   ),
                   column(7,
                          # mod_arvore_busca_nosel_ui(ns("arvore_busca_nosel_1")
                          mod_arvore_busca_nosel_ui("arvore_busca_nosel_1")
                   )
                 ),
                 fluidRow(
                   tabsetPanel(id='conditioned',
                               tabPanel("About", value=0,
                                        shiny::uiOutput("dinamic_ui_content"),
                               ),                   
                               
                               tabPanel("Tab 1",value=1,
                                        shiny::uiOutput("dinamic_ui_2_content")
                                        ,tabsetPanel(
                                 tabPanel("Tab 1.1"),
                                 tabPanel("Tab 1.2", tabsetPanel(
                                   tabPanel("Tab 1.2.1"),
                                   tabPanel("Tab 1.2.2"))))),
                               
                               tabPanel("Guidelines",value = 2, tabsetPanel( id = "Guidelines",         
                                                                             navbarMenu("Tab 2.1",
                                                                                        tabPanel("Tab 2.1.1",value=2.1),
                                                                                        tabPanel("Tab 2.1.2",value=2.1))))
                               
                   )))
                 )
                 
    ),
    tabPanel("Publicaç",
             id = "pub_2",
             # uiOutput("dinamic_ui_content")
    ),
    tabPanel("Publicações ao lal",
    )
  )
)

server <- function(input, output, session) {
  ## Conectando com o BQ
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "cidacs-ai-covid19-br",
    dataset = "backendDash",
    billing = "cidacs-ai-covid19-br"
  )
  bigrquery::bq_auth(email = "mikael.coletto.eng@gmail.com") # aqui vai ter uma primeira configuração, depois ele usa um token que ele salva
  ## Recebendo a primeira tabela de filtro
  df_filtros = dplyr::tbl(con,"hml_base_filtro") %>% dplyr::collect()
  countries_ <- df_filtros %>%
    dplyr::select(paises) %>%
    dplyr::distinct(paises)
  countries <- countries_$paises
  type_ <- df_filtros %>%
    dplyr::select(tipo = type) %>%
    dplyr::distinct(tipo)
  types <- type_$tipo
  date_ <- df_filtros %>%
    dplyr::select(data = date) %>%
    dplyr::mutate(data = if_else(is.na(data), data, lubridate::floor_date(data, "year"))) %>%
    dplyr::mutate(data_text = if_else(is.na(data), as.character("Sem Data"), as.character(data))) %>%
    dplyr::mutate(data_text =  gsub("-.*$", "", data_text)) %>%
    dplyr::distinct(data, .keep_all = T)
  dates <- date_$data
  
  tipo_pub_sel <- c("article", "book")
  ano_sel <- c("TODOS")
  pais_sel <- c("Argentina", "Austria", "Brazil")
  list_parameters <- list(tipo_pub_sel, ano_sel, pais_sel)
  df_dimensions_selection <- func_filtrar_dim(df_filtros, list_parameters, debug) %>%
    dplyr::rename(date_normal = date, count_date = count) %>%
    dplyr::filter(!is.na(date_normal)) %>%
    # dplyr::mutate(date_normal = format(as.Date(date_normal), "%Y-%m")) %>%
    dplyr::group_by(date_normal, type) %>%
    dplyr::summarise(count = sum(count_date)) %>%
    # dplyr::distinct(date, .keep_all = T) %>%
    dplyr::ungroup()
  
  
  p <- ggplot2::ggplot(data = df_dimensions_selection, aes(x = date_normal, y = count, color = type, group = type,
                                                           text=(paste0('<b>Data:</b>', date_normal, '<br>',
                                                                        '<b>Número de publicações:</b>', count, "<br>",
                                                                        '<b>Tipo:</b>', type)))) + 
    geom_line() +
    theme_minimal() +
    labs(x = ("Tempo"), y = ("Número de artigos"),
         title = ("Publicações ao longo do tempo, por tipo"),
         colour = ("Tipo"))
  
  p <- ggplotly(p, tooltip = "text") %>%
    plotly::config(modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d", "toggleSpikelines"), displaylogo = FALSE)
  
  
  observe({
    updateSelectizeInput(session, 'article_type', choices = types, server = TRUE)
    updateSelectizeInput(session, 'date', choices = dates, server = TRUE)
    updateSelectizeInput(session, 'countries', choices = countries, server = TRUE)
  })
  # updateSelectizeInput(session, 'countries', choices = cbind(name = rownames(countries),countries),
  #                      server = TRUE)
  # updateSelectizeInput(session, 'article_type', choices = type, server = TRUE)
  # updateSelectizeInput(session, 'date', choices = date, server = TRUE)
  
  output$plot <- renderPlotly({
    plotly::ggplotly(random_ggplot())
  })
  
  output$plot2 <- renderPlotly({
    fig <- plot_ly(
      x = c("giraffes", "orangutans", "monkeys"),
      y = c(20, 14, 23),
      name = "SF Zoo",
      type = "bar"
    )
    fig
  })
  output$plot3 <- renderPlotly({
    p
  })
  output$plot4 <- renderPlotly({
    ggplotly(random_ggplot())
  })
  
  output$plot5 <- renderPlotly({
    ggplotly(random_ggplot())
  })
  output$plot6 <- renderPlotly({
    ggplotly(random_ggplot())
  })
  output$plot7 <- renderPlotly({
    ggplotly(random_ggplot())
  })
  output$plot8 <- renderPlotly({
    ggplotly(random_ggplot())
  })
  
  output$tabela <- renderDataTable({
    shinipsum::random_DT(10, 5)
  })
  
  output$dinamic_ui_content <- renderUI({
    fluidRow(
      column(width=6,
             shinycssloaders::withSpinner(plotlyOutput("plot")),
             plotlyOutput("plot2")
      ),
      column(width=6,
             shinycssloaders::withSpinner(plotlyOutput("plot3")),
             plotlyOutput("plot4")
      ),
    )
  })
  output$dinamic_ui_2_content <- renderUI({
    fluidRow(
      column(width=6,
             shinycssloaders::withSpinner(plotlyOutput("plot5")),
             plotlyOutput("plot6")
      ),
      column(width=6,
             shinycssloaders::withSpinner(plotlyOutput("plot7")),
             plotlyOutput("plot8")
      ),
    )
  })
}

shinyApp(ui = ui, server = server)
