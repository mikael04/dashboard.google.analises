#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bs4Dash)
library(ggplot2)
library(shinyWidgets)
library(shinipsum)
library(shiny)
library(jsonlite)
library(bslib)
library(plotly)
library(thematic)
library(bigrquery)
library(DBI)
library(dplyr)
library(shiny.i18n)
source("fct_filtrar_dim.R")

# i18n <- Translator$new(translation_csvs_path = "data-raw/translations/")
i18n <- Translator$new(translation_json_path = "dados/translation_json/translations.json")


# countries <- c('Brazil', 'France', 'Germany', 'USA')



# comboTreeInput <- function(inputId, width = "30%", height = "100px", 
#                            choices, multiple = FALSE, cascaded = TRUE){
#     tags$div(style = sprintf("width: %s; height: %s;", width, height),
#              tags$input(id = inputId, class = "comboTree", type = "text", 
#                         placeholder = "Select",
#                         `data-choices` = as.character(toJSON(choices, auto_unbox = TRUE)),
#                         `data-multiple` = ifelse(multiple, "true", "false"), 
#                         `data-cascaded` = ifelse(cascaded, "true", "false")
#              )
#     )
# }

solar_theme <- bs_theme(
  
)

func_continent_paises <- function(continent_sel, df_continentes_paises) {
  continente_sel_paises <- df_continentes_paises %>%
    dplyr::filter(Continent == continent_sel) %>%
    dplyr::select(Country) %>%
    dplyr::pull()
  return(continente_sel_paises)
}

languages_choices <- function() {
  languages <- c("pt-br", "en-us")
  
  flags <- c(
    # "img/flags/br.png",
    # "img/flags/us.png"
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/br.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg"
  )
  language_options <-mapply(languages, flags, FUN = function(languages, flagUrl) {
    HTML(paste(
      tags$img(src=flagUrl, width=20, height=15),
      languages
    ))
  }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  languages_ <-list(languages, language_options)
  # languages_[[1]]
  # languages_[[2]]
  return(languages_)
}

languages <- c("pt-br", "en-us")

flags <- c(
  "img/flags/br.png",
  "img/flags/us.png"
  # "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/br.svg",
  # "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg"
)

languages_opt <- languages_choices()
# languages_opt[[1]]
# Define UI for application that draws a histogram

## Criando navbar tabs e navbarmenu -----
navbarTab <- function(tabName, ..., icon = NULL) {
  tags$li(
    class = "nav-item",
    tags$a(
      class = "nav-link",
      id = paste0("tab-", tabName),
      href = paste0("#shiny-tab-", tabName),
      `data-toggle` = "tab",
      `data-value` = tabName,
      icon,
      tags$p(...)
    )
  )
}


navbarMenu <- function(..., id = NULL) {
  if (is.null(id)) id <- paste0("tabs_", round(stats::runif(1, min = 0, max = 1e9)))
  
  tags$ul(
    class = "nav dropdown", 
    role = "menu",
    id = "sidebar-menu",
    ...,
    div(
      id = id,
      class = "sidebarMenuSelectedTabItem",
      `data-value` = "null",
      
    )
  )
}

# UI ----------------------------------------------------------------------


ui <- fluidPage(
  tags$head(),
  shinyjs::useShinyjs(),
  
  # You have to include this one-liner CSS
  # tags$style(".fullwidth { width: 100% !important; }"),
  theme = solar_theme,
  dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(),
    dashboardBody(
      # Boxes need to be put in a row (or column)
      fluidRow(
        box(plotOutput("plot1", height = 250)),
        
        box(
          title = "Controls",
          sliderInput("slider", "Number of observations:", 1, 100, 50)
        )
      )
    )
  )
  
)


# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  r <- reactiveValues()
  r$sidebar = 0
  observeEvent(input$hide_show_Sidebar, {
    if (isolate(r$sidebar)) {
      # In all these shinyjs functions, you need to use the same ID as you used in the div in the UI
      shinyjs::show(select = "#page_pub_temp_loc > div > div:nth-child(1)")
      shinyjs::removeClass(class = "fullwidth",
                           select = "#page_pub_temp_loc > div > div:nth-child(2)")
      r$sidebar = 0
      print(r$sidebar)
    } else {
      r$sidebar = 1
      print(r$sidebar)
      shinyjs::hide(select = "#page_pub_temp_loc > div > div:nth-child(1)")
      shinyjs::addClass(class = "fullwidth",
                        select = "#page_pub_temp_loc > div > div:nth-child(2)")
    }
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
    
  })
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
  continents <- c("Africa", "America", "Asia", "Europe", "Oceania")
  selection_continents <- c("TODOS", continents)
  countries <- c("TODOS", countries)
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
  
  df_continentes_paises <- data.table::fread("dados/Countries_Continents_americas.csv")
  
  
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
    updateSelectizeInput(session, 'continents', choices = selection_continents, server = TRUE)
    updateSelectizeInput(session, 'continents_2', choices = selection_continents, server = TRUE)
    updateSelectizeInput(session, 'countries', choices = countries, server = TRUE)
  })
  observeEvent(input$continents, {
    countries_continent <- func_continent_paises(input$continents, df_continentes_paises)
    updateSelectizeInput(session, 'countries', choices = countries_continent, server = TRUE)
    
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
  
  output$tabela <- renderDataTable({
    shinipsum::random_DT(10, 5)
  })
  
  output$dinamic_ui_content <- renderUI({
    fluidRow(
      column(width=6,
             shinycssloaders::withSpinner(plotlyOutput("plot",  width = "100%")),
             plotlyOutput("plot2",  width = "100%")
      ),
      column(width=6,
             shinycssloaders::withSpinner(plotlyOutput("plot3",  width = "100%")),
             plotlyOutput("plot4",  width = "100%")
      ),
      # dataTableOutput("tabela")
    )
  })
  # output$server_selectize_countries <- renderUI({
  #     selectizeInput(
  #         'countries', 'Selecione os países', choices =  countries,
  #         multiple = TRUE
  #     )
  # })
  # output$server_selectize_article_type <- renderUI({
  #     selectizeInput(
  #         'article_type', 'Selecione o tipo de artigo', choices = type,
  #         multiple = TRUE, options = list(maxItems = 5)
  #     )
  # })
  # output$server_selectize_date <- renderUI({
  #     selectizeInput(
  #         'date', 'Selecione a data', choices = date,
  #         multiple = TRUE, options = list(maxItems = 5)
  #     )
  # })
  
}

thematic_shiny(font = "auto")
# Run the application 
shinyApp(ui = ui, server = server)
