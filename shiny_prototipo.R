#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        # shiny.i18n::usei18n(i18n),
    ),
    theme = solar_theme,
    navbarPage(title = "COVID-19/CIDACS",
               id = "navbar_panel",
                   tabPanel("Publicações ao longo do tempo e local",
                            id = "pub_temp_loc",
                            sidebarLayout(
                                sidebarPanel(
                                    width = 2,
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
                                        # tags$h4("Perguntas"),
                                        column(4,
                                               dropdownButton(
                                                   inputId = "mydropdown",
                                                   label = "Controls",
                                                   icon = icon("sliders"),
                                                   status = "primary",
                                                   circle = FALSE,
                                                   sliderInput(
                                                       inputId = "n",
                                                       label = "Number of observations",
                                                       min = 10, max = 100, value = 30
                                                   )
                                               ))
                                    ),
                                    fluidRow(
                                        # tabsetPanel(
                                            "Header",
                                            tabPanel("First",
                                                     id = "first_panel",
                                                     shiny::uiOutput("dinamic_ui_content")),
                                            tabPanel("Second",
                                                     id = "sec_panel",),
                                            tabPanel("Third",
                                                     id = "thrd_panel",)
                                        # )
                                    )
                                    #selected = "pub_temp_loc",
                                    #theme = "boostrap.min.css",
                                    #fluid = T,
                                    #
                                    
                                )
                            )
                            
                   ),
                   tabPanel("Colaborações e financiamento",
                            id = "col_fin"),
                   tabPanel("Citação e altmetria",
                            id = "cit_alt")
               
    )
    
)

# Define server logic required to draw a histogram
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
