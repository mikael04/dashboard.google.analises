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

ui <- fluidPage(
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
                   tabsetPanel(id='conditioned',
                               tabPanel("About", value=0,
                                        shiny::uiOutput("dinamic_ui_content"),
                               ),                   
                               
                               tabPanel("Tab 1",value=1,tabsetPanel(
                                 tabPanel("Tab 1.1"),
                                 tabPanel("Tab 1.2", tabsetPanel(
                                   tabPanel("Tab 1.2.1"),
                                   tabPanel("Tab 1.2.2"))))),
                               
                               tabPanel("Guidelines",value = 2, tabsetPanel( id = "Guidelines",         
                                                                             navbarMenu("Tab 2.1",
                                                                                        tabPanel("Tab 2.1.1",value=2.1),
                                                                                        tabPanel("Tab 2.1.2",value=2.1)),
                                                                             
                                                                             navbarMenu("Tab 2.2",
                                                                                        tabPanel("Tab 2.2.1",value=2.2),
                                                                                        tabPanel("Tab 2.2.2",value=2.2))))
                               
                   )))
                 )
                 
    ),
    tabPanel("Publicaç",
    ),
    tabPanel("Publicações ao lal",
    )
  )
)

server <- function(input, output, session) {
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
    ggplotly(random_ggplot())
  })
  output$plot4 <- renderPlotly({
    ggplotly(random_ggplot())
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
}

shinyApp(ui = ui, server = server)
