#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(dqshiny)
library(rhandsontable)
library(shinipsum)
library(shiny)
library(jsonlite)
library(bslib)
library(plotly)
library(thematic)

countries <- c('Brazil', 'France', 'Germany', 'USA')
questions <- c("What are the effective pre-exposure prophylactics (PreP) for COVID?",
               "How much/Has social distancing had an impact on slowing the spread of COVID-19?",
               "How much/What is the impact of school closure in handling the COVID-19?",
               "How/How much OPENING AND CLOSING POLICY DECISIONS influence trends in COVID-19 cases?")

comboTreeInput <- function(inputId, width = "30%", height = "100px", 
                           choices, multiple = FALSE, cascaded = TRUE){
    tags$div(style = sprintf("width: %s; height: %s;", width, height),
             tags$input(id = inputId, class = "comboTree", type = "text", 
                        placeholder = "Select",
                        `data-choices` = as.character(toJSON(choices, auto_unbox = TRUE)),
                        `data-multiple` = ifelse(multiple, "true", "false"), 
                        `data-cascaded` = ifelse(cascaded, "true", "false")
             )
    )
}

solar_theme <- bs_theme(
    
)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = solar_theme,
    navbarPage("COVID-19/CIDACS",
               tabPanel("Publicações ao longo do tempo e local",
                        id = "pub_temp_loc",
                        sidebarLayout(
                            sidebarPanel(
                                width = 2,
                                selectizeInput(
                                    'e5', '5. Max number of items to select', choices = c("Todos os anos", "2020", "2021"),
                                    multiple = TRUE, options = list(maxItems = 1)
                                )
                            ),
                            mainPanel(
                                width = 10,
                                fluidRow(
                                    tags$h4("Perguntas"),
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
                                               ),
                                               prettyToggle(
                                                   inputId = "na",
                                                   label_on = "NAs keeped",
                                                   label_off = "NAs removed",
                                                   icon_on = icon("check"),
                                                   icon_off = icon("remove")
                                               )
                                           ))
                                ),
                                fluidRow(
                                    shiny::uiOutput("dinamic_ui_content")
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
    output$plot3 <- renderPlot({
        random_ggplot()
    })
    output$plot4 <- renderPlot({
        random_ggplot()
    })
    
    output$tabela <- renderDataTable({
        shinipsum::random_DT()
    })
    
    output$dinamic_ui_content <- renderUI({
        fluidRow(
            column(width=6,
                   shinycssloaders::withSpinner(plotlyOutput("plot")),
                   plotlyOutput("plot2")
            ),
            column(width=6,
                   shinycssloaders::withSpinner(plotOutput("plot3")),
                   plotOutput("plot4")
            ),
            dataTableOutput("tabela")
        )
    })
    
}

thematic_shiny(font = "auto")
# Run the application 
shinyApp(ui = ui, server = server)
