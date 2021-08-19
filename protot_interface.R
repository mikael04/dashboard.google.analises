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

library(shiny.semantic)

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

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4,
                   bootswatch = "minty"),
  bootstrapPage("COVID-19/CIDACS",
                navbarPage("COVID-19/CIDACS",
                           tabPanel("Publicações ao longo do tempo e local",
                                    id = "pub_temp_loc",
                                    sidebarLayout(
                                      sidebarPanel(
                                        width = 2,
                                        selectizeInput(
                                          'e5', '5. Max number of items to select', choices = state.name,
                                          multiple = TRUE, options = list(maxItems = 2)
                                        )
                                      ),
                                      mainPanel(
                                        width = 10,
                                        fluidRow(plotlyOutput("plotly")),
                                        fluidRow(
                                                                column(width=6,
                                                                       shinycssloaders::withSpinner(plotlyOutput("plot")),
                                                                       plotOutput("plot2")
                                                                ),
                                                                column(width=6,
                                                                       shinycssloaders::withSpinner(plotOutput("plot3")),
                                                                       plotOutput("plot4")
                                                                ))
                                                     
                                         )
                                      )
                                    ),
                           tabPanel("Colaborações e financiamento",
                                    id = "col_fin"),
                           tabPanel("Citação e altmetria",
                                    id = "cit_alt")
                )
             )
  )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$plotly <- renderPlotly({
    plotly::ggplotly(random_ggplot())
  })
  output$plot <- renderPlotly({
    plotly::ggplotly(random_ggplot())
  })
  output$plot2 <- renderPlot({
    random_ggplot()
  })
  output$plot3 <- renderPlot({
    random_ggplot()
  })
  output$plot4 <- renderPlot({
    random_ggplot()
  })
  output$table <- renderTable({
    random_table(10, 5)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
