library(shiny)




ui <- fluidPage(
  # sidebarPanel(
  #   prettySwitch(
  #     inputId = "Id027",
  #     label = "", 
  #     fill = TRUE,
  #     bigger = T,
  #     width = NULL
  #   ),
  #   prettySwitch(
  #     inputId = "Id028",
  #     label = "", 
  #     fill = TRUE,
  #     bigger = T,
  #     width = "100px"
  #   ),
  #   prettySwitch(
  #     inputId = "Id029",
  #     label = "", 
  #     fill = TRUE,
  #     bigger = T,
  #     width = "50%"
  #   ),
  #   prettySwitch(
  #     inputId = "Id030",
  #     label = "", 
  #     fill = TRUE,
  #     bigger = T,
  #     width = "100%"
  #   ),
  #   
  #   # Only show this panel if the plot type is a histogram
  #   conditionalPanel(
  #     condition = "input.plotType == 'hist'",
  #     selectInput(
  #       "breaks", "Breaks",
  #       c("Sturges",
  #         "Scott",
  #         "Freedman-Diaconis",
  #         "[Custom]" = "custom")),
  #     
  #     # Only show this panel if Custom is selected
  #     conditionalPanel(
  #       condition = "input.breaks == 'custom'",
  #       sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
  #     )
  #   )
  # )
  fluidRow(
    tabsetPanel(
      type = "tabs",
      id = "tabs_dash",
      tabPanel("Sobre",
               fluidRow(
                 id = "sobre",
                 type="tabs",
                 tabsetPanel(id = "sobre_painel",
                             selected = "O projeto",
                             type="tabs",
                             tabPanel(
                               "O projeto",
                               id = "projeto",
                               fluidRow(
                                 column(width = 12,
                                          box(plotOutput("distPlot6")),
                                          box(plotOutput("distPlot7"))
                                 ),
                                 
                                 column(width = 12,
                                          box(plotOutput("distPlot8")),
                                          box(plotOutput("distPlot9"))
                                 )
                               ),
                               fluidRow(
                               )
                             ),
                             tabPanel(
                               "Metodologia",
                               id = "metodologia"
                             ),
                             tabPanel(
                               "Equipe",
                               id = "equipe"
                             ),
                 )
               )
      ),
      tabPanel("Gráficos")
    )
  )
)

server <- function(input, output) {
  
  output$distPlot6 <- renderPlot({
    shinipsum::random_ggplot("bar")
  })
  output$distPlot7 <- renderPlot({
    shinipsum::random_ggplot("bar")
  })
  output$distPlot8 <- renderPlot({
    shinipsum::random_ggplot("bar")
  })
  output$distPlot9 <- renderPlot({
    shinipsum::random_ggplot("bar")
  })
  
}

shinyApp(ui = ui, server = server)