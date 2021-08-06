#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- dashboardPage(
        dashboardHeader(
            title = "Cidacs/COVID19",
            titleWidth = 300
        ),
        dashboardSidebar(
            width = 300,
            
            sidebarMenu(id = "sidebar",
                        width = 400,
                        menuItem("Filtros",
                                 tabName = "filtros"),
                        conditionalPanel(condition = 'input.sidebar == "filtros"',
                                         selectInput("idAno", "Selecione o ano:", choices = c("Todos os anos", 2020, 2021)),
                                         selectInput("idPais", "Selecione o país:", choices = c("Todos os países", "Brasil", "Argentina", "Chile", "Estados Unidos", "Uruguai")),
                                         selectInput("idPub", "Selecione o tipo de publicação:", choices = c("Todos os tipos", "Artigo", "Capítulo", "Livro", "Monografia", "Preprint"))
                        ),
                        menuItem("Perguntas",
                                 tabName = "perguntas"
                        )
            )
        ),
        dashboardBody(
            fluidPage(id = "eixos",
                      radioGroupButtons(
                          inputId = "Id069",
                          choices = c(`<i class='fas fa-clock'> Publicações ao longo do tempo</i>` = "Aba_1",
                                      `<i class='fas fa-money-check-alt'> Colaboração e Financiamento</i>` = "Aba_2",
                                      `<i class='fas fa-quote-right'> Citação e altmetria</i>` = "Aba3"),
                          justified = TRUE
                      )
                     )
            
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
