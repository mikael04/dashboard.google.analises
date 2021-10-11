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
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
    #tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css"),
    navbarPage(div(img(src='marca_ric_covid.svg',style="margin-top: 0px; padding-right:5px;padding-bottom:5px", width = 300))
               %>% 
                   tagAppendAttributes(class = 'logo'),
               #theme = "journal",
               windowTitle = "Repositório de informações sobre COVID-19",
               id = "navbar",
               #selected = "pub_temp_loc",
               theme = "navbar.css",
               #fluid = T,
                       tabPanel("Publicações ao longo do tempo e local",
                                id = "pub_temp_loc"),
                       tabPanel("Citação e altmetria",
                                id = "cit_alt")
               ),

    # Sidebar with a slider input for number of bins
        sidebarMenu(id = "sidebar",
                         selectInput("idAno", "Selecione o ano:", choices = c("Todos os anos", 2020, 2021)),
                         selectInput("idPais", "Selecione o país:", choices = c("Todos os países", "Brasil", "Argentina", "Chile", "Estados Unidos", "Uruguai")),
                         selectInput("idPub", "Selecione o tipo de publicação:", choices = c("Todos os tipos", "Artigo", "Capítulo", "Livro", "Monografia", "Preprint")))
                    
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
