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
library(thematic)
library(shinipsum)


thematic_shiny(font = "auto")

# Define UI for application that draws a histogram
ui <- tagList(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "navbar.css")
    ),
    navbarPage(
        # theme = bslib::bs_theme(
        #     version = 3,
        #     bg = "#ecf0f5", fg = "#660909", primary = "#64C1C7",
        #     # bslib also makes it easy to import CSS fonts
        #     base_font = bslib::font_google("Comfortaa")
        #     # base_font = bslib::font_link("Comfortaa", href = "https://fonts.googleapis.com/css2?family=Comfortaa&display=swap"),
        #     # code_font = bslib::font_link("Comfortaa", href = "https://fonts.googleapis.com/css2?family=Comfortaa&display=swap"),
        #     # heading_font = bslib::font_link("Comfortaa", href = "https://fonts.googleapis.com/css2?family=Comfortaa&display=swap")
        # ),
        #tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css"),
    
        div(img(src='marca_ric_covid.svg',style="margin-top: 0px; padding-right:5px;padding-bottom:5px", width = 380)) |>
            tagAppendAttributes(class = 'logo'),
               #theme = "journal",
               windowTitle = "Repositório de informações sobre COVID-19",
               id = "navbar",
               #selected = "pub_temp_loc",
               theme = "navbar.css",
               #fluid = T,
               tabPanel("Sobre", id = "sobre"),
               tabPanel("Publicações no tempo",
                        id = "pub_temp_loc",
                        sidebarLayout(
                            sidebarPanel(
                                id = "sidebar",
                                selectInput("idAno", "Selecione o ano:", choices = c("Todos os anos", 2020, 2021)),
                                selectInput("idPais", "Selecione o país:", choices = c("Todos os países", "Brasil", "Argentina", "Chile", "Estados Unidos", "Uruguai")),
                                selectInput("idPub", "Selecione o tipo de publicação:", choices = c("Todos os tipos", "Artigo", "Capítulo", "Livro", "Monografia", "Preprint"))
                            ),
                            mainPanel(
                                plotOutput("distPlot")
                            )
                            
                        )),
               tabPanel("Citações e altimetria",
                        id = "cit_alt")
               )

    # Sidebar with a slider input for number of bins
        # sidebarMenu(id = "sidebar",
        #                  selectInput("idAno", "Selecione o ano:", choices = c("Todos os anos", 2020, 2021)),
        #                  selectInput("idPais", "Selecione o país:", choices = c("Todos os países", "Brasil", "Argentina", "Chile", "Estados Unidos", "Uruguai")),
        #                  selectInput("idPub", "Selecione o tipo de publicação:", choices = c("Todos os tipos", "Artigo", "Capítulo", "Livro", "Monografia", "Preprint")))
                    
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
