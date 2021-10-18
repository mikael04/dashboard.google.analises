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
library(shinyTree)
library(shinyWidgets)


thematic_shiny(font = "auto")

# Define UI for application that draws a histogram
ui <- tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "geral.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "navbar.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "sidebar.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "body.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "footer.css"),
    # includeCSS("www/geral.css"),
    tags$head(
    ),
    navbarPage(
        theme = bslib::bs_theme(
            version = 3,
            bg = "#ecf0f5", fg = "#660909", primary = "#000000",
            # bg = "#324C63", fg = "#660909", primary = "#000000",
            # bslib also makes it easy to import CSS fonts
            base_font = bslib::font_google("Comfortaa")
            # base_font = bslib::font_link("Comfortaa", href = "https://fonts.googleapis.com/css2?family=Comfortaa&display=swap"),
            # code_font = bslib::font_link("Comfortaa", href = "https://fonts.googleapis.com/css2?family=Comfortaa&display=swap"),
            # heading_font = bslib::font_link("Comfortaa", href = "https://fonts.googleapis.com/css2?family=Comfortaa&display=swap")
        ),
        #tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css"),
    
        div(img(src='marca_ric_covid.svg',style="margin-top: 0px; padding-right:5px;padding-bottom:5px", width = 380)) |>
            tagAppendAttributes(class = 'logo'),
        
        #theme = "journal",
        windowTitle = "Repositório de informações sobre COVID-19",
        id = "navbar",
        #selected = "pub_temp_loc",
        # theme = "navbar.css",
        #fluid = T,
        tabPanel("Sobre", id = "sobre"),
        tabPanel("Publicações no tempo",
                 id = "pub_temp_loc",
                 sidebarLayout(
                    sidebarPanel(
                            id = "sidebar",
                            div(id="filtro_tit",
                                tags$h4("FILTROS")
                                ),
                            width = 2,
                            selectInput("idAno", "Selecione o ano:",
                                        choices = c("Todos os anos", 2020, 2021),
                                        selectize = T),
                            selectInput("idPais", "Selecione o país:", choices = c("Todos os países", "Brasil", "Argentina", "Chile", "Estados Unidos", "Uruguai")),
                            selectInput("idPub", "Selecione o tipo de publicação:", choices = c("Todos os tipos", "Artigo", "Capítulo", "Livro", "Monografia", "Preprint"))
                    ),
                    mainPanel(
                        width = 10,
                        #### 1.1.2.1 Linha de perguntas ----
                        fluidRow(
                            div(id="perguntas",
                                # actionButton("toggleSidebar", "Toggle sidebar"),
                                tags$h4("PERGUNTAS"),
                                column(3,
                                       # actionButton(
                                       #     inputId = "sel_perg",
                                       #     label = "SELECIONE O ASSUNTO OU PERGUNTA",
                                       #     # inputId = ns("dropdown"),
                                       #     icon = icon("question"),
                                       #     circle = FALSE
                                       # ),
                                       # selectInput("idPerg",
                                       #             "SELECIONE O ASSUNTO OU PERGUNTA",
                                       #             )
                                        dropdownButton(
                                           label = "Selecione o assunto ou pergunta",
                                           width = "100%",
                                           # inputId = ns("dropdown"),
                                           inputId = "dropdown",
                                           icon = icon("question"),
                                           circle = FALSE,
                                           tags$div(
                                               # actionButton(inputId = ns("toggle2"),
                                               actionButton(inputId = "toggle2",
                                                            label = "Selecionar")
                                           ),
                                           tags$div(style = "margin:10px",
                                                    column(2, align="center",
                                                           #mod_arvore_busca_nosel_ui("arvore_busca_nosel_1"),
                                                           # mod_arvore_busca_ui(ns("arvore_busca_1")),
                                                           shinyTree("tree",
                                                                     search=TRUE, searchtime = 1000,
                                                                     theme="proton", themeIcons = FALSE, themeDots = T)
                                                    )
                                           )
                                       )
                                ),
                                column(7,
                                       # mod_arvore_busca_nosel_ui(ns("arvore_busca_nosel_1")
                                       htmlOutput("Pergunta/tema selecionado X"),
                                ),
                            )
                        ),
                        br(),
                        fluidRow(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Plots",
                                                 id="plots",
                                                 column(
                                                     width = 6,
                                                     
                                                     plotOutput("distPlot"),
                                                     plotOutput("distPlot2")
                                                 ),
                                                 column(
                                                     width = 6,
                                                     plotOutput("distPlot3"),
                                                     plotOutput("distPlot4")
                                                 )
                                                 ),
                                        tabPanel("Table",
                                                 id = "table")
                            )
                            
                        )
                    )
                    
                )),
        tabPanel("Citações e altimetria",
                id = "cit_alt")
        ),

        tags$footer(class = "footer",
                    div(class = "text",
                        span(id = "site",
                             "www.cidacs.bahia.fiocruz.br/riccovid"),
                        br(),
                        span(id = "localicazao",
                             "Technological Park of Bahia | Edf. Technocentre, 121 World Street - Trobogy, Salvador - BA, CEP.:41745-715")
                    ),
                    div(class="marcas_svg",
                        img(src='marcas_suporte.svg',style="margin-top: 0px; padding-right:5px;padding-bottom:5px", width = 430) |>
                            tagAppendAttributes(class = 'logo'))
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
        shinipsum::random_ggplot("bar")
    })
    output$distPlot2 <- renderPlot({
        shinipsum::random_ggplot("bar")
    })
    output$distPlot3 <- renderPlot({
        shinipsum::random_ggplot("bar")
    })
    output$distPlot4 <- renderPlot({
        shinipsum::random_ggplot("bar")
    })
    output$distPlot5 <- renderPlot({
        shinipsum::random_ggplot("bar")
    })
    df <- data.table::fread("../dados/perguntas_full_clean.csv")
    tree <- dfToTree(df, c("EIXO", "TOPICS", "QUERIES", "QUESTIONS"))
    output$tree <- renderTree({tree})
}

# Run the application 
shinyApp(ui = ui, server = server)
