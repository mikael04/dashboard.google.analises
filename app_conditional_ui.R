library(shiny)
library(shinyWidgets)
library(shinipsum)

sidebar_ <- sidebarPanel(
  id = "sidebar",
  width = 2,
  ### 1.1.1 Filtros laterais ----
  tags$h4(("Filtros")),
  uiOutput(("server_selectize_article_type_pub_tempo")),
  uiOutput(("server_selectize_date_pub_tempo")),
  tags$div(class = "country_filter",
           tags$p("Selecione o(s) país(es)")),
  uiOutput(("server_selectize_countries_pub_tempo")),
  # selectizeInput(
  #   'article_type', ('Selecione o tipo de artigo'), choices = character(0),
  #   multiple = TRUE, options = list(maxItems = 5)
  # ),
  # selectizeInput(
  #   'date', ('Selecione a data'), choices = character(0),
  #   multiple = TRUE, options = list(maxItems = 5)
  # ),
  # selectizeInput(
  #   'countries', ('Selecione os países'), choices =  character(0),
  #   multiple = TRUE
  # ),
  actionButton(inputId = "act_btn_filtros_perg", label = (("Atualizar gráficos"))),
  # selectizeInput('selected_language',
  #                # ("Change language"),
  #                ("Mude o idioma"),
  #                choices = i18n$get_languages(),
  #                selected = "pt-br"),
  
  pickerInput("selected_language", ("Selecione o idioma"),
              multiple = F,
              choices = c("en-us", "pt-br"),
              selected = "pt-br"
  )
)

ui <- navbarPage(
  "Teste",
  id = "navbarPage",
  tabPanel("Publicações ao longo do tempo e local",
           id = "pub_temp_loc",
           sidebarLayout(
             # sidebarPanel(
             #   pickerInput("picket", "selecione",
             #               choices = c("a", "b"))
             # ),
              uiOutput("sidebar_"),
              mainPanel(
                # tabsetPanel(id = "pub_temp_tabs",
                            plotOutput("hist")
                # ),
                # tabsetPanel(id = "tabs_2",
                #             plotOutput("hist2")
                #             ), 
              )
            )
  ),
  tabPanel("Citações",
             id = "cit",
             sidebarLayout(
               sidebarPanel(
                 pickerInput("picker", "selecione mn",
                             choices = c("1", "2"))
               ),
               # uiOutput("sidebar_"),
               mainPanel(
                 # tabsetPanel(id = "pub_temp_tabs",
                 plotOutput("hist2")
                 # ),
                 # tabsetPanel(id = "tabs_2",
                 #             plotOutput("hist2")
                 #             ), 
               )
             )
  )
)

server <- function(input, output, session) {
  observeEvent(input$dist, {
    updateTabsetPanel(inputId = "params", selected = input$dist)
  }) 
  sample <- reactive({
    switch(input$dist,
           normal = rnorm(input$n, input$mean, input$sd),
           uniform = runif(input$n, input$min, input$max),
           exponential = rexp(input$n, input$rate)
    )
  })
  output$hist <- renderPlot({
    shinipsum::random_ggplot()
  })
  output$hist2 <- renderPlot({
    shinipsum::random_ggplot()
  })
  # output$hist2 <- renderPlot(hist(sample()), res = 96)
  
  types = c("TODOS", "article", "preprint")
  output$server_selectize_article_type_pub_tempo <- renderUI({
    selectizeInput(
      'article_type', ('Selecione o(s) tipo(s) de publicação(ões):'), choices = types,
      multiple = TRUE, options = list(maxItems = 5)
    )
  })
  selection_continents <- c("TODOS", "America", "Europa")
  output$sidebar_ <- renderUI({
    h2("Selecione o(s) país(es)")
    dropdown(
      label = ("TODOS"),
      # inputId = ns("dropdown"),
      inputId = "dropdown",
      circle = FALSE,
      pickerInput(
        'continents', 'Selecione o(s) continente(s)', choices =  selection_continents,
        selected = "TODOS",
        multiple = TRUE
      ),
      selectizeInput(
        'countries', 'Selecione o(s) país(es)', choices =  character(0),
        multiple = TRUE
      ),
      
    )
  })
  date <- c("TODOS", "2020", "2021")
  output$server_selectize_date_pub_tempo <- renderUI({
    selectizeInput(
      'date', ('Selecione o(s) ano(s):'), choices = date,
      multiple = TRUE, options = list(maxItems = 1)
    )
  })
  
  output$sidebar_ <- renderUI({
    sidebarPanel(
      id = "sidebar",
      width = 2,
      ### 1.1.1 Filtros laterais ----
      tags$h4(("Filtros")),
      uiOutput(("server_selectize_article_type_pub_tempo")),
      uiOutput(("server_selectize_date_pub_tempo")),
      tags$div(class = "country_filter",
               tags$p("Selecione o(s) país(es)")),
      uiOutput(("server_selectize_countries_pub_tempo")),
      # selectizeInput(
      #   'article_type', ('Selecione o tipo de artigo'), choices = character(0),
      #   multiple = TRUE, options = list(maxItems = 5)
      # ),
      # selectizeInput(
      #   'date', ('Selecione a data'), choices = character(0),
      #   multiple = TRUE, options = list(maxItems = 5)
      # ),
      # selectizeInput(
      #   'countries', ('Selecione os países'), choices =  character(0),
      #   multiple = TRUE
      # ),
      actionButton(inputId = "act_btn_filtros_perg", label = (("Atualizar gráficos"))),
      # selectizeInput('selected_language',
      #                # ("Change language"),
      #                ("Mude o idioma"),
      #                choices = i18n$get_languages(),
      #                selected = "pt-br"),
      
      pickerInput("selected_language", ("Selecione o idioma"),
                  multiple = F,
                  choices = c("en-us", "pt-br"),
                  selected = "pt-br"
      )
    )
    
  })
}

shinyApp(ui = ui, server = server)