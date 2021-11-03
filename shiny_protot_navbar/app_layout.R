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
library(ggplot2)
source("mod_arvore_busca.R")
source("mod_evol_pub_tipo.R")
source("mod_paises_pub_2.R")
source("mod_tabela_artg.R")
source("fct_no_sel.R")
source("fct_filtrar_dim.R")
source("fct_tratar_ano_sel.R")
source("fct_tratar_pais_sel.R")
source("fct_tratar_tipo_pub_sel.R")


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
        tags$script("
          Shiny.addCustomMessageHandler('background-color', function(color) {
            document.body.style.backgroundColor = color;
          });
          $(document).ready(function() {
            console.log('pronto');
            $('#tabs_dash li a').on('click', function() {
                aba_anterior = $(this).text()
                console.log('Clickando Aba (anterior) 1', aba_anterior);
                console.log('Clickando  6', $(this).text());
                //if(aba_anterior == aba_aux){ //Nesse caso, o usuário apenas clicou na mesma aba já selecionada
                //}else{
                    if(aba_anterior == 'Table'){ // Tabela
                        // Aba de navegação topo
                        $('.navbar-default .navbar-nav li a').css({'background-color': '#EADDDD'})
                        $('.navbar-default .navbar-nav .active a').css({'background-color': '#660909'})
                        
                        // Aba de navegação (Gráfico/Tabela)
                        //$('').css({'background-color': '#660909'})
          
                        // Footer
                        $('.footer').css({'background-color': '#660909'})
                        
                        //$('.navbar-default .navbar-nav .active a').css({'background-color': '#660909'})
                        
                        //$('.navbar-default .navbar-nav .active a').css({'background-color': '#660909'})
                        
                    }else{ // Plots
                        // Aba de navegação topo
                        $('.navbar-default .navbar-nav a').css({'background-color': '#DEEBF0'})
                        $('.navbar-default .navbar-nav .active a').css({'background-color': '#64C1C7'})
                        
                        // Aba de navegação (Gráfico/Tabela)
                        //$('.tabbable ul li a').css({'background-color': '#64C1C7'})
          
                        // Footer
                        $('.footer').css({'background-color': '#64C1C7'})
                        
                    }
                //}
                
            })
            
          });
        "),
        tags$head(
            tags$style(
                HTML("
          .datatables {
              font-size: 1.5vw;
          }

          @media screen and (min-width: 1024px) {
              .datatables {
                  font-size: 12px;
              }
          }
        ")
            )
        )
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
        #selected = "dashboard",
        # theme = "navbar.css",
        #fluid = T,
        tabPanel("Dashboard",
                 id = "dashboard",
                 fluidRow(
                     tabsetPanel(type = "tabs",
                                 id = "tabs_dash",
                                 tabPanel("Plots",
                                          id="plots",
                                          sidebarLayout(
                                              sidebarPanel(
                                                  id = "sidebar_g",
                                                  div(id="filtro_grap",
                                                      tags$h4("FILTROS")
                                                  ),
                                                  width = 2,
                                                  selectInput("date", "Selecione o ano:",
                                                              choices = c("TODOS", 2020, 2021),
                                                              selectize = T),
                                                  selectInput("countries", "Selecione o país:", choices = c("TODOS", "Brazil", "Argentina", "Chile", "United States", "Uruguai")),
                                                  selectInput("article_type", "Selecione o tipo de publicação:", choices = c("TODOS", "article", "book", "chapter", "monography", "preprint")),
                                                  actionButton("ref_perg", label = "Refinar pesquisa",
                                                                           icon = icon("search-plus"))
                                                  # shinyWidgets::actionBttn("ref_perg", label = "Refinar pesquisa",
                                                  #                          icon = icon("search-plus"), style = "material-flat")
                                              ),
                                              mainPanel(
                                                  width = 10,
                                                  #### 1.1.2.1 Linha de perguntas ----
                                                  # fluidRow(
                                                  #     div(id="perguntas",
                                                  #         # actionButton("toggleSidebar", "Toggle sidebar"),
                                                  #         tags$h4("PERGUNTAS"),
                                                  #         column(3,
                                                  #                actionButton(
                                                  #                    inputId = "sel_perg",
                                                  #                    label = "SELECIONE O ASSUNTO OU PERGUNTA",
                                                  #                    # inputId = ns("dropdown"),
                                                  #                    icon = icon("question"),
                                                  #                    circle = FALSE
                                                  #                )
                                                  #                # selectInput("idPerg",
                                                  #                #             "SELECIONE O ASSUNTO OU PERGUNTA",
                                                  #                #             )
                                                  #                #  dropdownButton(
                                                  #                #     label = "Selecione o assunto ou pergunta",
                                                  #                #     width = "100%",
                                                  #                #     # inputId = ns("dropdown"),
                                                  #                #     inputId = "dropdown",
                                                  #                #     icon = icon("question"),
                                                  #                #     circle = FALSE,
                                                  #                #     tags$div(
                                                  #                #         # actionButton(inputId = ns("toggle2"),
                                                  #                #         actionButton(inputId = "toggle2",
                                                  #                #                      label = "Selecionar")
                                                  #                #     ),
                                                  #                #     tags$div(style = "margin:10px",
                                                  #                #              column(2, align="center",
                                                  #                #                     #mod_arvore_busca_nosel_ui("arvore_busca_nosel_1"),
                                                  #                #                     # mod_arvore_busca_ui(ns("arvore_busca_1")),
                                                  #                #                     shinyTree("tree",
                                                  #                #                               search=TRUE, searchtime = 1000,
                                                  #                #                               theme="proton", themeIcons = FALSE, themeDots = T)
                                                  #                #              )
                                                  #                #     )
                                                  #                # )
                                                  #         ),
                                                  #         column(7,
                                                  #                # mod_arvore_busca_nosel_ui(ns("arvore_busca_nosel_1")
                                                  #                htmlOutput("Pergunta/tema selecionado X"),
                                                  #         ),
                                                  #     )
                                                  # ),
                                                  div(class="graphs-box",
                                                    fluidRow(
                                                      br(),
                                                      fluidRow(
                                                          column(
                                                              width = 6,
                                                              plotOutput("distPlot1")
                                                          ),
                                                          column(
                                                              width = 6,
                                                              plotOutput("distPlot2")
                                                          )
                                                      ),
                                                      br(),
                                                      fluidRow(
                                                          column(
                                                              width = 12,
                                                              plotOutput("distPlot3")
                                                          )
                                                      ),
                                                      br(),
                                                      fluidRow(
                                                          column(
                                                              width = 6,
                                                              plotOutput("distPlot4")
                                                          ),
                                                          column(
                                                              width = 6,
                                                              plotOutput("distPlot5")
                                                          )
                                                      )
                                                      )
                                                    )
                                                      
                                                  )
                                              ),
                                          actionButton(inputId = "att_grap_filtros_perg",
                                                       label = "Atualizar",
                                                       icon = icon("sync"))
                                          ),
                                 tabPanel("Table",
                                          id = "table",
                                          sidebarLayout(
                                              sidebarPanel(
                                                  id = "sidebar_t",
                                                  # div(class="sidebar",
                                                      div(id="filtro_tab",
                                                          tags$h4("FILTROS")
                                                      ),
                                                      width = 2,
                                                      selectInput("date_tab", "Selecione o ano:",
                                                                  choices = c("TODOS", 2020, 2021),
                                                                  selectize = T),
                                                      selectInput("countries_tab", "Selecione o país:", choices = c("TODOS", "Brazil", "Argentina", "Chile", "United States", "Uruguai")),
                                                      selectInput("article_type_tab", "Selecione o tipo de publicação:", choices = c("TODOS", "article", "book", "chapter", "monography", "preprint"))
                                                  # )
                                              ),
                                              mainPanel(
                                                  width = 10,
                                                  div(class="dt-responsive",
                                                      DT::DTOutput("tabela_perg")
                                                  )
                                                  
                                                  )
                                          ),
                                          actionButton(inputId = "att_tab_filtros_perg",
                                                       label = "Atualizar",
                                                       icon = icon("sync"))
                                 )
                                 
                     )
                 )
                     
                ),
        tabPanel("Sobre", id = "sobre")
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
server <- function(input, output, session) {
    debug = T
    # df_dim_au_co_jo <- fst::read_fst("../dados/app/df_dimensions_tabelas_clean.fst") |> 
    #     dplyr::rename(date = date_normal)
    # df_tabela_base_plus_categ <- fst::read_fst("../dados/app/df_tabela_base_plus_categ.fst")
    # df_perguntas <- data.table::fread("../dados/perguntas_full_clean.csv")
    # # dplyr::glimpse(df_tabela_base_plus_categ)
    # select_test <- F
    # first_plot <- reactiveVal(value = T)
    # list_parameters <- eventReactive(input$att_tab_filtros_perg | input$sel_perg_teste, {
    #     print("Começando lista de parâmetros")
    #     # toggleDropdownButton(inputId = "dropdown")
    #     ## Artigos resposta
    #     # no_sel <- func_ret_no_sel(input$`arvore_busca_1-tree`, debug)
    #     # mod_tabela_artg_server("tabela_artg_1", no_sel)
    #     if(select_test){
    #         input <- NULL
    #         input$article_type <- "article"
    #         input$date <- "2020"
    #         input$countries <- "Brazil"
    #     }
    #     
    #     # first_plot$value <- FALSE
    #     ## tipo de publicação selecionado
    #     tipo_pub_sel <- func_trat_tipo_pub(input$article_type)
    #     ## Ano selecionado
    #     ano_sel <- func_trat_ano(input$date)
    #     ## país selecionado
    #     pais_sel <- func_trat_pais(input$countries)
    #     ## lista com parâmetros
    #     list_parameters <- list(tipo_pub_sel, ano_sel, pais_sel)
    #     print(list_parameters)
    #     if(select_test){
    #         list_parameters <- as.list("article", "2020", "Brazil")
    #     }
    #     # browser()
    #     list_parameters
    # }, ignoreInit = FALSE)
    # 
    # 
    # #### 1.0.4.2 Base de artigos resultante -----
    # ## Base pós seleção de filtros
    # df_tabela_base_plus_categ_filtered <- eventReactive(input$att_tab_filtros_perg | input$sel_perg_teste, {
    #     print("Começando filtro")
    #     ## Tabela base (id) filtrada
    #     # browser()
    #     func_filtrar_dim_tabela(df_tabela_base_plus_categ, list_parameters(), debug)
    #     #teste
    #     # df_tabela_base_plus_categ_filtered <- func_filtrar_dim_tabela(df_tabela_base_plus_categ, list_parameters, debug)
    # })
    # df_dim_au_co_jo_filtered_tab <- reactive({
    #     print("Começando df_dim graph")
    #     # browser()
    #     ids <- df_tabela_base_plus_categ_filtered() |> 
    #         dplyr::select(id) |> 
    #         dplyr::pull()
    #     df_dim_au_co_jo |> 
    #         dplyr::filter(id %in% ids)
    # })
    # no_sel <- reactiveVal("Does pregnancy increase the risk for severe COVID-19?" )
    # observeEvent(input$sel_perg_teste, {
    #     new_value <- "Does pregnancy increase the risk for severe COVID-19?" 
    #     no_sel(new_value)
    # })
    # 
    # observeEvent(input$att_tab_filtros_perg | input$sel_perg_teste, {
    #     # browser()
    #     print("observe button, create graph")
    #     mod_evol_pub_tipo_server("evol_pub_tipo_1",
    #                              df_dimensions_type_date_country = df_tabela_base_plus_categ_filtered(),
    #                              plotly = F, teste = F, debug = T)
    #     mod_paises_pub_2_server("paises_pub_2_1", df_filtros = df_tabela_base_plus_categ_filtered(),
    #                             plotly = F, teste = F, debug = F)
    #     print("Valor selecionado pergunta")
    #     print(no_sel())
    #     node_tier <- "3"
    #     df_buscas <- arrow::read_parquet("../dados/app/Banco1909PerguntasAmostra.parquet")
    #     df_buscas_relacao <- data.table::fread("../dados/relacaoColunaPergunta.csv") |>
    #         dplyr::select(col_name = `Nome da Coluna`, col_name_plus_abs = `Nome da Coluna Abs`, perg = Pergunta)
    #     
    #     # browser()
    #     mod_tabela_artg_server("tabela_artg_1", no_sel(), node_tier, df_buscas,
    #                            df_buscas_relacao, df_dim_au_co_jo_filtered_tab(),
    #                            teste = F, debug = T)
    #     
    #     
    # })
    
    
    output$distPlot1 <- renderPlot({
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
    # tree <- dfToTree(df, c("EIXO", "TOPICS", "QUERIES", "QUESTIONS"))
    # output$tree <- renderTree({tree})
    #### 1.1.3.1 Módulo de perguntas ----
    # mod_arvore_busca_server("arvore_busca_1", df_perguntas, debug)
    # #### 1.1.3.2  Botão (abrir modal) de selecionar perguntas ----
    # observeEvent(input$sel_perg, {
    #     mod_arvore_busca_server("arvore_busca_1", df_perguntas, debug)
    #     showModal(modalDialog(
    #         mod_arvore_busca_ui("arvore_busca_1"),
    #         ## árvore (colapsible tree) em nós
    #         # mod_arvore_ui("arvore_1"),
    #         footer = tagList(actionButton("select_node", "Selecionar")),
    #         easyClose = TRUE
    #     ))
    # })
    
    df_tabela_perg_filt <- fst::read_fst("../dados/df_tabela_perg_filt.fst")
    
    df_tabela_perg_filt <- df_tabela_perg_filt |>
        # dplyr::select(authors_ln, tittle_50char, journals, countries, type)
        dplyr::select(authors_last_name, title_n_char, abstract_50char, journals, countries,
                      type, doi, citations, altmetrics, authors_ln, title, abstract,
                      journal_lists, research_org_country_names) |> 
        dplyr::rename(`Autor(es)` = authors_last_name, `Título` = title_n_char,
                      `Resumo` = abstract_50char, `Revista` = journals,
                      `País` = countries, `Tipo` = type, doi = doi,
                      `Citações` = citations, `Altmetria` = altmetrics,
                      `Autor(es) nome completo` = authors_ln,
                      `Título completo` =  title,`Resumo completo` = abstract,
                      `Revistas completo` = journal_lists,
                      `Países completo` = research_org_country_names)
    
    output$tabela_perg <- DT::renderDataTable({
        DT::datatable(df_tabela_perg_filt[,1:14],
                      options = list(autoWidth = T,
                                     extensions="Responsive",
                                     columnDefs = list(list(visible=FALSE, targets=c(10, 11, 12, 13, 14)),
                                                       list(width = '200px', targets = c(1, 3))),
                                     rowCallback = DT::JS(
                                         "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                                         "var full_text_author = aData[10]",
                                         "var full_text_title = aData[11]",
                                         "var full_text_abs = aData[12]",
                                         "var full_text_journals = aData[13]",
                                         "var full_text_countries = aData[14]",
                                         "$('td:eq(1)', nRow).attr('title', full_text_author);",
                                         "$('td:eq(2)', nRow).attr('title', full_text_title);",
                                         "$('td:eq(3)', nRow).attr('title', full_text_abs);",
                                         "$('td:eq(4)', nRow).attr('title', full_text_journals);",
                                         "$('td:eq(5)', nRow).attr('title', full_text_countries);",
                                         "}"
                                         # "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                                         # "var full_text_title = aData[9]",
                                         # "$('td:eq(7)', nRow).attr('title', full_text_title);",
                                         # "}"
                                     ),
                                     dom = 'Bfrtip',
                                     responsive = TRUE,
                                     buttons =
                                         list(list(
                                             extend = 'collection',
                                             buttons = c('csv', 'excel', 'pdf'),
                                             text = 'Baixar tabela'
                                         ))
                      ),
        )
        })
        # proxy = dataTableProxy('tabela_perg') %>%
        #     hideCols(hide = 3)
        # DT::datatable(df_tabela_perg_filt[,1:14],
        #               extensions="Responsive",
        #               options = list(autoWidth = T,
        #                              columnDefs = list(list(visible=FALSE, targets=c(10, 11, 12, 13, 14))),
        #                              rowCallback = DT::JS(
        #                                  "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
        #                                  "var full_text_author = aData[10]",
        #                                  "var full_text_title = aData[11]",
        #                                  "var full_text_abs = aData[12]",
        #                                  "var full_text_journals = aData[13]",
        #                                  "var full_text_countries = aData[14]",
        #                                  "$('td:eq(1)', nRow).attr('title', full_text_author);",
        #                                  "$('td:eq(2)', nRow).attr('title', full_text_title);",
        #                                  "$('td:eq(3)', nRow).attr('title', full_text_abs);",
        #                                  "$('td:eq(4)', nRow).attr('title', full_text_journals);",
        #                                  "$('td:eq(5)', nRow).attr('title', full_text_countries);",
        #                                  "}"
        #                                  # "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
        #                                  # "var full_text_title = aData[9]",
        #                                  # "$('td:eq(7)', nRow).attr('title', full_text_title);",
        #                                  # "}"
        #                              ),
        #                              dom = 'Bfrtip',
        #                              responsive = TRUE,
        #                              buttons =
        #                                  list(list(
        #                                      extend = 'collection',
        #                                      buttons = c('csv', 'excel', 'pdf'),
        #                                      text = 'Baixar tabela'
        #                                  ))
        #               )
        # )
    # })
    ## Mudar a cor de background
    observeEvent(input$tabs_dash, {
        # browser()
        if(input$tabs_dash == "Plots"){
            session$sendCustomMessage("background-color", "#ecf0f5")
        } else {
            session$sendCustomMessage("background-color", "#faf6f6")
            # session$sendCustomMessage("change-colors")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
