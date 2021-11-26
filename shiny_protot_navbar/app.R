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
source("mod_map_pub.R")
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
    tags$link(rel = "stylesheet", type = "text/css", href = "sobre.css"),
    # includeCSS("www/geral.css"),
    shinyjs::useShinyjs(),
    tags$script(src = "mudar_aba.js"),
    tags$head(
        tags$head(
            tags$style(
                HTML("
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
        
        div(img(src='marca_ric_covid.svg',style="margin-top: 0px; padding-right:5px;padding-bottom:5px", width = 380),
            ## div para logo beta
            div(img(src='beta.png', width = 30)) |>
                tagAppendAttributes(class = 'logo_beta')) |>
            tagAppendAttributes(class = 'logo'),
        #theme = "journal",
        windowTitle = "Repositório de informações sobre COVID-19",
        id = "navbar",
        #selected = "dashboard",
        # theme = "navbar.css",
        #fluid = T,
        selected = "Dashboard",
        tabPanel(
            "Dashboard",
            id = "dashboard",
            fluidRow(
                tabsetPanel(
                    type = "tabs",
                    id = "tabs_dash",
                    selected="plots",
                    tabPanel(
                        "Gráficos",
                        id="plots",
                        value = "plots",
                        fluidRow(
                            div(
                                class="plots_first_row",
                                div(
                                    class = "background-top-perguntas",
                                    fluidRow(
                                      br(),
                                      column(
                                          width = 2
                                      ),
                                      column(
                                          width = 1,
                                          # actionBttn(inputId = "refine_pesquisa",
                                          #            icon = icon("search"),
                                          #            label = "Refine sua pesquisa",
                                          #            style='simple'),
                                          column(
                                              width=4,
                                              div(class='refine-icon-plots',
                                                  icon('search-plus')),
                                          ),
                                          column(
                                              width=8,
                                              div(class='refine-text-plots',
                                                  span("REFINE SUA PESQUISA"))
                                          )
                                      ),
                                      column(
                                          width = 4,
                                          actionButton(inputId =  "sel_perg_plots",
                                                       label = "Selecione: EIXO / TÓPICO / CONSULTA / PERGUNTA")
                                      ),
                                      column(
                                          width = 1
                                      ),
                                      column(
                                          width = 3,
                                          # radioButtons(inputId = "sel_estrategia",
                                          #              label = "Escolha a estratégia de busca",
                                          #              choices = c("Título", "Título e Abstract"))
                                      ),
                                      column(
                                          width = 1
                                      )
                                    ),
                                    fluidRow(
                                        class="sec_row",
                                        column(
                                            width = 2
                                        ),
                                        column(
                                            width = 10,
                                            div(
                                                class = "selecionados",
                                                textOutput("eixo_sel_plots"),
                                                textOutput("topico_sel_plots"),
                                                textOutput("consulta_sel_plots"),
                                                br(),
                                                br()
                                            )
                                        )
                                    )
                                )
                            )
                        ),
                        sidebarLayout(
                            sidebarPanel(
                                id = "sidebar_g",
                                div(
                                    id="filtro_grap",
                                    tags$h4("FILTROS")
                                ),
                                width = 2,
                                ## ****************** ##
                                ## Teste              ##
                                ## ****************** ##
                                selectInput("date", "Selecione o ano:",
                                            choices = c("TODOS", 2020, 2021),
                                            selectize = T),
                                selectInput("countries", "Selecione o país:", choices = c("TODOS", "Brazil", "Argentina", "Chile", "United States", "Uruguai")),
                                selectInput("article_type", "Selecione o tipo de publicação:", choices = c("TODOS", "article", "book", "chapter", "monography", "preprint")),
                                ## ****************** ##
                                
                                # ## ****************** ##
                                # ## Produção           ##
                                # ## ****************** ##
                                # selectizeInput(
                                #     'article_type_grap', ('Selecione o(s) tipo(s) de publicação(ões):'), choices = filtro_types,
                                #     multiple = TRUE, options = list(maxItems = 5)
                                # ),
                                # selectizeInput(
                                #     'year_grap', ('Selecione o(s) ano(s):'), choices = filtro_years,
                                #     multiple = TRUE, options = list(maxItems = 1)
                                # ),
                                # tags$div(
                                #     class = "country_filter",
                                #     tags$p("Selecione o(s) país(es)")),
                                # dropdown(
                                #     label = ("TODOS"),
                                #     # inputId = ns("dropdown"),
                                #     inputId = "dropdown_graph",
                                #     circle = FALSE,
                                #     pickerInput(
                                #         'continents_grap', 'Selecione o(s) continente(s)', choices =  selection_continents,
                                #         selected = "TODOS",
                                #         multiple = TRUE
                                #     ),
                                #     selectizeInput(
                                #         'countries_grap', 'Selecione o(s) país(es)', choices =  filtro_countries,
                                #         multiple = TRUE
                                #     )
                                # ),
                                # shinyWidgets::actionBttn(
                                #     inputId = "att_grap_filtros_perg",
                                #     label = "Atualizar",
                                #     icon = icon("sync"),
                                #     style = "fill"
                                # )
                                # ## ****************** ##
                            ),
                            mainPanel(
                                width = 10,
                                div(
                                    class="graphs-box",
                                    ## ****************** ##
                                    ## Teste              ##
                                    ## ****************** ##
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
                                                # plotOutput("distPlot3")
                                                mod_map_pub_ui("map_pub_1")
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
                                    ## ****************** ##
                                    
                                    # ## ****************** ##
                                    # ## Produção           ##
                                    # ## ****************** ##
                                    # fluidRow(
                                    #     br(),
                                    #     fluidRow(
                                    #         column(
                                    #             width = 6,
                                    #             shinycssloaders::withSpinner(mod_evol_pub_tipo_ui("evol_pub_tipo_1"))
                                    #         ),
                                    #         column(
                                    #             width = 6,
                                    #             shinycssloaders::withSpinner(mod_paises_pub_2_ui("paises_pub_2_1"))
                                    #         )
                                    #     ),
                                    #     br(),
                                    #     fluidRow(
                                    #         column(
                                    #             width = 6,
                                    #             # shinycssloaders::withSpinner(mod_map_pub_ui("map_pub_1"))
                                    #             shinycssloaders::withSpinner(plotOutput("distPlot1"))
                                    #         ),
                                    #         column(
                                    #             width = 6,
                                    #             # shinycssloaders::withSpinner(mod_categ_pub_ui("categ_pub_1"))
                                    #             shinycssloaders::withSpinner(plotOutput("distPlot3"))
                                    #         )
                                    #         # column(
                                    #         #   width = 12,
                                    #         #   shinycssloaders::withSpinner(mod_map_pub_ui("map_pub_1"))
                                    #         # )
                                    #     ),
                                    #     br(),
                                    #     fluidRow(
                                    #         column(
                                    #             width = 6,
                                    #             plotOutput("distPlot2")
                                    #         ),
                                    #         column(
                                    #             width = 6,
                                    #             # shinycssloaders::withSpinner(mod_artigos_autores_ui(("artigos_autores_1")))
                                    #             shinycssloaders::withSpinner(plotOutput("distPlot4"))
                                    #         )
                                    #     )
                                    # )
                                    # ## ****************** ##
                                )
                                  
                            )
                        ),
                    ),
                    tabPanel("Tabela",
                            id = "table",
                            value = "table",
                            fluidRow(
                                div(
                                    class="table_first_row",
                                    div(
                                        class = "background-top-perguntas",
                                        fluidRow(
                                            br(),
                                            column(
                                                width = 2
                                            ),
                                            column(
                                                width = 1,
                                                # actionBttn(inputId = "refine_pesquisa",
                                                #            icon = icon("search"),
                                                #            label = "Refine sua pesquisa",
                                                #            style='simple'),
                                                column(
                                                    width=4,
                                                    div(class='refine-icon-table',
                                                        icon('search-plus')),
                                                ),
                                                column(
                                                    width=8,
                                                    div(
                                                        class='refine-text-table',
                                                        span("REFINE SUA PESQUISA"))
                                                )
                                            ),
                                            column(
                                                width = 4,
                                                actionButton(
                                                    inputId =  "sel_perg_table",
                                                    label = "Selecione: EIXO / TÓPICO / CONSULTA / PERGUNTA")
                                            ),
                                            column(
                                                width = 1
                                            ),
                                            column(
                                                width = 3,
                                                # radioButtons(inputId = "sel_estrategia",
                                                #              label = "Escolha a estratégia de busca",
                                                #              choices = c("Título", "Título e Abstract"))
                                                awesomeRadio(
                                                    inputId = "sel_estrategia",
                                                    label = "Escolha a estratégia de busca", 
                                                    choices = c("Título", "Título e Abstract"),
                                                    selected = "Título",
                                                    inline = TRUE, 
                                                    status = "success"
                                                )
                                            ),
                                            column(
                                                width = 1
                                            )
                                        ),
                                        fluidRow(
                                            class="sec_row",
                                            column(
                                                width = 2
                                            ),
                                            column(
                                                width = 10,
                                                div(
                                                    class = "selecionados",
                                                    textOutput("eixo_sel_tab"),
                                                    textOutput("topico_sel_tab"),
                                                    textOutput("consulta_sel_tab"),
                                                    textOutput("pergunta_sel_tab"),
                                                    br()
                                                )
                                            )
                                        )
                                    )
                                )
                            ),
                            fluidRow(
                                sidebarLayout(
                                    sidebarPanel(
                                        id = "sidebar_t",
                                        # div(class="sidebar",
                                        div(
                                            id="filtro_tab",
                                            tags$h4("FILTROS")
                                        ),
                                        width = 2,
                                        
                                        ## ****************** ##
                                        ## Teste              ##
                                        ## ****************** ##
                                        selectInput(
                                            "date_tab", "Selecione o ano:",
                                            choices = c("TODOS", 2020, 2021),
                                            selectize = T),
                                        selectInput("countries_tab", "Selecione o país:", choices = c("TODOS", "Brazil", "Argentina", "Chile", "United States", "Uruguai")),
                                        selectInput("article_type_tab", "Selecione o tipo de publicação:", choices = c("TODOS", "article", "book", "chapter", "monography", "preprint")),
                                        ## ****************** ##
                                        
                                        # ## ****************** ##
                                        # ## Produção           ##
                                        # ## ****************** ##
                                        # selectizeInput(
                                        #     'article_type_tab', ('Selecione o(s) tipo(s) de publicação(ões):'), choices = filtro_types,
                                        #     multiple = TRUE, options = list(maxItems = 5)
                                        # ),
                                        # selectizeInput(
                                        #     'year_tab', ('Selecione o(s) ano(s):'), choices = filtro_years,
                                        #     multiple = TRUE, options = list(maxItems = 1)
                                        # ),
                                        # tags$div(
                                        #     class = "country_filter",
                                        #     tags$p("Selecione o(s) país(es)")
                                        # ),
                                        # dropdown(
                                        #     label = ("TODOS"),
                                        #     inputId = "dropdown_tab",
                                        #     circle = FALSE,
                                        #     pickerInput(
                                        #         'continents_tab', 'Selecione o(s) continente(s)', choices =  selection_continents,
                                        #         selected = "TODOS",
                                        #         multiple = TRUE
                                        #     ),
                                        #     selectizeInput(
                                        #         'countries_tab', 'Selecione o(s) país(es)', choices =  filtro_countries,
                                        #         multiple = TRUE
                                        #     )
                                        # ),
                                        # ## ****************** ##
                                        shinyWidgets::actionBttn(
                                            inputId = "att_tab_filtros_perg",
                                            label = "Atualizar",
                                            icon = icon("sync"),
                                            style = "fill"
                                        )
                                    ),
                                    mainPanel(
                                        width = 10,
                                        div(
                                            class="dt-responsive background-table",
                                            ## ****************** ##
                                            ## Teste              ##
                                            ## ****************** ##
                                            DT::DTOutput("tabela_perg")
                                            ## ****************** ##
                                            
                                            # ## ****************** ##
                                            # ## Produção           ##
                                            # ## ****************** ##
                                            # mod_tabela_artg_ui("tabela_artg_1")
                                            # ## ****************** ##
                                        )
                                    )
                                )
                            )
                    ),
                    div(
                        class = "sel_aba",
                        prettySwitch(
                            inputId = "sel_aba",
                            label = "", 
                            fill = TRUE,
                            bigger = T,
                            width = NULL
                        )
                    ),
                    div(class = "abas_divisor",
                        span("/"))
                                 
                )
            )
                 
        ),
        tabPanel(
            "Sobre",
            id = "sobre",
            type="tabs",
            fluidRow(
                tabsetPanel(
                    id = "sobre_painel",
                    selected = "Equipe",
                    type="tabs",
                    tabPanel(
                        "O projeto",
                        id = "projeto",
                        div(
                            class="sobre_projeto",
                            fluidRow(
                                column(
                                    width = 12,
                                    div(
                                        class = "sobre_proj_title",
                                        "O PROJETO"
                                    ),
                                    # box(textOutput("title")),
                                    br(),
                                    box(textOutput("text1")),
                                    box(plotOutput("distPlot7"))
                                    ),
                                    column(
                                        width = 12,
                                        box(plotOutput("distPlot8")),
                                        box(plotOutput("distPlot9"))
                                    )
                                )
                        )
                                     
                    ),
                    tabPanel(
                        "Metodologia",
                        id = "metodologia"
                    ),
                    tabPanel(
                        "Equipe",
                        id = "equipe",
                        div(
                            class="sobre_equipe",
                            fluidRow(
                                div(class = "sobre_equipe_title",
                                    "Equipe"
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 4,
                                    column(
                                        width = 6,
                                        div(
                                            class="foto_curr",
                                            img(class = "foto", src="eu.jpg", align = "left"),
                                            shinyWidgets::actionBttn(
                                               inputId="mikael",
                                               label = "Curriculo Lattes",
                                               style = "material-flat"
                                            )
                                        )
                                    ),
                                        column(
                                            width = 6,
                                               div(
                                                   class = "equipe_nome_title",
                                                   span("Mikael")
                                               ),
                                               div(
                                                   class="equipe_texto_apres",
                                                   span("Formado em Engenharia de Computação (UFSM) e graduando de Estatística. \n\n
                                                        Bolsista do projeto Google IA, atuando na área de análise de dados e desenvolvimento de painéis."))
                                               ),
                                ),
                                column(
                                    width = 4,
                                    column(
                                        width = 6,
                                           div(
                                               class="foto_curr",
                                               img(class = "foto", src="eu.jpg", align = "left"),
                                               shinyWidgets::actionBttn(
                                                   inputId="mikael",
                                                   label = "Curriculo Lattes",
                                                   style = "material-flat"
                                               )
                                            )
                                    ),
                                    column(
                                        width = 6,
                                        div(
                                            class = "equipe_nome_title",
                                            span("Mikael")
                                        ),
                                        div(
                                            class="equipe_texto_apres",
                                            span("Formado em Engenharia de Computação (UFSM) e graduando de Estatística. \n\n
                                                    Bolsista do projeto Google IA, atuando na área de análise de dados e desenvolvimento de painéis.")
                                        )
                                    )
                                ),
                                column(
                                    width = 4,
                                    column(
                                        width = 6,
                                        div(
                                            class="foto_curr",
                                            img(class = "foto", src="eu.jpg", align = "left"),
                                            shinyWidgets::actionBttn(
                                                inputId="mikael",
                                                label = "Curriculo Lattes",
                                                style = "material-flat"
                                            )
                                        )
                                    ),
                                    column(
                                        width = 6,
                                        div(
                                            class = "equipe_nome_title",
                                            span("Mikael")
                                           ),
                                           div(
                                               class="equipe_texto_apres",
                                               span("Formado em Engenharia de Computação (UFSM) e graduando de Estatística. \n\n
                                                    Bolsista do projeto Google IA, atuando na área de análise de dados e desenvolvimento de painéis.")
                                            )
                                    )
                                )
                            )
                        )
                    ),
                )
            )
        )
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
    df_perguntas <- data.table::fread("../dados/arvore_perguntas_16_11.csv")
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
    r_aux <- "COVID19"
    # df_tabela_base_filtros <- fst::read_fst("data-raw/app/df_tabela_base_filtros.fst")
    # mod_map_pub_server("map_pub_1", r_aux, df_tabela_base_filtros, teste = F, debug = T)
    
    
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
    output$title <- renderText({
        "O PROJETO"
    })
    output$text1 <- renderText({ "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        Sed purus enim, dictum ut orci non, porta dapibus lorem.
        Nunc at luctus ipsum, vitae dictum eros.
        Nulla laoreet massa ac dolor suscipit aliquet.
        Cras posuere felis quis tellus convallis, sed ultricies arcu hendrerit.
        Vestibulum efficitur turpis ultrices, tincidunt ante vel, imperdiet libero.
        Sed nunc nulla, blandit eget lectus ac, pellentesque varius enim.
        Nunc tortor lectus, tempus vitae ex at, pellentesque blandit ante.
        Integer odio diam, pulvinar sed laoreet eget, sagittis at elit.
        Maecenas tincidunt lacus urna, in pellentesque massa dapibus viverra.
        Praesent efficitur odio eget arcu eleifend tempus. 
        Donec rhoncus dolor eget urna porta, id consequat ante elementum.
        Sed pretium, mi venenatis tincidunt cursus, mauris tellus consectetur nunc, vitae consectetur erat dolor ac felis.
        Pellentesque suscipit ornare odio, at consectetur velit volutpat a.
        Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.
        Phasellus mattis quam orci, nec aliquam elit pulvinar ut.
        Aliquam ac erat vehicula, suscipit eros ut, auctor quam.
        In arcu ante, porta nec egestas vitae, dignissim sed leo.
        Vestibulum magna velit, iaculis suscipit facilisis viverra, luctus semper mi.
        Curabitur tincidunt ante mi, et posuere enim volutpat eget. Aenean mollis ante quam.
        Nam augue orci, pretium in metus eget, bibendum posuere turpis. 
        Nullam sed posuere nisi, a feugiat magna.
        Curabitur iaculis urna ac magna iaculis sodales. 
        Sed eu facilisis diam, quis elementum tellus. 
        Ut at lacus et velit rhoncus feugiat. 
        Etiam in metus malesuada, feugiat nisl id, lacinia neque. 
        Vestibulum varius lorem est, nec auctor tortor finibus in. 
        Cras a pellentesque libero, sed semper lectus.
        Mauris mauris erat, volutpat eu lacinia quis, pretium ut felis. 
        Maecenas tristique, urna eu dignissim lobortis, ex massa mattis urna, in fringilla augue ante eleifend leo. 
        Vestibulum eget dignissim magna. Phasellus a mauris eu nisl consequat bibendum." })
    output$eixo_sel_tab <- renderText({
        paste0("EIXO: ")
    })
    output$topico_sel_tab <- renderText({
        paste0("TÓPICO: ")
    })
    output$consulta_sel_tab <- renderText({
        paste0("CONSULTA: ")
    })
    output$pergunta_sel_tab <- renderText({
        paste0("PERGUNTA: ")
    })
    output$eixo_sel_plots <- renderText({
        paste0("EIXO: ")
    })
    output$topico_sel_plots <- renderText({
        paste0("TÓPICO: ")
    })
    output$consulta_sel_plots <- renderText({
        paste0("CONSULTA: ")
    })
    output$pergunta_sel_plots <- renderText({
        paste0("PERGUNTA: ")
    })
    # tree <- dfToTree(df, c("EIXO", "TOPICS", "QUERIES", "QUESTIONS"))
    # output$tree <- renderTree({tree})
    #### 1.1.3.1 Módulo de perguntas ----
    # mod_arvore_busca_server("arvore_busca_1", df_perguntas, debug)
    #### 1.1.3.2  Botão (abrir modal) de selecionar perguntas ----
    observeEvent(input$sel_perg_plots, {
        mod_arvore_busca_server("arvore_busca_1", df_perguntas, debug)
        showModal(modalDialog(
            mod_arvore_busca_ui("arvore_busca_1"),
            ## árvore (colapsible tree) em nós
            # mod_arvore_ui("arvore_1"),
            footer = tagList(actionButton("select_node", "Selecionar")),
            easyClose = TRUE
        ))
    })
    
    observeEvent(input$sel_perg_table, {
        mod_arvore_busca_server("arvore_busca_2", df_perguntas, debug)
        showModal(modalDialog(
            mod_arvore_busca_ui("arvore_busca_2"),
            ## árvore (colapsible tree) em nós
            # mod_arvore_ui("arvore_1"),
            footer = tagList(actionButton("select_node", "Selecionar")),
            easyClose = TRUE
        ))
    })
    
    df_perguntas_g <- data.table::fread("../dados/perguntas_teste.csv")
    # df_perguntas <- data.table::fread("../dados/perguntas_teste.csv")
    observeEvent(input$ref_perg, {
        mod_arvore_busca_server("arvore_busca_2", df_perguntas_g, debug)
        showModal(modalDialog(
            mod_arvore_busca_ui("arvore_busca_2"),
            ## árvore (colapsible tree) em nós
            # mod_arvore_ui("arvore_1"),
            footer = tagList(actionButton("select_node_graph", "Selecionar")),
            easyClose = TRUE
        ))
    })
    observeEvent(input$select_node, {
        ## pega o nó selecionado
        no_sel <- func_ret_no_sel(input$`arvore_busca_1-tree`, debug)
        ## me diz qual nível é o nó selecionado (1 eixo, 2 topic, 3 querie, 4 question)
        df_node_hierarchy <- func_get_node_hierarchy(df_perguntas, no_sel, debug)
        node_tier <- func_get_node_tier(df_perguntas, no_sel, debug)
        # mod_arvore_busca_nosel_server("arvore_busca_nosel_1", no_sel, node_tier, debug)
        # df_dim_au_co_jo <- df_dim_au_co_jo_()
        removeModal()
    })
    #### 1.1.3.2  Botão (fechar modal) de selecionar perguntas ----
    observeEvent(input$select_node, {
        ## pega o nó selecionado
        no_sel <- func_ret_no_sel(input$`arvore_busca_1-tree`, debug)
        ## me diz qual nível é o nó selecionado (1 eixo, 2 topic, 3 querie, 4 question)
        df_node_hierarchy <- func_get_node_hierarchy(df_perguntas, no_sel, debug)
        node_tier <- func_get_node_tier(df_perguntas, no_sel, debug)
        # mod_arvore_busca_nosel_server("arvore_busca_nosel_1", no_sel, node_tier, debug)
        # df_dim_au_co_jo <- df_dim_au_co_jo_()
        removeModal()
        # browser()
        output$eixo_sel_tab <- renderText({
            paste0("EIXO: ", df_node_hierarchy$EIXO)
        })
        output$topico_sel_tab <- renderText({
            if(node_tier > 1){
                paste0("TÓPICO: ", df_node_hierarchy$TOPICS)
            }else{
                paste0("TÓPICO: NÃO SELECIONADo")
            }
        })
        output$consulta_sel_tab <- renderText({
            if(node_tier > 2){
                paste0("CONSULTA: ", df_node_hierarchy$QUERIES)
            }else{
                paste0("CONSULTA: NÃO SELECIONADA")
            }
        })
        output$pergunta_sel_tab <- renderText({
            if(node_tier > 3){
                paste0("PERGUNTA: ", df_node_hierarchy$QUESTIONS)
            }else{
                paste0("PERGUNTA: NÃO SELECIONADA")
            }
        })
    })
    
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
                      extensions=c("Responsive", "Buttons"),
                      options = list(autoWidth = T,
                                     columnDefs = list(list(visible=FALSE, targets=c(10, 11, 12, 13, 14))),
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
        if(input$tabs_dash == "plots"){
            shinyWidgets::updatePrettySwitch(session, "sel_aba",
                                             value = F)
        } else {
            # session$sendCustomMessage("background-color", "#faf6f6")
            shinyWidgets::updatePrettySwitch(session, "sel_aba",
                                             value = T)
        }
    }, ignoreInit = T)
    observeEvent(input$sel_aba, {
        # browser()
        if(input$sel_aba){
            updateTabsetPanel(session, "tabs_dash",
                              selected = "table")
            session$sendCustomMessage("switch_change", T)
        }else{
            updateTabsetPanel(session, "tabs_dash",
                              selected = "plots")
            session$sendCustomMessage("switch_change", F)
        }
    }, ignoreInit = T)
    
    ## Adicionar link currículo lattes
    shinyjs::onclick("mikael", runjs("window.open('http://google.com', '_blank')"))
}

# Run the application 
shinyApp(ui = ui, server = server)
