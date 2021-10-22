#' arvore_busca UI Function
#'
#' @description Módulo que irá construir o campo de busca em formato hierárquico (árvore)
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shinyTree
#' @import stringr
#' @import shinyjs
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

mod_arvore_busca_ui <- function(id){
  ns <- NS(id)
  tagList(
    # verbatimTextOutput(ns("sel_text")),
    # "Nó selecionado:",
    # textOutput(ns("sel_text")),
    shinyTree(ns("tree"),
              search=TRUE, searchtime = 1000,
              theme="proton", themeIcons = FALSE, themeDots = T),
  )
}
    #arvore_busca_1-tree-search-input
#' arvore_busca Server Functions
#'
#' @noRd 
mod_arvore_busca_server <- function(id, df_perguntas, debug){
  moduleServer( id, function(input, output, session){
    # print("Começando a arvore busca")
    ns <- session$ns
    # df_ordered <- df |>
    #   dplyr::arrange(QUESTIONS)
    tree <- dfToTree(df_perguntas, c("EIXO", "TOPICS", "QUERIES", "QUESTIONS"))
    output$tree <- renderTree({tree})
    # print(paste0("Estamos na busca, já fez a árvore indo pra UI"))
    # output selected node
    # browser()
    # output$sel_text <- renderText({
    #   tree <- input$tree
    #   browser()
    #   req(tree)
    #   selected <- shinyTree::get_selected(tree, format = "classid")
    #   selected[[1]][1]
    # })
  })
}
# shinyApp(mod_arvore_busca_ui, mod_arvore_busca_server)
## To be copied in the UI
# mod_arvore_busca_ui("arvore_busca_1")
    
## To be copied in the server
# mod_arvore_busca_server("arvore_busca_1")
