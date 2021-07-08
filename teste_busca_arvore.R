library(shiny)
library(shinyTree)
library(shinyWidgets)
library(shinyjs)

df <- data.table::fread("dados/perguntas_full.csv")
df_manip <- df[stringr::str_count(df$QUERIES, "") > 1]
data.table::fwrite(df_manip, "dados/perguntas_full_clean.csv")
# df <- data.frame(Titanic)
# tree <- dfToTree(df, c("Sex", "Class", "Survived"))
tree <- dfToTree(df_manip, c("EIXO", "TOPICS", "QUERIES", "QUESTIONS"))

ui <- fluidPage(
  useShinyjs(),
  # dropdownButton(circle = FALSE, label = "Show Tree",
  #                shinyTree("tree", stripes = TRUE, checkbox = TRUE)
  # ),
  # shinyTree("tree", checkbox = TRUE),
  shinyTree("myTree",
            search=TRUE, searchtime = 10,
            theme="proton", themeIcons = FALSE, themeDots = T,
            contextmenu = TRUE),
  actionButton("closeAll_my", "Close My Tree"),
  actionButton("openAll_my", "Open My Tree"),
  hr(),
  "Currently Selected:",
  verbatimTextOutput("sel_names"),
  verbatimTextOutput("sel_slices"),
  verbatimTextOutput("sel_classid"),
  textOutput("sel_text")
  
)

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = "",
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      ),
      root3 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      )
    )
  })
  
  output$myTree <- renderTree({tree})
  
  observeEvent(input$closeAll_my, {
    runjs(HTML('$("#myTree").jstree("close_all");'))
  })
  observeEvent(input$openAll_my, {
    runjs(HTML('$("#myTree").jstree("open_all");'))
  })
  output$sel_names <- renderPrint({
    tree <- input$myTree
    req(tree)
    get_selected(tree)
  })
  output$sel_slices <- renderPrint({
    tree <- input$myTree
    req(tree)
    get_selected(tree, format = "slices")
  })
  output$sel_classid <- renderPrint({
    tree <- input$myTree
    req(tree)
    get_selected(tree, format = "classid")
  })
  observeEvent(input$myTree, {
     output$sel_text <- renderText({
       tree <- input$myTree
       # if(is.null(tree)){
         req(tree)
         selected <- get_selected(tree, format = "classid")
         selected[[1]][1]
       # }else{
       #   paste0("Nada selecionado")
       # }
     })
  })
}

shinyApp(ui, server)