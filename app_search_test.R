library(shiny)
library(shinyTree)

texasCi <- readRDS("dados/texasCities.Rds")

#' Example of creating a tree from a datatable and converting the tree back to datatable
ui <- shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("Searchable shinyTree!"),
    
    sidebarPanel(
      helpText(p(HTML("A searchable Shiny Tree example &mdash; try searching for a county in the text box on the right.")),
               p(HTML("Try searching for <code>Mata</code>")),
               p("Data was irreproducibly pulled from ", tags$a(href="http://www.countymapsoftexas.com/regionsall.shtml", "this page"), "; some rows might have been missed."),
               HTML("<hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree", earch = list(
        show_only_matches = TRUE
      ), searchtime = 1000)
    )
  ))

#' Example of creating a tree from a datatable and converting the tree back to datatable
server <- shinyServer(function(input, output, session) {
  log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))
  
  output$tree <- renderTree({
    texasCi
  })
})

if(interactive()){
  shinyApp(ui, server)
}