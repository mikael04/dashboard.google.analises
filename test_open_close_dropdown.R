if (interactive()) {
  
  library("shiny")
  library("shinyWidgets")
  
  ui <- fluidPage(
    tags$h2("Toggle Dropdown Button"),
    br(),
    fluidRow(
      column(
        width = 6,
        dropdownButton(
          actionButton(inputId = "toggle2",
                       label = "Selecionar o nó"),
          tags$h3("List of Inputs"),
          selectInput(inputId = 'xcol',
                      label = 'X Variable',
                      choices = names(iris)),
          sliderInput(inputId = 'clusters',
                      label = 'Cluster count',
                      value = 3,
                      min = 1,
                      max = 9),
          circle = TRUE, status = "danger",
          inputId = "mydropdown",
          icon = icon("gear"), width = "300px"
        )
      ),
      column(
        width = 6,
        actionButton(inputId = "toggle1",
                     label = "Open dropdown")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    observeEvent(list(input$toggle1, input$toggle2), {
      toggleDropdownButton(inputId = "mydropdown")
    }, ignoreInit = F)
    
  }
  
  shinyApp(ui = ui, server = server)
  
}