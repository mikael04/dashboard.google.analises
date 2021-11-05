library(shiny)




ui <- fluidPage(
  sidebarPanel(
    prettySwitch(
      inputId = "Id027",
      label = "", 
      fill = TRUE,
      bigger = T,
      width = NULL
    ),
    prettySwitch(
      inputId = "Id028",
      label = "", 
      fill = TRUE,
      bigger = T,
      width = "100px"
    ),
    prettySwitch(
      inputId = "Id029",
      label = "", 
      fill = TRUE,
      bigger = T,
      width = "50%"
    ),
    prettySwitch(
      inputId = "Id030",
      label = "", 
      fill = TRUE,
      bigger = T,
      width = "100%"
    ),
    
    # Only show this panel if the plot type is a histogram
    conditionalPanel(
      condition = "input.plotType == 'hist'",
      selectInput(
        "breaks", "Breaks",
        c("Sturges",
          "Scott",
          "Freedman-Diaconis",
          "[Custom]" = "custom")),
      
      # Only show this panel if Custom is selected
      conditionalPanel(
        condition = "input.breaks == 'custom'",
        sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
      )
    )
  )
)

server <- function(input, output) {
  
  
}

shinyApp(ui = ui, server = server)