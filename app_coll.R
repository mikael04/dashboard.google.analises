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
library(shinipsum)
library(collapsibleTree)
library(dplyr)
require(colorspace)

# Dataset is from https://community.tableau.com/docs/DOC-1236
load(system.file("extdata/Superstore_Sales.rda", package = "collapsibleTree"))
# For the sake of speed, let's only plot sales in Ontario
Superstore_Sales <- Superstore_Sales[Superstore_Sales$Region=="Ontario",]

# Define UI for application that draws a collapsible tree
ui <- fluidPage(
    
    # Application title
    titlePanel("Collapsible Tree Example: Gradient Mapping"),
    
    # Sidebar with a select input for the root node
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "hierarchy", "Tree hierarchy",
                choices = c(
                    "Customer Segment", "Product Category", "Product Sub-Category",
                    "Order Priority", "Product Container"
                ),
                selected = c("Customer Segment","Product Category", "Product Sub-Category"),
                multiple = TRUE
            ),
            selectInput(
                "fill", "Node color",
                choices = c("Order Quantity", "Sales", "Unit Price"),
                selected = "Sales"
            ),
            tags$p("The node you most recently clicked:"),
            verbatimTextOutput("str"),
            tags$br(),
            tags$a(href = "https://community.tableau.com/docs/DOC-1236", "Sample dataset from Tableau")
        ),
        
        # Show a tree diagram with the selected root node
        mainPanel(
            collapsibleTreeOutput("plot", height = "500px")
        )
    )
)

# Define server logic required to draw a collapsible tree diagram
server <- function(input, output) {
    df_soc_pand <- data.table::fread("perguntas_full.csv")
    output$plot <- renderCollapsibleTree({
        collapsibleTree(
            df_soc_pand,
            hierarchy = input$hierarchy,
            inputId = "node",
            root = "Sales",
            attribute = "Sales"
        )
    })
    
    output$str <- renderPrint(str(input$node))
}

# Run the application
shinyApp(ui = ui, server = server)