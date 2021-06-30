#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(dqshiny)
library(rhandsontable)
library(shiny)
library(jsonlite)

countries <- c('Brazil', 'France', 'Germany', 'USA')
my_autocomplete_list <- c("John Doe","Ash","Ajay sharma",
                          "Ken Chong","Will Smith","Neo")
opts <- sapply(1:100000, function(i) paste0(sample(letters, 7), collapse=""))
questions <- c("What are the effective pre-exposure prophylactics (PreP) for COVID?",
               "How much/Has social distancing had an impact on slowing the spread of COVID-19?",
               "How much/What is the impact of school closure in handling the COVID-19?",
               "How/How much OPENING AND CLOSING POLICY DECISIONS influence trends in COVID-19 cases?")

comboTreeInput <- function(inputId, width = "30%", height = "100px", 
                           choices, multiple = FALSE, cascaded = TRUE){
    tags$div(style = sprintf("width: %s; height: %s;", width, height),
             tags$input(id = inputId, class = "comboTree", type = "text", 
                        placeholder = "Select",
                        `data-choices` = as.character(toJSON(choices, auto_unbox = TRUE)),
                        `data-multiple` = ifelse(multiple, "true", "false"), 
                        `data-cascaded` = ifelse(cascaded, "true", "false")
             )
    )
}

choices <- list(
    list(id = 1, title = "item1"),
    list(id = 2, title = "item2", 
         subs = list(
             list(id = 21, title = "item2-1"), 
             list(id = 22, title = "item2-2")
         )
    ), 
    list(id = 3, title = "item3",
         subs = list(
             list(id = 31, title = "item3-1", isSelectable = FALSE,
                  subs = list(
                      list(id = 311, title = "item3-1-1"),
                      list(id = 312, title = "item3-1-2")
                  )
             ),
             list(id = 32, title = "item3-2")
         )
    )
)


# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", href = "style.css"),
        tags$link(rel = "stylesheet", href = "https://cdn.materialdesignicons.com/5.0.45/css/materialdesignicons.min.css"),
        tags$script(src = "comboTreePlugin.js"),
        tags$script(src = "comboTreeBinding.js")
    ),
    # Application title
    titlePanel("Tipos de buscas"),
    br(),
    h3("You selected:"),
    verbatimTextOutput("selections"),
    br(),
    comboTreeInput("mycombotree", choices = choices),
    
    selectizeInput(
        inputId = 'search',
        label = 'Search',
        choices = questions,
        selected = NULL,
        multiple = TRUE, # allow for multiple inputs
        options = list(create = FALSE) # if TRUE, allows newly created inputs
    ),
    # multiInput(
    #     inputId = "Id010",
    #     label = "Countries :", 
    #     choices = NULL,
    #     choiceNames = lapply(seq_along(countries), 
    #                          function(i) tagList(tags$img(src = flags[i],
    #                                                       width = 20, 
    #                                                       height = 15), countries[i])),
    #     choiceValues = countries
    # )
    fluidRow(
        column(3,
               autocomplete_input("auto2", "Named:", max_options = 1000,
                                  structure(opts, names = opts[order(opts)]))
               # feel free to test this with select... and may get yourself a coffee
               # , selectInput("sel", "Select:", opts)
        ), column(3,
                  tags$label("Value:"), verbatimTextOutput("val1", placeholder = TRUE)
        )
    ),
    dq_handsontable_output("myTable", 9L),
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$val2 <- renderText(as.character(input$auto2))
    hw <- c("Hello", "my", "funny", "world!")
    data <- data.frame(A = rep(hw, 500L), B = hw[c(2:4, 1L)], C = 1:500, D = 500:1)
    dq_render_handsontable("myTable", data,
                           filters = c("Sel", "Text", NA, "Auto"), sorting = TRUE,
                           page_size = c(17L, 5L, 500L, 1000L),
                           col_param = list(list(col = 3L, format = "0.00")),
                           cell_param = list(list(row = 2:9, col = 2L, readOnly = TRUE))
    )
    output[["selections"]] <- renderPrint({
        input[["mycombotree"]]
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
# library(dplyr)
# df_soc_pand <- readxl::read_excel(path = "data-raw/perguntas.xlsx", sheet=1) %>%
#     filter(!is.na(TOPICS))
# # 
# df <- data.frame(matrix(unlist(df_soc_pand), nrow=length(df_soc_pand), byrow=TRUE))
# 
# # list_soc_pand <- purrr::transpose(df_soc_pand)
# list_df <- purrr::transpose(df)
# list_df
# list_soc_pand
# choices
# 
# asplit(df_soc_pand, 2)
# asplit(df, 1)
# 
# split(df, row(df)[, 1])
# split(df, row(df_soc_pand)[, 1])
# p <- collapsibleTree::collapsibleTreeNetwork(df_soc_pand, c("TOPICS", "QUERIES", "QUESTIONS"),
#                                              #root = "Sociedade e Pandemia",
#                                              attribute = "QUESTIONS",
#                                              width = 2000,
#                                       collapsed = T,
#                                       zoomable = T,
#                                       tooltipHtml = "tooltip")
# p
# org <- data.frame(
#     Manager = c(
#         NA, "Ana", "Ana", "Bill", "Bill", "Bill", "Claudette", "Claudette", "Danny",
#         "Fred", "Fred", "Grace", "Larry", "Larry", "Nicholas", "Nicholas"
#     ),
#     Employee = c(
#         "Ana", "Bill", "Larry", "Claudette", "Danny", "Erika", "Fred", "Grace",
#         "Henri", "Ida", "Joaquin", "Kate", "Mindy", "Nicholas", "Odette", "Peter"
#     ),
#     Title = c(
#         "President", "VP Operations", "VP Finance", "Director", "Director", "Scientist",
#         "Manager", "Manager", "Jr Scientist", "Operator", "Operator", "Associate",
#         "Analyst", "Director", "Accountant", "Accountant"
#     )
# )
# p2 <- collapsibleTree::collapsibleTree(org, c("Manager"), collapsed = T)
# p2
# # choices
# # 
# shiny::runApp(system.file("examples/03shiny", package = "collapsibleTree"))
