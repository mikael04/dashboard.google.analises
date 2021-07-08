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
library(shinycustomloader)
library(shinyjs)
library(shinyWidgets)

gif = "nyancat.gif"

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("Busca na árvore"),
    # Show a plot of the generated distribution
    mainPanel(
        fluidRow(
            box(width = 12,
                tags$p("O nó selecionado é:"),
                verbatimTextOutput("no"),
                withLoader(collapsibleTreeOutput("plot", height = "400px", width = 1600), type="image", loader="nyancat.gif"),
                actionButton("act_plot", HTML("Plotar <br/> gráficos")),
                actionBttn(
                    inputId = "bttn2",
                    label = "Go!",
                    color = "success",
                    style = "material-flat",
                    icon = icon("sliders"),
                    block = TRUE
                ),
                ),
            verbatimTextOutput("str2"),
        ),
        fluidRow(
            withLoader(plotOutput("graph"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    debug <- T
    if(debug){
        print(paste0("Começando o servidor"))
    }
    plot <- NULL
    ## Valores reativos para alterar vendedores ao selecionar
    r <- reactiveValues(value = "default")
    df_soc_pand <- data.table::fread("dados/perguntas_full.csv")
    hierarch <- c("EIXO", "TOPICS", "QUERIES")
    output$plot <- renderCollapsibleTree({
        p <- collapsibleTree::collapsibleTree(df_soc_pand, hierarchy = hierarch,
                                              root = "COVID19",
                                              inputId = "node",
                                              collapsed = T,
                                              zoomable = F)
        p
    })
    if(debug){
        print(paste0("Árvore gerada"))
    }
    r$node <- "COVID19"
    output$str2 <- renderText(paste0(r$node))
    if(debug){
        print(paste0("Texto gerado"))
    }
    #input$node <- NULL
    observeEvent(input$node, {
        r$node <- input$node
        if(debug){
            #print(input$node)
            print(paste0("input[1] = ", r$node[1]))
            print(paste0("input[2] = ", r$node[2]))
            print(paste0("input[3] = ", r$node[3]))
            print(paste0("input[4] = ", r$node[4]))
        }
        if(r$node[4] == "NULL"){
            if(r$node[3] == "NULL"){
                if(debug) print(paste0("r$node[3] is null"))
                if(r$node[2] == "NULL"){
                    if(debug) print(paste0("r$node[2] is null"))
                    if(r$node[1] == "NULL"){
                        if(debug) print(paste0("r$node[1] is null"))
                        if(debug) print("is.null node 1")
                        no_sel <- ("COVID19")
                        if(debug) print(paste0("Nó selecionado: ", no_sel))
                        
                    }else{
                        if(debug) print("is.null node 2 else node 1")
                        no_sel <- r$node[1]
                        if(debug) print(paste0("Nó selecionado: ", no_sel))
                    }
                }else{
                    if(debug) print("is.null node 3 else node 2")
                    no_sel <- r$node[1]
                    if(debug) print(paste0("Nó selecionado: ", no_sel))
                }
            }else{
                if(debug) print("else node 3")
                no_sel <- r$node[1]
                if(debug) print(paste0("Nó selecionado: ", no_sel))
            }
        }else{
            if(debug) print("else node 4")
            no_sel <- r$node[1]
            if(debug) print(paste0("Nó selecionado: ", no_sel))
        }
        output$str2 <- renderText(paste0(no_sel))
        #output$str2 <- renderText({paste0(r$node)})
        #print("Node = ", r$node)
        if(r$node[4] == "NULL" && r$node[3] == "NULL" && r$node[2] != "NULL" && r$node[1] != "NULL"){
            if(debug){
                #print(r$node)
                print(paste0("no[1] = ", r$node[1]))
                print(paste0("no[2] = ", r$node[2]))
                print(paste0("no[3] = ", r$node[3]))
                print(paste0("no[4] = ", r$node[4]))
            }
            print("debug 1")
            raiz <- as.character(r$node[1])
            df_soc_pand_quer <- df_soc_pand %>%
                dplyr::filter(TOPICS == raiz) %>%
                dplyr::select(-EIXO, -TOPICS)
            print("debug 2")
            #print("raiz = ", raiz)
            hierarch <- c("QUESTIONS")

            # df_soc_pand
            output$plot <- renderCollapsibleTree({
                p <- collapsibleTree::collapsibleTree(df_soc_pand_quer, c("QUERIES", "QUESTIONS"),
                                                      root = raiz,
                                                      inputId = "node",
                                                      collapsed = T,
                                                      zoomable = T,
                                                      tooltipHtml = "tooltip")
                p
            })
        }
    })
    print("Erro debug 1")
    observeEvent(input$bttn2, {
        shinyjs::hide("plot")
        shinyjs::hide("graph")
        shinyjs::show("plot")
        print("Erro debug 4")
    })
    output$graph <- renderPlot({
        random_ggplot()
    })
    # output$graph2 <- renderPlot({
    #     random_ggplot()
    # })
    # 
    print(debug)
    if(debug){
        #print(node)
        print(paste0("input[1] = ", input$node[1]))
        print(paste0("input[2] = ", input$node[2]))
        print(paste0("input[3] = ", input$node[3]))
        print(paste0("input[4] = ", input$node[4]))
    }
    if(typeof(node) == "character"){
        output$no <- renderText({paste0(node)})
    }else{
        if(input$node[4] == "NULL"){
            if(input$node[3] == "NULL"){
                if(debug) print(paste0("input$node[3] is null"))
                if(input$node[2] == "NULL"){
                    if(debug) print(paste0("input$node[2] is null"))
                    if(input$node[1] == "NULL"){
                        if(debug) print(paste0("input$node[1] is null"))
                        if(debug) print("is.null node 1")
                        node <- ("COVID19")
                        if(debug) print(paste0("Nó selecionado: ", node))
                    }else{
                        if(debug) print("is.null node 2 else node 1")
                        node <- input$node[1]
                        if(debug) print(paste0("Nó selecionado: ", node))
                    }
                }else{
                    if(debug) print("is.null node 3 else node 2")
                    node <- input$node[1]
                    if(debug) print(paste0("Nó selecionado: ", node))
                }
            }else{
                if(debug) print("else node 3")
                node <- input$node[1]
                if(debug) print(paste0("Nó selecionado: ", node))
            }
        }else{
            if(debug) print("else node 4")
            node <- node[1]
            if(debug) print(paste0("Nó selecionado: ", node))
        }
    }
    output$no <- renderText({paste0(node)})
}

# Run the application 
shinyApp(ui = ui, server = server)

# 
# library(collapsibleTree)
# # Data from US Forest Service DataMart
# species <- read.csv(system.file("extdata/species.csv", package = "collapsibleTree"))
# collapsibleTree(df = species, c("REGION", "CLASS", "NAME"), fill = "green")
# 
# # Visualizing the order in which the node colors are filled
# library(RColorBrewer)
# 
# collapsibleTree(warpbreaks, c("wool", "tension"),fill = brewer.pal(9, "RdBu"),fillByLevel = TRUE)
# 
# collapsibleTree(warpbreaks, c("wool", "tension"),fill = brewer.pal(9, "RdBu"),fillByLevel = FALSE)
# 
# collapsibleTree(warpbreaks, c("wool", "tension", "breaks"),tooltip = TRUE,attribute = "breaks")
# 
# # Node size can be mapped to any numeric column, or to leafCount
# collapsibleTree(warpbreaks, c("wool", "tension", "breaks"),nodeSize = "breaks")
# 
# # collapsibleTree.Node example
# data(acme, package="data.tree")
# acme$Do(function(node) node$cost <- data.tree::Aggregate(node, attribute = "cost", aggFun = sum))
# collapsibleTree(acme, nodeSize  = "cost", attribute = "cost", tooltip = TRUE)
# 
# # Emulating collapsibleTree.data.frame using collapsibleTree.Node
# species <- read.csv(system.file("extdata/species.csv", package = "collapsibleTree"))
# hierarchy <- c("REGION", "CLASS", "NAME")
# species$pathString <- paste("species",apply(species[,hierarchy], 1, paste, collapse = "//"),sep = "//")
# df <- data.tree::as.Node(species, pathDelimiter = "//")
# collapsibleTree(df)
# 
# collapsibleTreeNetwork(df, inputId = NULL, attribute = "leafCount",aggFun = sum, fill = "lightsteelblue", linkLength = NULL,fontSize = 10, tooltip = TRUE, tooltipHtml = NULL,nodeSize = NULL, collapsed = TRUE, zoomable = TRUE, width = NULL,height = NULL)
# 
# # Create a simple org chart
# org <- data.frame(Manager = c(NA, "Ana", "Ana", "Bill", "Bill", "Bill", "Claudette", "Claudette", "Danny","Fred", "Fred", "Grace", "Larry", "Larry", "Nicholas", "Nicholas"),
#                   Employee = c("Ana", "Bill", "Larry", "Claudette", "Danny", "Erika", "Fred", "Grace","Henri", "Ida", "Joaquin", "Kate", "Mindy", "Nicholas", "Odette", "Peter"),
#                   Title = c("President", "VP Operations", "VP Finance", "Director", "Director", "Scientist","Manager", "Manager", "Jr Scientist", "Operator", "Operator", "Associate","Analyst", "Director", "Accountant", "Accountant"))
# collapsibleTree::collapsibleTreeNetwork(org, attribute = "Title")
# c1 <- c("Sociedade e Pandemia", "Sociedade e Pandemia", "Cuidados Clínicos", "Cuidados Clínicos", "Cuidados Clínicos", "Vírus e Patogênese")
# c2 <- c("Prevention and Control", "Politics and Economy", "Symptoms", "Diagnosis", "Prognosis", "Hystory and Origin")
# c3 <- c(HTML("Social lockdown"), HTML("Vacine roll-out"), HTML("CoV early symptoms"), HTML("None"), HTML("Reinfection"), HTML("SARS-CoV-2 originasd asd asd asdasd\t asd <br> asdasd asdasfgghasf"))
# df <- data.frame(EIXO = c1, TOPICS =  c2, QUERIES = c3)
# collapsibleTree::collapsibleTree(df, hierarchy = c("EIXO", "TOPICS", "QUERIES"))
# # Add in colors and sizes
# org$Color <- org$Title
# levels(org$Color) <- colorspace::rainbow_hcl(11)
# collapsibleTreeNetwork(org,attribute = "Title",fill = "Color",nodeSize = "leafCount",collapsed = FALSE)# Use unsplash api to add in random photos to tooltiporg$tooltip <- paste0(org$Employee,"<br>Title: ",org$Title,"<br><img src='https://source.unsplash.com/collection/385548/150x100'>")collapsibleTreeNetwork(org,attribute = "Title",fill = "Color",nodeSize = "leafCount",tooltipHtml = "tooltip",collapsed = FALSE)
# 
# 
# # Color in by number of children
# collapsibleTreeSummary(warpbreaks, c("wool", "tension", "breaks"), maxPercent = 50)
# # Color in by the value of breaks and use the terrain_hcl gradient
# collapsibleTreeSummary(
#     warpbreaks,
#     c("wool", "tension", "breaks"),
#     attribute = "breaks",
#     fillFun = colorspace::terrain_hcl,
#     maxPercent = 50
# )
# df_soc_pand <- data.table::fread("data-raw/perguntas_full.csv")
# df_soc_pand_quer <- df_soc_pand %>%
#     dplyr::filter(TOPICS == "Health-system arrangements") %>%
#     dplyr::select(-EIXO, -TOPICS)
# #print("df_soc_pand", df_soc_pand_quer)
# p <- collapsibleTree::collapsibleTree(df_soc_pand_quer, c("QUERIES", "QUESTIONS"),
#                                       root = "Health-system arrangements",
#                                       inputId = "node",
#                                       collapsed = T,
#                                       zoomable = T,
#                                       width = 1400,
#                                       tooltipHtml = "tooltip")
# print("Erro debug 2")
# p
