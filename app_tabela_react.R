library(shiny)
library(shinydashboard)
library(dplyr)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)

gif = "nyancat.gif"
df_dimensions_cut <- fst::read_fst("dados/dimensions_compressed.fst") %>%
  dplyr::select(id, date_normal, subtitles, type, research_org_country_names,
                title.preferred)
df_perguntas <- data.table::fread("dados/buscaCompleta2305.csv") %>%
  dplyr::select(-abstract.preferred, -title.preferred)
df_perguntas_dict <- data.table::fread("dados/Relacao.csv")

## manipulando para ter mesmo nome de coluna do df_perguntas
df_perguntas_dict[,1] <- lapply(df_perguntas_dict[,1], gsub, pattern = " ", replacement = "", fixed = T)
df_perguntas_dict[,1] <- lapply(df_perguntas_dict[,1], gsub, pattern = "B", replacement = "b", fixed = T)

# dt1 <- df_perguntas
newnames = df_perguntas_dict$Pergunta
# oldnames = df_perguntas_dict$Busca
# for(i in 1:68){
#   ##nesse caso, eu sei que as colunas estão ordenadas
#   colnames(df_perguntas)[i+3] <- newnames[i]
# }

df_dimensions_ij_perguntas <- dplyr::inner_join(df_dimensions_cut,
                                                df_perguntas, by="id")
# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  # Application title
  titlePanel("Busca tabela"),
  # Show a plot of the generated distribution
  mainPanel(
    fluidRow(
      selectInput(
        inputId = "question",
        label = "Question",
        choices = newnames,
        selected = newnames[1],
        multiple = F
      ),
      h3("Pergunta selecionada:"),
      textOutput("sel_question")
    ),
    fluidRow(
      shinycssloaders::withSpinner(reactable::reactableOutput("table")))
  )
)
df_dimensions_ij_perguntas
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table <- reactable::renderReactable({
    reactable::reactable(df_dimensions_ij_perguntas[,1:8])
  })
  output$sel_question <- renderText({
    "Selecione uma pergunta"
  })
  observeEvent(input$question, {
    col_name <- df_perguntas_dict[df_perguntas_dict$Pergunta == input$question]$Busca
    df_dimensions_ij_perguntas_search <- df_dimensions_ij_perguntas %>%
      dplyr::filter(!!as.name(col_name) == '1')
    output$table <- reactable::renderReactable({
      reactable::reactable(df_dimensions_ij_perguntas_search[,1:8])
    })
    
    output$sel_question <- renderText({
      input$question
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)