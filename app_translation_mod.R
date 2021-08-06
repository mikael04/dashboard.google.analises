library(shiny)
library(shiny.i18n)

# File with translations
i18n <- Translator$new(translation_csvs_path = "dados/translations/")

languageButton_UI <- function(id, i18n) {
  ns <- NS(id)
  fluidPage(
    shiny.i18n::usei18n(i18n),
    div(style = "float: right;",
        selectInput('selected_language',
                    i18n$t("Mude o idioma"),
                    choices = i18n$get_languages(),
                    selected = i18n$get_key_translation())
    ),
    titlePanel(i18n$t("Olá Shiny!"), windowTitle = NULL),
    sidebarLayout(
      sidebarPanel(
        sliderInput("bins",
                    i18n$t("Número de bins:"), # you use i18n object as always
                    min = 1,
                    max = 50,
                    value = 30),
        selectInput("idPais", i18n$t(""), choices = c("")),
        selectInput("select", i18n$t("Escolha"), c("um", "dois", "três"))
        
      ),
      mainPanel(
        p(i18n$t("Essa é a descrição do plot."))
      )
    )
  )
}

languageButton_Server <- function(id, global_session) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
    }
  )
}

ui <- fluidPage(
  usei18n(i18n),
  languageButton_UI("language_button", i18n = i18n)
)

server <- function(input, output, session) {
  languageButton_Server("language_button", global_session = session)
  i18n_r <- reactive({
    i18n
  })
  r_aux <- reactiveVal(0)
  value <- reactiveVal(0)
  observeEvent(input$selected_language, {
    lang <- ifelse(input$selected_language == "pt", "pt", "en")
      # browser()
      # This print is just for demonstration
      print(paste("Language change!", input$selected_language))
      print(paste("Language change lang!", lang))
      # Here is where we update language in session
      shiny.i18n::update_lang(session, lang)
      i18n_r()$set_translation_language(lang)
  })

  observe({
    updateSelectInput(session, "idPais",
                      label = i18n_r()$t("Selecione o país:"),
                      choices = i18n_r()$t(c("Todos os países", "Brasil", "Argentina", "Chile", "Estados Unidos", "Uruguai")))
    updateSelectInput(session, "select", label = i18n_r()$t("Escolha"),
                      choices = i18n_r()$t(c("um", "dois", "três")))
  })
      
    
}

shinyApp(ui, server)

