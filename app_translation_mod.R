library(shiny)
library(shiny.i18n)

# File with translations
i18n <- Translator$new(translation_csvs_path = "dados/translations/")
i18n$set_translation_language("en") # here you select the default translation to display

languageButton_UI <- function(id, i18n) {
  ns <- NS(id)
  fluidPage(
    shiny.i18n::usei18n(i18n),
    div(style = "float: right;",
        selectInput('selected_language',
                    i18n$t("Change language"),
                    choices = i18n$get_languages(),
                    selected = i18n$get_key_translation())
    ),
    titlePanel(i18n$t("Hello Shiny!"), windowTitle = NULL),
    sidebarLayout(
      sidebarPanel(
        sliderInput("bins",
                    i18n$t("Number of bins:"), # you use i18n object as always
                    min = 1,
                    max = 50,
                    value = 30)
      ),
      mainPanel(
        p(i18n$t("This is description of the plot."))
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
  
  
  observeEvent(input$selected_language, {
    # This print is just for demonstration
    print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })
}

shinyApp(ui, server)
