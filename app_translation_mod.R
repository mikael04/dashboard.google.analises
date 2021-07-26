library(shiny)
library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "dados/translations/translation.json")

i18n$set_translation_language("fr")

languageButton_UI <- function(id, i18n) {
  ns <- NS(id)
  tagList(selectInput('Selecione ao idioma',
                      i18n$t("Selecione o idioma"),
                      choices = i18n$get_languages(),
                      selected = i18n$get_key_translation())
  )
}

languageButton_Server <- function(id, global_session) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      observeEvent(input$go,{
        print(input$go[1])
        if((input$go[1] %% 2) != 0){
          updateActionButton(session, "go",
                             label = "Français")
          update_lang(global_session, "en")
        } else {
          updateActionButton(session, "go",
                             label = "English")
          update_lang(global_session, "fr")
        }
      })
    }
  )
}

ui <- fluidPage(
  usei18n(i18n),
  languageButton_UI("language_button", i18n = i18n)
)

server <- function(input, output, session) {
  languageButton_Server("language_button", global_session = session)
}

shinyApp(ui, server)
