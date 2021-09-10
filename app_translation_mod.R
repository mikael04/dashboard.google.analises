library(shiny)
library(shiny.i18n)
library(plotly)
library(ggplot2)

# File with translations
# i18n <- Translator$new(translation_csvs_path = "dados/translations/")
i18n <- Translator$new(translation_json_path = "dados/translation_json/translations.json")

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
        p(i18n$t("Essa é a descrição do plot.")),
        plotly::plotlyOutput(ns("evol_type_pub"))
      )
    )
  )
}

languageButton_Server <- function(id, global_session, i18n) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      debug = T
      plotly = F
      teste = T
        df_type_date <- data.table::fread("dados/df_dimensions_type_date.csv")
        if(debug){
          print("teste == true, usando database teste")
        }
      
      output$evol_type_pub <- plotly::renderPlotly({
        if(plotly){
          p <- plotly::plot_ly(df_type_date, x = ~date_normal, y = ~count, mode = 'line', type = 'scatter',
                               color = ~type) %>%
            plotly::layout(title = "",
                           xaxis = list(title = "",
                                        type = 'date',
                                        tickformat = "%B<br>%Y"),
                           yaxis = list(title = "Número de publicações")) %>%
            plotly::config(modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d",
                                                      "toggleSpikelines"), displaylogo = FALSE)
          p
        }else{
          p <- ggplot2::ggplot(data = df_type_date, aes(x = date_normal, y = count, group = type,
                                                        text=(paste('<b>Data:</b>', date_normal, '<br>',
                                                                    '<b>Número de publicações:</b>', count, "<br>",
                                                                    '<b>Tipo:</b>', type)))) + 
            geom_line() +
            theme_minimal() +
            scale_fill_identity(guide = 'legend') +
            scale_colour_manual(name = i18n$t('Tipo'), 
                                values = c('black'='black','red'='red', 'black'='black','blue'='blue', 'green'='green','yellow'='yellow'),
                                labels = c(i18n$t('artigos'),i18n$t('livros'),i18n$t('capítulos'),i18n$t('monografias'),i18n$t('pré-impressões'), i18n$t('proceeding'))) + 
            labs(x = i18n$t("Tempo"), y = i18n$t("Número de artigos") , title = i18n$t("Publicações ao longo do tempo, por tipo"))
          
          # p
          
          # x=seq(1,10,length=100)
          # data=data.frame(x,dnorm(x,mean=6.5,sd=1))
          # names(data)=c('x','new.data')
          # x.ribbon=seq(1,10,length=20)
          # ribbon=data.frame(x.ribbon,
          #                   dnorm(x.ribbon,mean=5,sd=1)+.01,
          #                   dnorm(x.ribbon,mean=5,sd=1)-.01,
          #                   dnorm(x.ribbon,mean=5,sd=1))
          # names(ribbon)=c('x.ribbon','max','min','avg')
          # p2 <- ggplot()+geom_ribbon(data=ribbon,aes(ymin=min,ymax=max,x=x.ribbon,fill='lightgreen'))+
          #   geom_line(data=ribbon,aes(x=x.ribbon,y=avg,color='black'))+
          #   geom_line(data=data,aes(x=x,y=new.data,color='red'))+
          #   xlab('x')+ylab('density') + 
          #   scale_fill_identity(guide = 'legend',labels = c('m1')) +
          #   scale_colour_manual(values =c('black'='black','red'='red'), labels = c('c2','c1'))
          
          # p2
          # ggplotly(p, tooltip = "text") %>%
          #   plotly::config(modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d", "toggleSpikelines"), displaylogo = FALSE)
          library(plotly)
          library(tidyr)
          library(plyr)
          
          data <- spread(Orange, Tree, circumference)
          data <- rename(data, c("1" = "Tree1", "2" = "Tree2", "3" = "Tree3", "4" = "Tree4", "5" = "Tree5"))
          
          fig <- plot_ly(data, x = ~age, y = ~Tree1, type = 'scatter', mode = 'lines', name = 'Tree 1')
          fig <- fig %>% add_trace(y = ~Tree2, name = 'Tree 2')
          fig <- fig %>% add_trace(y = ~Tree3, name = 'Tree 3')
          fig <- fig %>% add_trace(y = ~Tree4, name = 'Tree 4')
          fig <- fig %>% add_trace(y = ~Tree5, name = 'Tree 5')
          
          fig
          ggplotly(fig) %>%
              plotly::config(modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d", "toggleSpikelines"), displaylogo = FALSE)
            
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
  languageButton_Server("language_button", global_session = session, i18n)
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
    languageButton_Server("language_button", global_session = session, i18n)
  })
      
    
}

shinyApp(ui, server)

