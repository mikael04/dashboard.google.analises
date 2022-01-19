
library(blastula)
library(keyring)

# create_smtp_creds_file(
#   file = "mikael_sup_gmail_creds",
#   user = "mikael.coletto.sup@gmail.com",
#   provider = "gmail",
#   
# )



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Título"),
  shinyWidgets::actionBttn("feedback", "Enviar sugestão", style = "material-flat")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$feedback, {
    # browser()
    showModal(modalDialog(
      textInput("usuario", "Digite seu nome (Opcional)"),
      textInput("email_usuario", "Digite seu email para contato (Opcional)"),
      textAreaInput("texto_usuario", "Digite a sua mensagem", rows = 6),
      footer = tagList(actionButton("send_email", "Enviar sugestão")),
      easyClose = TRUE
    ))
  })
  observeEvent(input$send_email, {
    # browser()
    usuario <- input$usuario
    email_usuario <- input$email_usuario
    texto_usuario <- input$texto_usuario
    date_time <- blastula::add_readable_time()
    email <- NULL
    email <-
      blastula::compose_email(
        body = blastula::md(glue::glue(
          texto_usuario)),
        footer = blastula::md(glue::glue("Email enviado por {usuario}, email: {email_usuario}, em {date_time}."))
      )
    
    email |> 
      smtp_send(
        to = "mikael.coletto.sup@gmail.com",
        from = "mikael.coletto.sup@gmail.com",
        subject = "Testando envio",
        credentials = creds_file("mikael_sup_gmail_creds")
      )
    removeModal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




