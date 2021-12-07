library(shiny)
library(plotly)

func_plotly_modifica <- function(plotly_id, proxyPlotly, df_type_date, plot, remove){
  observe({
    if(remove){
      # browser()
      p <- proxyPlotly()
      # plotly::plotlyProxy('evol_type_pub', session, FALSE) |>
      ## Removendo linhas anteriores
      df_type_date_article <- df_type_date |> 
        dplyr::filter(type == "article")
      df_type_date_book <- df_type_date |> 
        dplyr::filter(type == "book")
      df_type_date_chapter <- df_type_date |> 
        dplyr::filter(type == "chapter")
      df_type_date_monograph <- df_type_date |> 
        dplyr::filter(type == "monograph")
      df_type_date_preprint <- df_type_date |> 
        dplyr::filter(type == "preprint")
      df_type_date_proceeding <- df_type_date |> 
        dplyr::filter(type == "proceeding")
      p |> 
        plotlyProxyInvoke("deleteTraces", list(as.integer(0))) |> 
        plotlyProxyInvoke("deleteTraces", list(as.integer(0))) |> 
        plotlyProxyInvoke("deleteTraces", list(as.integer(0))) |> 
        plotlyProxyInvoke("deleteTraces", list(as.integer(0))) |> 
        plotlyProxyInvoke("deleteTraces", list(as.integer(0))) |> 
        plotlyProxyInvoke("deleteTraces", list(as.integer(0))) |> 
        plotlyProxyInvoke("addTraces",
                          list(
                            list(
                              x = df_type_date_article$date, y = df_type_date_article$count,
                              type = 'scatter',
                              mode = 'line',
                              name = "article"
                            ),
                            list(
                              x = df_type_date_book$date, y = df_type_date_book$count,
                              type = 'scatter',
                              mode = 'line',
                              name = "book"
                            ),
                            list(
                              x = df_type_date_chapter$date, y = df_type_date_chapter$count,
                              type = 'scatter',
                              mode = 'line',
                              name = "chapter"
                            ),
                            list(
                              x = df_type_date_monograph$date, y = df_type_date_monograph$count,
                              type = 'scatter',
                              mode = 'line',
                              name = "monograph"
                            ),
                            list(
                              x = df_type_date_preprint$date, y = df_type_date_preprint$count,
                              type = 'scatter',
                              mode = 'line',
                              name = "preprint"
                            ),
                            list(
                              x = df_type_date_proceeding$date, y = df_type_date_proceeding$count,
                              type = 'scatter',
                              mode = 'line',
                              name = "proceeding"
                            )
                          )
        )
      # df_type_date, x = ~date, y = ~count, mode = 'line', type = 'scatter',
      # color = ~type
    }
  })
}

mod_plotly_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("evol_type_pub"))
  )
}

mod_plotly_server <- function(id, session, df_type_date, plotly, first_plot, remove, debug){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # output$graphe <- renderPlotly({
    #   plot_ly() %>%
    #     layout(title="test") %>%
    #     add_trace(x=runif(2), y=runif(2), name="ABC_test", type="scatter", mode="lines+markers")
    # })
    # 
    # observeEvent(input$update, {
    #   plotlyProxy("graphe", session, FALSE) %>%
    #     plotlyProxyInvoke("deleteTraces", list(as.integer(0))) %>%
    #     plotlyProxyInvoke("addTraces", list(x=runif(2),
    #                                         y=runif(2),
    #                                         name="ABC_test",
    #                                         type = 'scatter',
    #                                         mode = 'lines+markers'))
    # })
    
    # here I pass map as reactive
    passMap = reactive({input$evol_type_pub})
    proxyPlotly <- reactive(plotly::plotlyProxy('evol_type_pub', session, FALSE))
    if(debug){
      # print("teste == false, usando database recebido")
    }
    if(first_plot){
      output$evol_type_pub <- plotly::renderPlotly({
        if(plotly){
          p <- plotly::plot_ly(df_type_date, x = ~date, y = ~count, mode = 'line', type = 'scatter',
                               color = ~type) |>
            plotly::layout(title = "",
                           xaxis = list(title = "",
                                        type = 'date',
                                        tickformat = "%B<br>%Y"),
                           yaxis = list(title = "Número de publicações")) |>
            plotly::config(modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d",
                                                      "toggleSpikelines"), displaylogo = FALSE)
          p
        }else{
          p <- ggplot2::ggplot(data = df_type_date, aes(x = date, y = count, color = type, group = type,
                                                        text=(paste0('<b>Data:</b>', date, '<br>',
                                                                     '<b>Número de publicações:</b>', count, "<br>",
                                                                     '<b>Tipo:</b>', type)))) + 
            geom_line() +
            theme_minimal() +
            labs(x = ("Tempo"), y = ("Número de artigos"),
                 title = ("Publicações ao longo do tempo, por tipo"),
                 colour = ("Tipo"))
          
          plotly::ggplotly(p, tooltip = "text")  |>
            plotly::config(modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d", "select2d",
                                                      "pan2d", "pan3d",
                                                      "lasso2d", "autoScale2d", "toggleSpikelines", "resetScale2d"
            ),
            displaylogo = FALSE)
        }
      })
    }else{
      # df_type_date <- df_dimensions_type_date_country |>
      #   dplyr::filter(!is.na(date)) |>
      #   # dplyr::mutate(date_normal = format(as.Date(date_normal), "%Y-%m")) |>
      #   dplyr::group_by(date, type) |>
      #   dplyr::summarise(count_date_type = n()) |>
      #   # dplyr::distinct(date, .keep_all = T) |>
      #   dplyr::ungroup() |> 
      #   dplyr::rename(count = count_date_type)
      if(!remove){
        func_plotly_modifica(plotly_id = "evol_type_pub", proxyPlotly, df_type_date, plot = T, remove = F)
      }else{
        func_plotly_modifica(plotly_id = "evol_type_pub", proxyPlotly, df_type_date, plot = F, remove = T)
      }
      
    }
    
    
  })
}
  


ui <- fluidPage(
  tagList(
    mod_plotly_ui("plotly"),
    actionButton(inputId = "btn_remove", label = "Remover dados"),
    actionButton(inputId = "btn_change", label = "Adicionar novos dados")
  )
)
server <- function(input, output, session) {
  plotly = T
  df_type_date <- data.table::fread("dados/app/first_plots/df_mod_evol_pub_tipo_first_plot.csv")
  
  mod_plotly_server("plotly", session, df_type_date, plotly, first_plot = T, remove = F, debug = F)
  observeEvent(input$btn_change, {
    df_type_date <- data.table::fread("dados/app/first_plots/df_mod_evol_pub_tipo_first_plot.csv") |> 
      dplyr::filter(!is.na(date)) |>
      dplyr::mutate(count = dplyr::if_else(type == 'book', as.integer(10000), count))
    
    mod_plotly_server("plotly", session, df_type_date, plotly, first_plot = F, remove = F, debug = F)
  })
  observeEvent(input$btn_remove, {
    mod_plotly_server("plotly", session, df_type_date, plotly, first_plot = F, remove = T, debug = F)
  })
}

shinyApp(ui, server)