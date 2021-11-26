#' evol_pub_tipo UI Function
#'
#' @description Gráfico que mostra a evolução de publicações agrupadas por tipo de publicação e mês
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import plotly
#' @import ggplot2
#' @import purrr
#' @importFrom shiny NS tagList 
mod_evol_pub_tipo_ui <- function(id, plotly){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("evol_type_pub"))
  )
}
    
#' evol_pub_tipo Server Functions
#'
#' @noRd 
mod_evol_pub_tipo_server <- function(id, df_dimensions_type_date_country, plotly, i18n, teste, debug){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # browser()
    if(teste){
      df_type_date <- data.table::fread("../dados/df_dimensions_type_date.csv") |> 
        dplyr::rename(date = date_normal)
      if(debug){
        print("teste == true, usando database teste")
      }
    }else{
      # browser()
      df_type_date <- df_dimensions_type_date_country |>
        dplyr::filter(!is.na(date)) |>
        # dplyr::mutate(date_normal = format(as.Date(date_normal), "%Y-%m")) |>
        dplyr::group_by(date, type) |>
        dplyr::summarise(count_date_type = dplyr::n()) |>
        # dplyr::distinct(date, .keep_all = T) |>
        dplyr::ungroup() |> 
        dplyr::rename(count = count_date_type)
      if(debug){
        print("teste == false, usando database recebido")
      }
    }
    
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
        
        plotly::ggplotly(p, tooltip = "text") |>
          plotly::config(modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d", "toggleSpikelines"), displaylogo = FALSE)
      
    }
    })
  })
}
    
## To be copied in the UI
# mod_evol_pub_tipo_ui("evol_pub_tipo_1")
    
## To be copied in the server
# mod_evol_pub_tipo_server("evol_pub_tipo_1")
