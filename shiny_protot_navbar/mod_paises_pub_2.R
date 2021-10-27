#' paises_pub_2 UI Function
#'
#' @description Módulo destinado a plotar publicações por países
#' 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import plotly
#' @import ggplot2
#' @importFrom shiny NS tagList 
mod_paises_pub_2_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("paises_pub"))
  )
}
    
#' paises_pub_2 Server Functions
#'
#' @noRd 
mod_paises_pub_2_server <- function(id, df_filtros, plotly, teste, debug){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # print("Começando o paises pub")
    if(teste){
      df_filtros_graph <- data.table::fread("data-raw/nao_mais_usados/df_paises_count_ordered.csv") |>
        dplyr::rename(Count = count)
      df_filtros_graph <- df_filtros_graph |>
        dplyr::slice_head(n = 20)
    }else{
      df_filtros_graph <- df_filtros |>
        dplyr::filter(!is.na(country) & country != "NoCountry") |>
        dplyr::group_by(country) |>
        dplyr::summarise(Count = n()) |>
        dplyr::arrange(desc(Count)) |>
        dplyr::slice_head(n = 20) |>
        dplyr::ungroup() |> 
        dplyr::rename(Paises = country)
    }
    
    output$paises_pub <- plotly::renderPlotly({
      ## Plot
      if(plotly){
          layout_axis_y <- list(
            title = "",
            showline = F,
            showticklabels = T,
            showgrid = F
          )
          layout_axis_x <- list(
            title = "",
            showline = F,
            showticklabels = T,
            showgrid = F,
            tickformat = "digit"
          )
          # m <- list(l=350, r=50, b=50, t=30, pad=4)
          # browser()
          p <- plotly::plot_ly(df_filtros_graph, x = ~Count, y = ~reorder(Paises, Count), type = 'bar')
          p <- p |>
            plotly::layout(title = ('Titulo'), xaxis = layout_axis_x, yaxis = layout_axis_y, dragmode='pan') |>
            plotly::config(modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d", "toggleSpikelines"), displaylogo = FALSE)
          
          p
          # p <- plotly::plot_ly(df_filtros_graph, type = 'bar', x = ~count, y = ~reorder(Paises, count))
          # p <- p |>
          #   plotly::layout(title = "Publicações por país", xaxis = layout_axis_x, yaxis = layout_axis_y, dragmode='pan') |>
          #   plotly::config(modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d", "toggleSpikelines"), displaylogo = FALSE)
          # p
        }else{
            p <- ggplot2::ggplot(df_filtros_graph, ggplot2::aes(x = Count, y = (reorder(Paises, Count)), text=purrr::map(paste('<b>Número de publicações:</b>', Count, '<br>', '<b>País:</b>', Paises), HTML))) +
              geom_bar(fill = "steelblue", stat = "identity") + 
              theme_minimal() + 
              labs(x = ("Número de artigos"), y = ("Países"), title = ("Publicações por país"))
              # labs(x = ("Número de artigos"), y = ("Países"), title = ("Publicações por país (20 maiores)"))
            plotly::ggplotly(p, tooltip = "text") |>
              plotly::config(modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d", "toggleSpikelines"), displaylogo = FALSE)
        }
    })
  })
}
    
## To be copied in the UI
# mod_paises_pub_2_ui("paises_pub_2_1")
    
## To be copied in the server
# mod_paises_pub_2_server("paises_pub_2_1")
