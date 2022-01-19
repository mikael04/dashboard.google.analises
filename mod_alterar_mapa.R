#' alterar_mapa 
#'
#' @description
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
mod_alterar_mapa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns("btn"), label = "plotar poligonos"),
    actionButton(inputId = ns("btn_remove"), label = "remover poligonos")
  )
}

mod_alterar_mapa_server <- function(passMap, df_count_base_filtros, map_count, debug){
  moduleServer( id, function(input, output, session){
    observeEvent(input$att_grap_filtros_perg, {
      # Criando texto tooltip
      mytext <- paste(
        "Pais: ", map_count@data$NAME,"<br/>", 
        # i18n$t("Publicações: "), map_count@data$count,
        ("Publicações: "), map_count@data$count,
        sep="") %>%
        lapply(htmltools::HTML)
      
      ## Criando breaks e paleta de cores
      max_count <- base::max(map_count@data$count)
      min_count <- base::min(map_count@data$count)
      if(max_count > 100000){
        mybins <- c(0,100,1000,5000,10000,25000,50000,100000,1000000)
      }else{
        if(max_count > 10000){
          mybins <- c(0,10,100,500,1000,2500,5000,10000,100000)
        }else{
          if(max_count > 1000){
            mybins <- c(0,1,10,50,100,250,500,1000,10000)
          }else{
            mybins <- c(0,1,5,10,25,50,100,500,1000)
          }
        }
      }
      mypalette <- leaflet::colorBin( palette="YlOrBr", domain=map_count@data$count, na.color="transparent", bins=mybins)
      passMap() |> 
        ## Limpando dados anteriores
        leaflet::clearShapes() |> 
        leaflet::clearControls() |> 
        addPolygons( stroke=FALSE ,
                     fillOpacity = 0.5, smoothFactor = 0.5,
                     fillColor = ~mypalette(count),
                     color = "white",
                     weight = 0.3,
                     label = mytext,
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", padding = "3px 8px"), 
                       textsize = "13px", 
                       direction = "auto"
                     )
        ) %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Zoom to Level 1",
          onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
        # addProviderTiles(options = providerTileOptions(noWrap = TRUE)) %>%
        # addLegend(pal = mypalette, values = ~count, opacity=0.9, title = i18n$t("Publications"), position = "topright" )
        addLegend(pal = mypalette, values = ~count, opacity=0.9, title = "Publications", position = "topright" )
    })
    
  })
}