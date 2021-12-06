library(shiny)
library(dplyr)
library(sf)
library(leaflet)

mod_btn_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns("btn"), label = "plotar poligonos"),
    actionButton(inputId = ns("btn_remove"), label = "remover poligonos")
  )
}

mod_btn_server <- function(id, passMap, mytext, mypalette, map_count, first_plot,
                           df_count_base_filtros){
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    # browser()
    if(first_plot){
      observeEvent(input$btn, {
        passMap() |> 
          ## Limpando dados anteriores
          leaflet::clearShapes() |> 
          leaflet::clearControls() |> 
          ## Gerando novos polígoonos
          addPolygons(
            data = map_count,
            stroke=FALSE ,
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
          ) |> 
          addEasyButton(easyButton(
            icon="fa-globe", title="Zoom to Level 1",
            onClick=JS("function(btn, map){ map.setZoom(1); }"))) |> 
          # addProviderTiles(options = providerTileOptions(noWrap = TRUE)) %>%
          # addLegend(pal = mypalette, values = ~count, opacity=0.9, title = i18n$t("Publications"), position = "topright" )
          addLegend(pal = mypalette, values = map_count$count, opacity=0.9, title = "Publications", position = "topright" )
        
      })
    }else{
      # base::saveRDS(world_spdf, "dados/world_spdf.RDS")
      world_spdf_rds <- base::readRDS("dados/world_spdf.RDS")
      # world_spdf_simplified <- rmapshaper::ms_simplify(world_spdf)
      ## Renomeando colunas de world_spdf (esse dataframe tem todos os países)
      country_names <- as.data.frame(world_spdf_rds@data$NAME) %>%
        dplyr::rename(NAME = 'world_spdf_rds@data$NAME')
      ## Fazendo juncão para adicionar países que não possuem (NA)
      df_count_base_filtros <- dplyr::right_join(df_count_base_filtros, country_names, by=c("NAME"))
      ## Alterando valores de NA para 0
      df_count_base_filtros$count[is.na(df_count_base_filtros$count)] = 0
      
      ## Criando novo SPDF com todos os países e adicionando a coluna "count" para contagem de publicacoes
      map_count <- sp::merge(world_spdf_rds,df_count_base_filtros, by="NAME")
      
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
      
      # Criando texto tooltip
      mytext <- paste(
        "Pais: ", map_count@data$NAME,"<br/>", 
        # i18n$t("Publicações: "), map_count@data$count,
        ("Publicações: "), map_count@data$count,
        sep="") %>%
        lapply(htmltools::HTML)
      
      observeEvent(input$btn, {
        passMap() |> 
          ## Limpando dados anteriores
          leaflet::clearShapes() |> 
          leaflet::clearControls() |> 
          ## Gerando novos polígoonos
          addPolygons(
            data = map_count,
            stroke=FALSE ,
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
          ) |> 
          addEasyButton(easyButton(
            icon="fa-globe", title="Zoom to Level 1",
            onClick=JS("function(btn, map){ map.setZoom(1); }"))) |> 
          # addProviderTiles(options = providerTileOptions(noWrap = TRUE)) %>%
          # addLegend(pal = mypalette, values = ~count, opacity=0.9, title = i18n$t("Publications"), position = "topright" )
          addLegend(pal = mypalette, values = map_count$count, opacity=0.9, title = "Publications", position = "topright" )
        
      })
    }
    
    
    observeEvent(input$btn_remove, {
      passMap() |> 
        leaflet::clearShapes() |> 
        leaflet::clearControls()
    })
    
  })
}

mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("mapa_pub")),
    mod_btn_ui(ns("other"))
  )
}
mod_map_server <- function(id, first_plot){
  moduleServer( id, function(input, output, session){
    
    # here I pass map as reactive
    passMap = reactive({input$mapa_pub})
    
    proxymap <- reactive(leafletProxy('mapa_pub')) 
    if(first_plot){
      df_count_base_filtros <- data.table::fread("dados/app/first_plots/df_mod_map_pub_first_plot.csv")
      debug <- T
      # world_spdf <- rgdal::readOGR( 
      #   dsn= "dados/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp" , 
      #   layer = "TM_WORLD_BORDERS_SIMPL-0.3",
      #   verbose = F
      # )
      # base::saveRDS(world_spdf, "dados/world_spdf.RDS")
      world_spdf_rds <- base::readRDS("dados/world_spdf.RDS")
      # world_spdf_simplified <- rmapshaper::ms_simplify(world_spdf)
      ## Renomeando colunas de world_spdf (esse dataframe tem todos os países)
      country_names <- as.data.frame(world_spdf_rds@data$NAME) %>%
        dplyr::rename(NAME = 'world_spdf_rds@data$NAME')
      ## Fazendo juncão para adicionar países que não possuem (NA)
      df_count_base_filtros <- dplyr::right_join(df_count_base_filtros, country_names, by=c("NAME"))
      ## Alterando valores de NA para 0
      df_count_base_filtros$count[is.na(df_count_base_filtros$count)] = 0
      
      ## Criando novo SPDF com todos os países e adicionando a coluna "count" para contagem de publicacoes
      map_count <- sp::merge(world_spdf_rds,df_count_base_filtros, by="NAME")
      
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
      
      # Criando texto tooltip
      mytext <- paste(
        "Pais: ", map_count@data$NAME,"<br/>", 
        # i18n$t("Publicações: "), map_count@data$count,
        ("Publicações: "), map_count@data$count,
        sep="") %>%
        lapply(htmltools::HTML)
      
      
      output$mapa_pub <- leaflet::renderLeaflet({
        m <- leaflet(map_count,
                     options = list(zoomControl = T,
                                    minZoom = 1, maxZoom = 3,
                                    dragging = T, noWrap = T,
                                    worldCopyJump = F,
                                    maxBounds = list(
                                      list(-150, -310),
                                      list(150, 310)
                                    )))
        m <- m |> 
          addTiles() |> 
          setView( lat=0, lng=22 , zoom=1)
        m
      })
      
      
      
      mod_btn_server("other", proxymap, mytext, mypalette, map_count, first_plot,
                     df_count_base_filtros)  
    }else{
      # ns <- session$ns
      # browser()
      paises_alterados <- c("Brazil", "Micronesia", "Bermudas", "Vanuatu", "Cuba", "Iceland")
      df_count_base_filtros <- data.table::fread("dados/app/first_plots/df_mod_map_pub_first_plot.csv")
      df_count_base_filtros <- df_count_base_filtros |> 
        dplyr::mutate(count = if_else(NAME %in% paises_alterados, as.integer(150), as.integer(count)))
      mod_btn_server("other", proxymap, mytext, mypalette, map_count, first_plot,
                     df_count_base_filtros)  
    }
  })
}

ui <- fluidPage(
  tagList(
    mod_map_ui("mapa"),
    actionButton(inputId = "btn_rem", label = "MAIN remover poligonos")
  )
)

server <- function(input, output, session){
  first_plot <- reactiveValues(value = TRUE)
  
  mod_map_server("mapa", T)
  observeEvent(input$btn_rem, {
    mod_map_server("mapa", F)
  })
}

shinyApp(ui = ui, server = server)