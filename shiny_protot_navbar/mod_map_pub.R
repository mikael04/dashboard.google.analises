#' map_pub UI Function
#'
#' @description Função que receberá um dataframe (com nome de países e contagem de publicações por país) e criará choropleth e contagem de pub
## Recebe df(nome dos países e contagem) e r(reativo), e parâmetro para debug
## Chama função para criar SPDF usado para plotagem (manda o df, e o nome da coluna (ainda não usado) e parâmetro de debug)
## Recebe um SPDF com todos os países e seus respectivos valores da coluna selecionada
## Usa a lib leaflet para plotar esse SPDF com a coluna desejada (e cria legenda e tooltip)
## Usa uma paleta(mypalette) de cores com intervalos pré-definidos (mybins)
## Plota o mapa dos países com suas respecitvas cores (separados e categorizados através do bin, pallete), legenda e tooltip
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_pub_ui <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("mapa_pub"))
  )
}

#' map_pub Server Functions
#'
#' @noRd 
mod_map_pub_server <- function(id, r, df_count_base_filtros, teste, debug){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # browser()
    if(teste){
      # df_count_base_filtros <- fst::read.fst("data-raw/app/df_count_base_filtros.fst") |>
      df_count_base_filtros <- df_count_base_filtros |> 
        dplyr::filter(country != "NoCountry") |> 
        dplyr::group_by(country) |> 
        dplyr::mutate(count_paises = sum(count)) |> 
        dplyr::distinct(country, .keep_all = T) |> 
        dplyr::select(NAME = country, count = count_paises) |> 
        dplyr::arrange(desc(count)) |>
        dplyr::ungroup()
    }else{
      df_count_base_filtros <- df_count_base_filtros |>
        dplyr::filter(country != "NoCountry") |> 
        dplyr::group_by(country) |> 
        dplyr::mutate(count_paises = dplyr::n()) |> 
        dplyr::distinct(country, .keep_all = T) |> 
        dplyr::ungroup() |> 
        dplyr::arrange(count_paises) |>
        dplyr::select(NAME = country, count = count_paises)
    }
    output$mapa_pub <- leaflet::renderLeaflet({
      ## Test
      #map_count <- func_create_spdf_w_col_name(df_count_base_filtros, "count", TRUE)
      map_count <- func_create_spdf_w_col_name(df_count_base_filtros, "count", debug)
      ## Criando breaks e paleta de cores
      mybins <- c(0,100,1000,5000,10000,25000,50000,100000, 1000000)
      mypalette <- leaflet::colorBin( palette="YlOrBr", domain=map_count@data$count, na.color="transparent", bins=mybins)
      
      # Criando texto tooltip
      mytext <- paste(
        "Pais: ", map_count@data$NAME,"<br/>", 
        # i18n$t("Publicações: "), map_count@data$count,
        ("Publicações: "), map_count@data$count,
        sep="") %>%
        lapply(htmltools::HTML)
      
      m <- leaflet(map_count,
                   options = list(zoomControl = T,
                                  minZoom = 1.3, maxZoom = 3,
                                  dragging = T, noWrap = T,
                                  worldCopyJump = F,
                                  maxBounds = list(
                                    list(-150, -310),
                                    list(150, 310)
                                  ))) %>% 
        addTiles()  %>% 
        setView( lat=0, lng=80 , zoom=1.3) %>%
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
      
      m
    })
  })
}

## To be copied in the UI
# mod_map_pub_ui("map_pub_1")

## To be copied in the server
# mod_map_pub_server("map_pub_1")
