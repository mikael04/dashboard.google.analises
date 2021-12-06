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
mod_map_pub_server <- function(id, first_plot, debug){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ## Gerando o mapa inicial, apenas layout
    map_count <- func_create_spdf_w_col_name(df_count_base_filtros, "count", debug)
    
    m <- leaflet(map_count,
                 options = list(zoomControl = T,
                                minZoom = 1, maxZoom = 3,
                                dragging = T, noWrap = T,
                                worldCopyJump = F,
                                maxBounds = list(
                                  list(-150, -310),
                                  list(150, 310)
                                ))) %>% 
      addTiles()  %>% 
      setView( lat=0, lng=22 , zoom=1)
    
    # here I pass map as reactive
    passMap = reactive({input$mapa_pub})
    
    proxymap <- reactive(leafletProxy('mapa_pub'))
    
    mod_alterar_mapa_server(input$att_grap_filtros_perg, proxymap, df_count_base_filtros, map_count, debug)
    
    
    # if(isolate(first_plot)){
    #   # browser()
    #   df_count_base_filtros <- data.table::fread("../dados/app/first_plots/df_mod_map_pub_first_plot.csv")
    # }else{
    #   df_count_base_filtros <- df_count_base_filtros |>
    #     dplyr::filter(country != "NoCountry") |> 
    #     dplyr::group_by(country) |> 
    #     dplyr::mutate(count_paises = n()) |> 
    #     dplyr::distinct(country, .keep_all = T) |> 
    #     dplyr::ungroup() |> 
    #     dplyr::arrange(count_paises) |>
    #     dplyr::select(NAME = country, count = count_paises)
    #   
    # }
    # 
    output$mapa_pub <- leaflet::renderLeaflet({
      ## Test
      # browser()
      #map_count <- func_create_spdf_w_col_name(df_count_base_filtros, "count", TRUE)
      m
      
    })
  })
  # proxymap
}

## To be copied in the UI
# mod_map_pub_ui("map_pub_1")

## To be copied in the server
# mod_map_pub_server("map_pub_1")
