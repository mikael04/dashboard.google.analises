#' map_pub 
#'
#' @description Uma função que recebe um df e devolve um SPDF com merge neste primeiro df (e substitui NAs por 0)
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

func_create_spdf_w_col_name <- function(df_count_ordered, col_name, debug){
  ## Lendo SPDF base
  world_spdf <- rgdal::readOGR( 
    dsn= "shiny_protot_navbar/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp" , 
    layer = "TM_WORLD_BORDERS_SIMPL-0.3",
    verbose = F
  )
  ## Renomeando colunas de world_spdf (esse dataframe tem todos os países)
  country_names <- as.data.frame(world_spdf@data$NAME) %>%
    dplyr::rename(NAME = 'world_spdf@data$NAME')
  ## Fazendo juncão para adicionar países que não possuem (NA)
  df_count_ordered <- right_join(df_count_ordered, country_names, by=c("NAME"))
  ## Alterando valores de NA para 0
  df_count_ordered$count[is.na(df_count_ordered$count)] = 0
  
  ## Criando novo SPDF com todos os países e adicionando a coluna "count" para contagem de publicacoes
  oo <- sp::merge(world_spdf,df_count_ordered, by="NAME")
  return(oo)
}
