#' filtrar_dim 
#'
#' @description Função usada para filtrar o banco de dados, após seleção de parâmetros
#' list_parameters 1 -> ano
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_filtrar_dim  <- function(df_dimensions_filters, list_parameters, debug){
  tipo_sel <- list_parameters[[1]]
  ano_sel <- list_parameters[[2]]
  pais_sel <- list_parameters[[3]]
  ## mantendo apenas campos usados
  df_dimensions_selection <- df_dimensions_filters %>%
    dplyr::select(type, date, paises, count)
  ## Filtrando tipo de artigo -----
  tipo_sel <- as.character(tipo_sel)
  if(length(tipo_sel) <= 1){ ## casos onde tem um argumento (um tipo ou todos)
    if(tipo_sel == "TODOS"){
      df_dimensions_selection <- df_dimensions_selection
    }else{
      df_dimensions_selection <- df_dimensions_selection %>%
        dplyr::filter(type %in% tipo_sel)
    }
  }else{ ## casos onde tem mais de um argumento (mais de um tipo)
    df_dimensions_selection <- df_dimensions_selection %>%
      dplyr::filter(type %in% tipo_sel)
  }
  
  ## Filtrando ano -----
  ano_sel <- as.character(ano_sel)
  if(length(ano_sel) == 1){ ## casos onde tem um argumento (um tipo ou todos)
    if(ano_sel == "TODOS"){
      df_dimensions_selection <- df_dimensions_selection
    }else{
      df_dimensions_selection <- df_dimensions_selection %>%
        dplyr::filter(as.character(lubridate::year(date)) == ano_sel)
    }
  }else{ ## casos onde tem mais de um argumento (mais de um tipo)
    df_dimensions_selection <- df_dimensions_selection %>%
      dplyr::filter(as.character(lubridate::year(date)) %in% ano_sel)
  }
  ## Filtrando países -----
  length(pais_sel)
  pais_sel <- as.character(pais_sel)
  if(length(pais_sel) == 1){ ## casos onde tem um argumento (um tipo ou todos)
    if(ano_sel == "TODOS"){
      df_dimensions_selection <- df_dimensions_selection
    }else{
      df_dimensions_selection <- df_dimensions_selection %>%
        dplyr::filter(paises == pais_sel)
    }
  }else{ ## casos onde tem mais de um argumento (mais de um tipo)
    df_dimensions_selection <- df_dimensions_selection %>%
      dplyr::filter(paises %in% pais_sel)
  }
  
  return(df_dimensions_selection)
}

## quais campos preciso? id +
## evol_pub_tipo -> date_normal, type
## wordcloud -> title.preferred, abstract.preferred
## paises_pub -> research_org_country_names
## categ_pub -> categories.for_v1.first_level.codes