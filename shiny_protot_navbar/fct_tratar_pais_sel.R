#' tratar_pais_sel 
#'
#' @description Uma função pra tratar o país selecionado, senão selecionado (opção default, "Todos os países" será mapeada como -1)
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

func_trat_pais <- function (string_paises, debug){
  if(is.null(string_paises)){
    pais_sel <- as.character("TODOS")
  }else{
    pais_sel <- as.character(string_paises)
  }
  return (pais_sel)
}
