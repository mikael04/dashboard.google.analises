#' tratar_ano_sel 
#'
#' @description Uma função pra tratar o ano selecionado, senão selecionado (opção default, "Todos os anos" será mapeada como -1)
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_trat_ano  <- function(string_ano, debug){
  if(is.null(string_ano)){
    ano_sel <- as.character("TODOS")
  }else{
    ano_sel <- as.character(string_ano)
  }
  return(ano_sel)
}
