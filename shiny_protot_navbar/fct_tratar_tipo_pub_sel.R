#' tratar_tipo_pub_sel 
#'
#' @description Uma função pra tratar o tipo de publicação selecionado, senão selecionado (opção default, "Todos os tipos" será mapeada como -1)
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

func_trat_tipo_pub <- function (string_tipo_pub, debug){
  if(is.null(string_tipo_pub)){
    tipo_pub <- as.character("TODOS") 
  }else{
    tipo_pub <- as.character(string_tipo_pub)
  }
  return (tipo_pub)
}
