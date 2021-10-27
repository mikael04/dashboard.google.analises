#' no_sel 
#'
#' @description A fct que recebe a árvore completa, com nó selecionado, organiza e retorna o último nó (nó selecionado)
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

func_ret_no_sel  <- function(input_tree, debug){
  tree <- input_tree
  req(input_tree)
  selected <- get_selected(tree, format = "classid")
  if(debug){
    print(paste0("selected = ", selected))
  }
  # browser()
  if(length(selected)>0){
    paste0(selected[[1]][1])
  }else{
    paste0("Nenhum nó selecionado")
  }
}