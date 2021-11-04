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
  if(length(selected)>0){
    paste0(selected[[1]][1])
  }else{
    paste0("Nenhum nó selecionado")
  }
}

func_get_node_tier  <- function(df_perguntas, no_sel, debug){
  un_eixo <- unique(df_perguntas$EIXO)
  un_top <- unique(df_perguntas$TOPICS)
  un_quer <- unique(df_perguntas$QUERIES)
  if(no_sel %in% un_eixo){
    if(debug) print(1)
    # return(1)
  }else{
    if(no_sel %in% un_top){
      if(debug) print(2)
      # return(2)
    }else{
      if(no_sel %in% un_quer){
        if(debug) print(3)
        # return(3)
      }else{
        if(debug) print(4)
        # return(4)
      }
    }
  }
}

func_get_node_hierarchy <- function(df_perguntas, input_tree, debug){
  node_tier <- func_get_node_tier(df_perguntas, input_tree, debug)
  # browser()
  if(node_tier > 3){
    df_perguntas |> 
      dplyr::filter(QUESTIONS == input_tree)
  }else{
    selecionado <- dplyr::if_else(node_tier == 3, "QUERIES",
                                  dplyr::if_else(node_tier == 2, "TOPICS", "EIXO"))
    df_perguntas |> 
      dplyr::filter(!!as.name(selecionado) == input_tree) |> 
      dplyr::distinct(!!as.name(selecionado), .keep_all = T)
  }
}
