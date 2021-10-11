## Funções para manipular nomes de autores e países

func_first_author <- function (x){
  stringr::str_extract(x, '[^|]+')
}
func_last_author <- function (x){
  sub(".*\\|", "", x)
}
func_first_country <- function (x){
  stringr::str_extract(x, '[^;]+')
}
func_last_country <- function (x){
  sub(".*\\;", "", x)
}
func_trans_names <- function(x){
  stringi::stri_trans_general(x, "Latin-ASCII")
}
func_count_numbers <- function(string, pattern){
  count <- stringr::str_count(string, pattern) + 1
  return(count)
}