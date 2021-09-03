func_first <- function (x, parameter){
  stringr::str_extract(x, paste0('[^', parameter, ']+'))
}
func_last <- function (x, parameter){
  sub(paste0(".*\\", parameter), "", x)
}
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
func_first_journal <- function (x){
  stringr::str_extract(x, '[^;]+')
}
func_last_journal <- function (x){
  sub(".*\\;", "", x)
}
func_trans_names <- function(x){
  stringi::stri_trans_general(x, "Latin-ASCII")
}
func_count_numbers <- function(string, pattern){
  count <- stringr::str_count(string, pattern) + 1
  return(count)
}