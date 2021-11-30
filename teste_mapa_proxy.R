#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(thematic)
library(shinipsum)
library(shinyTree)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(leaflet)
library(rgdal) ## Pacote para ajudar com mapas (spdf)
library(sp) ## Pacote para ajudar com mapas (spdf)


source("shiny_protot_navbar/mod_map_pub.R")
source("shiny_protot_navbar/fct_map_pub.R")
source("shiny_protot_navbar/fct_tratar_ano_sel.R")
source("shiny_protot_navbar/fct_tratar_pais_sel.R")
source("shiny_protot_navbar/fct_tratar_tipo_pub_sel.R")

ui <- shiny::tagList(
  navbarPage(
    
  )
)
server <- function(session, input, output) {
  df_count_base_filtros <- data.table::fread("data-raw/app/first_plots/df_mod_map_pub_first_plot.csv")
  df_count_base_filtros <- df_count_base_filtros |>
    dplyr::filter(country != "NoCountry") |> 
    dplyr::group_by(country) |> 
    dplyr::mutate(count_paises = n()) |> 
    dplyr::distinct(country, .keep_all = T) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(count_paises) |>
    dplyr::select(NAME = country, count = count_paises)
  
  first_plot <- reactiveValues(value = TRUE)
  
  first_plot$value <- FALSE
}

shiny::shinyApp(ui, server)