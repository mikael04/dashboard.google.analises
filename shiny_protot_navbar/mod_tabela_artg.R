#' tabela_artg UI Function
#'
#' @description Módulo que irá apresentar as tabelas 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import shinyjs
#' @import dplyr
#' @import promises
#' @import future
#'
#' @importFrom shiny NS tagList 
mod_tabela_artg_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    shinycssloaders::withSpinner(DT::DTOutput(ns("table_perg_filter")))
  )
}
    
#' tabela_artg Server Functions
#'
#' @noRd 
mod_tabela_artg_server <- function(id, arvore_no_sel, node_tier, df_buscas, df_buscas_relacao, df_dim_au_co_jo, teste, debug){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    future_on <- F
    ## dict com a relação entre estratégia de busca e pergunta
    #browser()
    if(future_on){
      plan(multisession)
    }
    # browser()
    
    # perg_sel <- "Does pregnancy increase the risk for severe COVID-19?"
    # "Can breast milk transmit SARS-CoV-2 to newborns?"
    # "Can mode of delivery transmit SARS-CoV-2 to neonates?"
    # "What are the precautions for breastfeeding for a SARS-CoV-2 positive mother?"
    # "What are the safest methods for neonatal feeding for a SARS-CoV-2 positive mother?"
    # "Women's Health and Gender" <- QUERIE
    output$table_perg_filter <- DT::renderDataTable(server = TRUE,{
      # if(future_on){
      #   #promises::future_promise({
      # }
        
        # df_dimensions_ij_perguntas_search <- df_dimensions_ij_perguntas %>%
        #   dplyr::filter(!!as.name(col_name$Busca) == '1')
      # df_dimensions_ij_perguntas_search <- data.table::fread("data-raw/df_dimensions_ij_perguntas_search.csv") %>%
      #   dplyr::select(doi, title.preferred, `authors/lastname`, date_normal, subtitles, type, research_org_country_names, doi)
      # df_dimensions_ij_perguntas_search <- data.table::fread("data-raw/df_dimensions_ij_perguntas_search.csv")
      # col_name <- df_perguntas_dict[df_perguntas_dict$Pergunta == arvore_no_sel]$Busca
      if(teste){
        df_dimensions_authors_countries_journal_sample <- data.table::fread("data-raw/nao_mais_usadas/df_dimensions_tabelas_clean_sample.csv") |>
          # dplyr::select(authors_ln, tittle_50char, journals, countries, type)
          dplyr::select(authors_last_name, title_50char, abstract_50char, journals, countries,
                        type, doi, citations, altmetrics, authors_ln, title, abstract,
                        journal_lists, research_org_country_names)
        
      }else{
        # browser()
        df_buscas_relacao_filtered <- df_buscas_relacao |> 
          dplyr::filter(perg == arvore_no_sel)
        
        buscas_tit <-  df_buscas_relacao_filtered$col_name
        buscas_tit_abs <-  df_buscas_relacao_filtered$col_name_plus_abs
        
        df_buscas_filtered <- df_buscas |> 
          dplyr::filter(!!as.name(buscas_tit) == 1) |> 
          dplyr::select(id, doi_busc = doi, !!as.name(buscas_tit))
        
        df_dimensions_authors_countries_journal_sample <- dplyr::inner_join(df_dim_au_co_jo, df_buscas_filtered, by="id") |> 
          dplyr::select(-!!as.name(buscas_tit), -doi_busc)
      }
      
      
      DT::datatable(df_dimensions_authors_countries_journal_sample[,1:14],
                    extensions = "Buttons",
                    options = list(columnDefs = list(list(visible=FALSE, targets=c(10, 11, 12, 13, 14))),
                                   rowCallback = DT::JS(
                                     "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                                     "var full_text_author = aData[10]",
                                     "var full_text_title = aData[11]",
                                     "var full_text_abs = aData[12]",
                                     "var full_text_journals = aData[13]",
                                     "var full_text_countries = aData[14]",
                                     "$('td:eq(1)', nRow).attr('title', full_text_author);",
                                     "$('td:eq(2)', nRow).attr('title', full_text_title);",
                                     "$('td:eq(3)', nRow).attr('title', full_text_abs);",
                                     "$('td:eq(4)', nRow).attr('title', full_text_journals);",
                                     "$('td:eq(5)', nRow).attr('title', full_text_countries);",
                                     "}"
                                     # "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                                     # "var full_text_title = aData[9]",
                                     # "$('td:eq(7)', nRow).attr('title', full_text_title);",
                                     # "}"
                                   ),
                                   dom = 'Bfrtip',
                                   buttons = 
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel', 'pdf'),
                                       text = 'Baixar tabela'
                                     ))
                                   )
      )
        
      })
      if(future_on){
        # %...>% {
        # }
        # 
        # })
      }
  })
}
    
## To be copied in the UI
# mod_tabela_artg_ui("tabela_artg_1")
    
## To be copied in the server
# mod_tabela_artg_server("tabela_artg_1")
