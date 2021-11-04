library("shiny")
library("DT")

ui = tagList(
  tags$head(
    tags$style(
      HTML("
          .datatables {
              font-size: 1.5vw;
          }

          @media screen and (min-width: 1024px) {
              .datatables {
                  font-size: 12px;
              }
          }
        ")
      )
    ),
  fluidPage(
    shinyWidgets::actionBttn("ref_perg", label = "Refinar pesquisa",
                             icon = icon("refresh"), style = "jelly"),
    DT::dataTableOutput('tbl')
  )
)
server = function(input, output) {
  
  df_tabela_perg_filt <- fst::read_fst("dados/df_tabela_perg_filt.fst")
  df_tabela_perg_filt <- df_tabela_perg_filt |>
    # dplyr::select(authors_ln, tittle_50char, journals, countries, type)
    dplyr::select(authors_last_name, title_n_char, abstract_50char, journals, countries,
                  type, doi, citations, altmetrics, authors_ln, title, abstract,
                  journal_lists, research_org_country_names) |> 
    dplyr::rename(`Autor(es)` = authors_last_name, `Título` = title_n_char,
                  `Resumo` = abstract_50char, `Revista` = journals,
                  `País` = countries, `Tipo` = type, doi = doi,
                  `Citações` = citations, `Altmetria` = altmetrics,
                  `Autor(es) nome completo` = authors_ln,
                  `Título completo` =  title,`Resumo completo` = abstract,
                  `Revistas completo` = journal_lists,
                  `Países completo` = research_org_country_names)
  output$tbl = DT::renderDataTable(
    df_tabela_perg_filt,extensions="Buttons",
      options = list(autoWidth = T,
                     columnDefs = list(list(visible=FALSE, targets=c(10, 11, 12, 13, 14)),
                                       list(width = '200px', targets = "_all")),
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
}

  
  shinyApp(ui = ui, server = server)
  