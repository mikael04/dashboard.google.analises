library(shiny)
library(DT)

shinyApp(
  ui = fluidPage(
    
    DT::dataTableOutput("table2")
    
  ),
  server = function(input, output) {
    
    output$table2<-DT::renderDataTable({
      responseDataFilter2_home<- mtcars
      DT::datatable(responseDataFilter2_home,options = list(rowCallback = JS(
        "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
        "var full_text = aData[1] + ','+ aData[2]",
        "$('td:eq(1)', nRow).attr('title', full_text);",
        "}")
      ))#, stringAsFactors = FALSe, row.names = NULL)
    },paging=FALSE,searching = FALSE,ordering=FALSE,scrollY = 400,scrollCollapse=TRUE,
    columnDefs = list(list(width = '800%', targets = c(1))),colnames="Name") 
    
  }
)