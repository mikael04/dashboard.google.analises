#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(purrr)

df_count_base_plus_categ_filtered <- data.table::fread("data-raw/app/first_plots/df_mod_categ_pub_first_plot.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plotly <- plotly::renderPlotly({
        p <- ggplot2::ggplot(df_count_base_plus_categ_filtered, aes(x = Count, y = reorder(Category, Count), text=map(paste('<b>Número de publicações:</b>', Count, '<br>', '<b>Categoria:</b>', Category), HTML))) +
            geom_bar(fill = "steelblue", stat = "identity") + 
            theme_minimal() + 
            labs(x = "Número de artigos", y = "Categorias")
        # labs(x = i18n$t("Número de artigos"), y = i18n$t("Categorias"), title = i18n$t("Publicações por Categoria (ANZSRC)"))
        # if(debug)
        # print("plot criado")
        plotly::ggplotly(p, tooltip = c("text")) |>
            plotly::config(modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d", "select2d",
                                                      "pan2d", "pan3d",
                                                      "lasso2d", "autoScale2d", "toggleSpikelines", "resetScale2d",
                                                      "hoverClosestCartesian", "hoverCompareCartesian"),
                           displaylogo = FALSE) |> 
            plotly::layout(title = list(text='Publicações por Categoria (ANZSRC)',
                                        xanchor = 'right', yanchor =  'top', pad = list(b = 200 )))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
